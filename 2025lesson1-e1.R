#加载必要的包
install.packages(c("StanHeaders","rstan"),type="source")
install.packages(c("pacman","tidyverse","ggplot2","dplyr","car","ggpubr",
                   "BayesFactor","bayestestR","gridExtra","TOSTER"))
library(ggplot2)    
library(dplyr)      
library(car)        
library(ggpubr)     
library(BayesFactor)
library(StanHeaders)
library(rstan)
library(bayestestR)
library(gridExtra)
library(tidyverse)
library(TOSTER)


#导入数据
SMS_data <- read.csv('/Users/liumingyu/Desktop/贝叶斯2025/data/SMS_Well_being.csv')
#选择需要的列
SMS_data <- SMS_data %>% select(uID, variable, factor, Country)
#查看数据
print(head(SMS_data, 2))
#高低分组
SMS_low <- SMS_data %>%
  filter(factor == "Low") %>%
  pull(variable)
SMS_high <- SMS_data %>%
  filter(factor == "High") %>%
  pull(variable)


##绘制小提琴图
#！！原python文件中设置了画图函数，最后的小提琴图绘图数据是随机生成的，并不是案例数据
#！！以下使用的是案例数据
ggplot(data = SMS_data,aes(factor,variable))+
  geom_violin(aes(fill=factor))+
  geom_boxplot(width=0.2,outlier.shape = NA)+
  guides(col="none")+
  theme(
    text = element_text(family = "Helvetica", size = 12), # 字体为Helvetica，字号为12
    axis.title = element_text(size = 12), # 坐标轴标题大小
    axis.text = element_text(size = 12), # 坐标轴刻度大小
    axis.line = element_line(linewidth = 0.2, color = "black"), # 坐标轴线条
    axis.ticks = element_line(linewidth = 0.2, color = "black"), # 坐标轴刻度线
    panel.background = element_blank(), # 去除背景
    panel.grid.major = element_line(), # 主网格线
    panel.grid.minor = element_blank(), # 次网格线
    plot.title = element_text(size = 14, hjust = 0.5), # 标题居中
    plot.margin = margin(10, 10, 10, 10) # 图形边距
  )



##传统T检验
#1.方差齐性检验（输出p值）
levene_res <- car::leveneTest(variable ~ factor,
                data = SMS_data)
levene_p <- levene_res$`Pr(>F)`[1]
cat("p=", levene_p, "\n")
#2.独立样本t检验
ttest_res <- t.test(variable ~ factor, data = SMS_data, var.equal = TRUE)
#3.各组描述性统计
mean_low <- mean(SMS_low)     #均值
mean_high <- mean(SMS_high)
sd_high <- sd(SMS_high)       #标准差
sd_low <- sd(SMS_low)
#4.输出结果
cat("t=", round(ttest_res$statistic, 2), ", p=", round(ttest_res$p.value, 3), "\n")
cat(sprintf("Low Social Status: %.3f ± %.2f；", mean_low, sd_low),
    sprintf("High Social Status: %.3f ± %.2f\n", mean_high, sd_high))


##双单侧检验（TOST）
#！！这里的tsum_TOST函数基于汇总统计数据而非原始数据进行检验
#！！跑出来的p值=2.87e-46与python代码中的p值（2.265256e-27）不一致，但结果都是显著的
n_high <- nrow(SMS_data %>% filter(factor == 'High'))
n_low <- nrow(SMS_data %>% filter(factor == 'Low'))
tost_res <- tsum_TOST(
  m1=mean_high,sd1=sd_high,n1=n_high,
  m2=mean_low,sd2=sd_low,n2=n_low,
  eqb = 0.2
  )
print(tost_res)

##贝叶斯因子（r值 = 0.707）
BF_sms <- ttestBF(x = SMS_low, y = SMS_high, r = 0.707)
print(BF_sms)

##敏感性分析
#Cauchy (0, 1)
BF_sms_1 <- ttestBF(x = SMS_low, y = SMS_high, r = 1)
print(BF_sms_1)

##通过Rstan建立基于贝叶斯的线性模型
#1.准备数据
x <- as.integer(factor(SMS_data$factor)) - 1  # 将 'High' 和 'Low' 转换为 0 和 1 对应的整数

#2.Stan 模型
stan_model_code <- "
data {
  int<lower=0> N;               
  vector[N] y;                  
  vector[N] x;                  
}

parameters {
  real beta0;                   
  real beta1;                  
  real<lower=0> sigma;          
}

model {
  beta0 ~ normal(0, 5);        
  beta1 ~ normal(0, 0.707); 
  sigma ~ cauchy(0, 2);        

  y ~ normal(beta0 + beta1 * x, sigma);
}
"


#先验抽样模型（stan中不能直接查看先验抽样，因此重新定义一个不含似然函数的模型用以抽样）
stan_model_prior <- "
data {
  int<lower=0> N;               
  vector[N] y;                  
  vector[N] x;                  
}

parameters {
  real beta0;                   
  real beta1;                  
  real<lower=0> sigma;          
}

model {
  beta0 ~ normal(0, 5);        
  beta1 ~ normal(0, 0.707); 
  sigma ~ cauchy(0, 2);
  // 不定义似然函数
}
"

#3.数据准备
stan_data <- list(N = nrow(SMS_data), x = x, y = SMS_data$variable)

#4.进行先验采样
prior_checks <- stan(model_code = stan_model_prior, 
            data = stan_data, 
            iter = 5000, 
            warmup = 0,
            chains = 1,
            seed = 202409)
prior_samples <- rstan::extract(prior_checks)

#5.进行后验采样
fit <- stan(model_code = stan_model_code, 
            data = stan_data, 
            iter = 2000, 
            warmup = 1000, 
            chains = 4, 
            control = list(adapt_delta = 0.9), 
            seed = 202409)
idata <- rstan::extract(fit)

#轨迹图
traceplot(fit)


##抽样分布可视化
#将样本转换成数据框
porior_df <- data.frame(beta1 = prior_samples$beta1)
posterior_df <- data.frame(beta1 = idata$beta1)
#先验分布图
prior_plot <- ggplot(porior_df, aes(x = beta1)) +
  geom_density(fill = "lightblue", alpha = 0.5) +
  labs(title = "Prior", x = "beta1", y = "Density") +
  theme(
    text = element_text(family = "Helvetica", size = 12), # 字体为Helvetica，字号为12
    axis.title = element_text(size = 12), # 坐标轴标题大小
    axis.text = element_text(size = 12), # 坐标轴刻度大小
    axis.line = element_line(linewidth = 0.2, color = "black"), # 坐标轴线条
    axis.ticks = element_line(linewidth = 0.2, color = "black"), # 坐标轴刻度线
    panel.background = element_blank(), # 去除背景
    panel.grid.major = element_line(), # 主网格线
    panel.grid.minor = element_blank(), # 次网格线
    plot.title = element_text(size = 14, hjust = 0.5), # 标题居中
    plot.margin = margin(10, 10, 10, 10) # 图形边距
  )
#后验分布图
posterior_plot <- ggplot(posterior_df, aes(x = beta1)) +
  geom_density(fill = "orange", alpha = 0.5) +
  labs(title = "Posterior", x = "beta1", y = "Density") +
  theme(
    text = element_text(family = "Helvetica", size = 12), # 字体为Helvetica，字号为12
    axis.title = element_text(size = 12), # 坐标轴标题大小
    axis.text = element_text(size = 12), # 坐标轴刻度大小
    axis.line = element_line(linewidth = 0.2, color = "black"), # 坐标轴线条
    axis.ticks = element_line(linewidth = 0.2, color = "black"), # 坐标轴刻度线
    panel.background = element_blank(), # 去除背景
    panel.grid.major = element_line(), # 主网格线
    panel.grid.minor = element_blank(), # 次网格线
    plot.title = element_text(size = 14, hjust = 0.5), # 标题居中
    plot.margin = margin(10, 10, 10, 10) # 图形边距
  )
#并排显示
grid.arrange(prior_plot, posterior_plot, nrow = 1, ncol = 2)



##使用ggplot进行可视化
#获取后验分布的密度
posterior_density <- density(posterior_df$beta1)
prior_density <- density(prior_df$beta1)

#将密度结果转为数据框
posterior_density_df <- data.frame(x = posterior_density$x, Density = posterior_density$y)
prior_density_df <- data.frame(x = prior_density$x, Density = prior_density$y)

#绘图
p <- ggplot() +
  # 绘制后验分布曲线
  geom_line(data = posterior_density_df, aes(x = x, y = Density), color = "orange") +
  # 绘制先验分布曲线
  geom_line(data = prior_density_df, aes(x = x, y = Density), color = "blue") +
  # 设置x轴范围
  xlim(-0.5, 0.5) +
  # 添加标签
  labs(x = "beta1 \n(represents the difference \nbetween two groups)") +
  # 添加垂直线
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  # 设置背景颜色
  theme(
    text = element_text(family = "Helvetica", size = 12), # 字体为Helvetica，字号为12
    axis.title = element_text(size = 12), # 坐标轴标题大小
    axis.text = element_text(size = 12), # 坐标轴刻度大小
    axis.line = element_line(linewidth = 0.2, color = "black"), # 坐标轴线条
    axis.ticks = element_line(linewidth = 0.2, color = "black"), # 坐标轴刻度线
    panel.background = element_blank(), # 去除背景
    panel.grid.major = element_line(), # 主网格线
    panel.grid.minor = element_blank(), # 次网格线
    plot.title = element_text(size = 14, hjust = 0.5), # 标题居中
    plot.margin = margin(10, 10, 10, 10) # 图形边距
  )
# 显示图形
print(p)

