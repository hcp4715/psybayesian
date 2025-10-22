# 安装和加载包
options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
if (!requireNamespace('pacman', quietly = TRUE)) {
  install.packages('pacman')
}
pacman::p_load("tidyverse","ggplot2", "dplyr","gridExtra","papaja", "patchwork","bayesplot","rstan","bayesrules")
options(warn = -1)  # 抑制警告

# 准备数据
stan_data <- list(N = 10,Y = 9)
# 定义模型
stan_model_code <- "
data {
  int<lower=0> N;       
  int<lower=0, upper=N> Y; 
}
parameters {
  real<lower=0, upper=1> pi;
}
model {
  pi ~ beta(2, 2);
  Y ~ binomial(N, pi);
}
"

# 进行采样
fit <- stan(
  model_code = stan_model_code,      # 定义的模型或模型文件路径
  data = stan_data,                  # 输入数据
  chains = 4,                        # 马尔可夫链数量
  iter = 5000,                       # 总迭代次数（每个链）
  warmup = 0,                        # 热身迭代次数（不保存）
  seed = 84735
)

trace <- bayesplot::mcmc_trace(fit, pars = "pi") +  
  papaja::theme_apa()
trace

options(repr.plot.width=8, repr.plot.height=16) 
auto_r <- bayesplot::mcmc_acf(fit, pars = "pi") + 
  papaja::theme_apa()

auto_r

bayesplot::rhat(fit, pars = "pi")
bayesplot::neff_ratio(fit, pars = "pi")

# 进行采样
thinned_fit <- rstan::stan(
  model_code = stan_model_code,      # 定义的模型或模型文件路径
  data = stan_data,                  # 输入数据
  chains = 4,                        # 马尔可夫链数量
  iter = 5000*2,                       # 总迭代次数（每个链）
  thin = 10,                         # 缩减马尔可夫链
  seed = 84735
)

thinned_trace <- bayesplot::mcmc_trace(thinned_fit, pars = "pi") +  
  papaja::theme_apa()
thinned_trace


p1 <- bayesplot::mcmc_acf_bar(fit, pars = "pi", lags = 10) + 
  papaja::theme_apa() + 
  ggplot2::ggtitle('Full Trace')
p2 <- bayesplot::mcmc_acf_bar(thinned_fit, pars = "pi", lags = 10) + 
  papaja::theme_apa() +
  ggplot2::ggtitle('Thinned Trace')
p1 + p2


# 使用bayesrules包中的 plot_beta_binomial（）函数可以绘制先验分布、似然分布和后验分布的 PDF 图
fig <- bayesrules::plot_beta_binomial(alpha = 2, beta = 2, y = 9, n = 10)  +
  papaja::theme_apa() +
  scale_y_continuous(expand = c(0,0)) + 
  theme(legend.text = element_text(size = 16))
print(fig)

options(repr.plot.width=15, repr.plot.height=5) 
# 创建 Beta 分布的数据
x_values <- seq(0, 1, length.out = 100)
y_values1 <- dbeta(x_values, shape1 = 11, shape2 = 3)
y_values2 <- dbeta(x_values, shape1 = 101, shape2 = 21)

# 计算 95% 可信区间（CI）
lower_bound1_95 <- qbeta(0.025, shape1 = 11, shape2 = 3)  
upper_bound1_95 <- qbeta(0.975, shape1 = 11, shape2 = 3)  
lower_bound2_95 <- qbeta(0.025, shape1 = 101, shape2 = 21)  
upper_bound2_95 <- qbeta(0.975, shape1 = 101, shape2 = 21)  

# 绘制 Beta 分布的 PDF
p1 <- ggplot2::ggplot(data.frame(x = x_values, y = y_values1),aes(x = x, y = y)) +
  geom_line(col = 'black', size = 1) +
  geom_area(data = subset(data.frame(x = x_values, y = y_values1),
                          x >= lower_bound1_95 & x <= upper_bound1_95), 
            aes(x = x, y = y), fill = "lightblue", alpha = 0.5) +
  labs( x = " ", y = NULL, title = "Continuous Prior\n(Beta(alpha = 11, beta = 3))")  +
  scale_y_continuous(expand = c(0,0),limit = c(0,12)) + 
  papaja::theme_apa()

p2 <- ggplot2::ggplot(data.frame(x = x_values, y = y_values2),aes(x = x, y = y)) +
  geom_line(col = 'black', size = 0.7) +
  geom_area(data = subset(data.frame(x = x_values, y = y_values2),
                          x >= lower_bound2_95 & x <= upper_bound2_95), 
            aes(x = x, y = y), fill = "lightblue", alpha = 0.5) +
  labs( x = " ", y = NULL, title = "Continuous Prior\n(Beta(alpha = 101, beta = 21))")  +
  scale_y_continuous(expand = c(0,0),limit = c(0,12)) + 
  papaja::theme_apa()

#并排显示
p1 + p2

# 创建 Beta 分布的数据
x_values <- seq(0, 1, length.out = 100)
y_values <- dbeta(x_values, shape1 = 101, shape2 = 21)

# 计算三种可信区间（CI）
lower_bound_95 <- qbeta(0.025, shape1 = 101, shape2 = 21)  
upper_bound_95 <- qbeta(0.975, shape1 = 101, shape2 = 21)  
lower_bound_50 <- qbeta(0.25, shape1 = 101, shape2 = 21)  
upper_bound_50 <- qbeta(0.75, shape1 = 101, shape2 = 21)  
lower_bound_99 <- qbeta(0.005, shape1 = 101, shape2 = 21)  
upper_bound_99 <- qbeta(0.995, shape1 = 101, shape2 = 21)  

# 绘制 Beta 分布的 PDF
p1 <- ggplot2::ggplot(data.frame(x = x_values, y = y_values),aes(x = x, y = y)) +
  geom_line(col = 'black', size = 1) +
  geom_area(data = subset(data.frame(x = x_values, y = y_values),
                          x >= lower_bound_50 & x <= upper_bound_50), 
            aes(x = x, y = y), fill = "#bdd7e7", alpha = 0.5) +
  labs( x = " ", y = NULL, title = "50% CI for Beta(101, 21)")  +
  scale_y_continuous(expand = c(0,0),limit = c(0,12)) + 
  xlim(0.5,1) +
  papaja::theme_apa()


p2 <- ggplot2::ggplot(data.frame(x = x_values, y = y_values),aes(x = x, y = y)) +
  geom_line(col = 'black', size = 0.7) +
  geom_area(data = subset(data.frame(x = x_values, y = y_values),
                          x >= lower_bound_95 & x <= upper_bound_95), 
            aes(x = x, y = y), fill = "#6baed6", alpha = 0.5) +
  labs( x = " ", y = NULL, title = "95% CI for Beta(101, 21)")  +
  scale_y_continuous(expand = c(0,0),limit = c(0,12)) + 
  xlim(0.5,1) +
  papaja::theme_apa()

p3 <- ggplot2::ggplot(data.frame(x = x_values, y = y_values),aes(x = x, y = y)) +
  geom_line(col = 'black', size = 0.7) +
  geom_area(data = subset(data.frame(x = x_values, y = y_values),
                          x >= lower_bound_99 & x <= upper_bound_99), 
            aes(x = x, y = y), fill = "#3182bd", alpha = 0.5) +
  labs( x = " ", y = NULL, title = "99% CI for Beta(101, 21)")  +
  scale_y_continuous(expand = c(0,0)) + 
  xlim(0.5,1) +
  papaja::theme_apa()

#并排显示
options(repr.plot.width=18, repr.plot.height=5) 
p1 + p2 + p3

#可以使用bayestestR包中的hdi函数计算HDI的上界和下界
library(bayestestR)
# 创建 Beta 分布的数据
x_values <- seq(0, 1, length.out = 10000)
y_values <- dbeta(x_values, shape1 = 101, shape2 = 21)

# 创建数据框
beta_df <- data.frame(x = x_values, y = y_values)

# 计算 HDI
hdi_result <- hdi(rbeta(100000, shape1 = 101, shape2 = 21), pd = 0.95)

# 提取 HDI 的下界和上界
hdi_lower <- hdi_result$CI_low
hdi_upper <- hdi_result$CI_high


# 绘图
p <- ggplot(beta_df, aes(x = x, y = y)) +
  geom_line(color = 'black', size = 1) +  # 绘制 PDF
  geom_area(data = beta_df[(beta_df$x >= hdi_lower) & (beta_df$x <= hdi_upper), ],
            aes(x = x, y = y), fill = 'lightblue', alpha = 0.5) +  # 填充 HDI 区域
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +  # pi=0.5
  labs(x = "x", y = "Density", title = "Beta Distribution PDF and 95% HDI") +
  xlim(0.4, 1) +
  papaja::theme_apa()

# 输出 HDI 结果
cat("95% HDI 下界:", hdi_lower, "\n")
cat("95% HDI 上界:", hdi_upper, "\n")
# 显示图形
options(repr.plot.width=8, repr.plot.height=6) 
print(p)

pp <- p + 
  geom_segment(aes(x = 0.4, xend = 0.6, y = 0, yend = 0), color = "green", size = 2)

pp

