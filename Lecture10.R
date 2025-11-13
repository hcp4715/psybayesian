# 安装和加载包
options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
if (!requireNamespace('pacman', quietly = TRUE)) {
  install.packages('pacman')
}
pacman::p_load("tidyverse","ggplot2", "dplyr","gridExtra","papaja", "patchwork","bayesplot","rstan","bridgesampling") # "bayesrules"
options(warn = -1)  # 抑制警告

# 导入数据
df_raw <- tryCatch({
  read.csv('/home/mw/input/bayes3797/Kolvoort_2020_HBM_Exp1_Clean.csv')
}, error = function(e) {
  read.csv('/Users/liumingyu/Desktop/2/PyBayesian/data/Kolvoort_2020_HBM_Exp1_Clean.csv')
})
# 显示数据前几行
head(df_raw) 
#——————————
# 数据分组和计算均值
df <- df_raw %>%
  dplyr::group_by(Subject, Label, Matching) %>%
  summarize(RT_sec = mean(RT_sec)) %>%
  ungroup()

# 将 Label 列的数字编码转为文字标签
df$Label <- recode(df$Label, `1` = 'Self', `2` = 'Friend', `3` = 'Stranger')

# 替换 Matching 列的值为小写标签
df$Matching <- recode(df$Matching, `Matching` = 'matching', `Nonmatching` = 'nonmatching')

# 设置索引
df <- df %>%
  mutate(index = row_number()) %>%
  column_to_rownames(var = "index")  # 设置索引为新创建的命名行

head(df)

cat("Label列共有", toString(unique(df$Label)), "类\n")

#将 Label 列转换为有序的分类变量
df <- df %>%
  mutate(Label = factor(Label, levels = c('Self', 'Friend', 'Stranger'), ordered = TRUE))

###模型1
# 将分类变量转换为哑变量
X1 <- as.integer(df$Label == 'Friend')
X2 <- as.integer(df$Label == 'Stranger')
#建立模型
#建立模型
model_code1 <- "
data {
  int<lower=0> N;                // 数据点数量
  vector[N] y;              // 观测数据
  vector[N] X1;    // Friend 
  vector[N] X2;    // Stranger 
}

parameters {
  real beta_0;                   // 截距
  real beta_1;                   // Friend 的斜率
  real beta_2;                   // Stranger 的斜率
  real<lower=0> sigma;           // 误差标准差
}

model {
  beta_0 ~ normal(5, 2);         // 截距的先验分布
  beta_1 ~ normal(0, 1);         // Friend 的斜率的先验分布
  beta_2 ~ normal(0, 1);         // Stranger 的斜率的先验分布
  sigma ~ exponential(0.3);      // 误差标准差的先验分布
    // 似然函数
 y ~ normal(beta_0 + beta_1 * X1 + beta_2 * X2, sigma);
}

//Stan可以使用随机数生成器在每次迭代中为每个数据点生成预测值。
//Generated Quantities 模块可用于获取我们想要的关于后验的任何其他信息，或对新数据进行预测。

generated quantities {
 real y_rep[N];

 for (n in 1:N) {
 y_rep[n] = normal_rng(beta_0 + beta_1 * X1[n] + beta_2 * X2[n], sigma);
 }
}
"

#进行后验采样
# 数据准备
data_list <- list(
  N = nrow(df),
  y = df$RT_sec,
  X1 = X1,
  X2 = X2
)

# 采样
model1_fit <- stan(model_code = model_code1, data = data_list, 
                   iter = 5000, 
                   chains = 4, 
                   warmup = 1000, 
                   thin = 1, 
                   seed = 84735)

#  MCMC诊断
neff_ratio1 <- as.data.frame(bayesplot::neff_ratio(model1_fit))
head(neff_ratio1,4)

model_summary1 <- summary(model1_fit)
round(head(model_summary1$summary, 4),3)

#ROPE+HDI
library(bayestestR)
# 提取后验样本
trace <- rstan::extract(model1_fit)
trace_beta1 <- as.data.frame(trace$beta_1)
trace_beta2 <- as.data.frame(trace$beta_2)

# 计算HDI
beta1_hdi95 <- bayestestR::hdi(trace_beta1)
beta2_hdi95 <- bayestestR::hdi(trace_beta2)

# 计算ROPE
rope1 <- bayestestR::rope(trace_beta1,range = c(-0.05, 0.05))
rope2 <- bayestestR::rope(trace_beta2,range = c(-0.05, 0.05))

beta1_plot <- plot(rope1, rope_color = "grey70") + 
  ggplot2::annotate("text", # 添加标签显示 Rope 值
                    x = 0, y = 0.5,
                    label = paste0("ROPE: ", round(rope1$ROPE_Percentage * 100, 1), "%"), 
                    color = "black", size = 6) +
  ggplot2::annotate("text",  # 添加标签显示 HDI 区间
                    x = mean(trace$beta_1), y = 0.1,
                    label = paste0("95% HDI:\n[", round(beta1_hdi95$CI_low, 3), ", ", round(beta1_hdi95$CI_high, 3), "]"),
                    color = "black", size = 6) +
  papaja::theme_apa() + ggplot2::scale_y_continuous(expand = c(0, 0)) +
  ggplot2::theme(legend.position = c(0.85, 0.75))  # 调整图例位置

beta2_plot <- plot(rope2, rope_color = "grey70") + 
  ggplot2::annotate("text", # 添加标签显示 Rope 值
                    x = 0, y = 0.5,
                    label = paste0("ROPE: ", round(rope2$ROPE_Percentage * 100, 1), "%"), 
                    color = "black", size = 6) +
  ggplot2::annotate("text",  # 添加标签显示 HDI 区间
                    x = mean(trace$beta_2), y = 0.1,
                    label = paste0("95% HDI:\n[", round(beta2_hdi95$CI_low, 3), ", ", round(beta2_hdi95$CI_high, 3), "]"),
                    color = "black", size = 6) +
  papaja::theme_apa() + ggplot2::scale_y_continuous(expand = c(0, 0)) +
  ggplot2::theme(legend.position = c(0.85, 0.75))  # 调整图例位置

# 显示图形
options(repr.plot.width=14, repr.plot.height=6) 
beta1_plot + beta2_plot

# 贝叶斯因子
# 定义先验模型
# 定义先验模型
stan_prior_code <- "
data {
  int<lower=0> N;                // 数据点数量
  vector[N] y;              // 观测数据
  vector[N] X1;    // Friend 指示变量
  vector[N] X2;    // Stranger 指示变量
}

parameters {
  real beta_0;                   // 截距
  real beta_1;                   // Friend 的斜率
  real beta_2;                   // Stranger 的斜率
  real<lower=0> sigma;           // 误差标准差
}

model {
  beta_0 ~ normal(5, 2);         // 截距的先验分布
  beta_1 ~ normal(0, 1);         // Friend 的斜率的先验分布
  beta_2 ~ normal(0, 1);         // Stranger 的斜率的先验分布
  sigma ~ exponential(0.3);      // 误差标准差的先验分布
  
  // 不定义似然函数
}
"

# 从先验采样
model1_pri <- stan(model_code = stan_prior_code, 
                   data = data_list, 
                   iter = 6000, 
                   warmup = 1000,
                   chains = 4,
                   seed = 84735)
# 提取先验样本
prior_samples1 <- extract(model1_pri)

# 提取后验样本
post_samples1 <- extract(model1_fit)

# 可以使用bayestestR包中的bayesfactor_parameters函数计算贝叶斯因子（Savage-Dickey）
beta1_BF <- bayestestR::bayesfactor_parameters(post_samples1$beta_1, prior_samples1$beta_1, direction = "two-sided", null = 0)
beta2_BF <- bayestestR::bayesfactor_parameters(post_samples1$beta_2, prior_samples1$beta_2, direction = "two-sided", null = 0)

print(beta1_BF)
print(beta2_BF)

# 配合see包可以对贝叶斯因子可视化
library(see)
plot_BF1 <- plot(beta1_BF)+
  see::scale_fill_material_d(palette = "beta_1") +
  see::scale_color_material_d(palette = "beta_1") +
  xlim(-0.3,0.3) +
  papaja::theme_apa() +
  ggplot2::theme(legend.position = c(0.85, 0.75))  # 调整图例位置

plot_BF2 <- plot(beta2_BF)+
  see::scale_fill_material_d(palette = "beta_2") +
  see::scale_color_material_d(palette = "beta_2") +
  xlim(-0.3,0.3) +
  papaja::theme_apa() +
  ggplot2::theme(legend.position = c(0.85, 0.75))  # 调整图例位置

options(repr.plot.width=16, repr.plot.height=6) 
plot_BF1 + plot_BF2

#  后验预测
y_rep <- as.matrix(model1_fit, pars = "y_rep")

pp_check <- bayesplot::ppc_dens_overlay(data_list$y, y_rep[1:200, ]) + 
  papaja::theme_apa()

options(repr.plot.width=10, repr.plot.height=6) 
pp_check


# 箱图部分
# 提取后验样本
post_samples1 <- extract(model1_fit)
# 先将列表转换为矩阵
temp_matrix <- do.call(rbind, post_samples1)

# 转置矩阵
posterior_samples1 <- t(temp_matrix)
column1_means <- apply(posterior_samples1, 2, mean, na.rm = TRUE)

# 计算预测值
df$pred_mean1 <- column1_means[1] + column1_means[2] * data_list$X1 + column1_means[3] * data_list$X2

# 创建箱线图风格的后验预测图
real_boxp <- ggplot(df, aes(x = Label, y = RT_sec, fill = Matching)) +
  ggplot2::geom_boxplot() +
  ggplot2:: scale_fill_manual(
    values = c("matching" = "#66c2a5", "nonmatching" = "#fc8d62"),
    labels = c("matching" = "Matching", "nonmatching" = "Non-matching"))  +
  papaja::theme_apa()

p1 <- real_boxp +
  ggplot2::geom_point(aes(y = pred_mean1), 
                      color = "red", 
                      size = 3, 
                      shape = 16,
                      position = position_dodge(width = 0.8))

p1


####模型二
# 准备数据并转换分类变量为哑变量
Matching <- as.integer(df$Matching == 'matching')
# 定义Stan模型
model_code2 <- "
data {
  int<lower=0> N;             // 样本数量
  vector[N] y;           // 响应变量
  vector[N] X1;               // 哑变量1（Friend）
  vector[N] X2;               // 哑变量2（Stranger）
  vector[N] Matching;         // 哑变量（Matching条件）
}

parameters {
  real beta_0;                // 截距
  real beta_1;                // Friend的主效应
  real beta_2;                // Stranger的主效应
  real beta_3;                // Matching的主效应
  real<lower=0> sigma;        // 误差项的标准差
}

model {
  // 先验分布
  beta_0 ~ normal(5, 2);
  beta_1 ~ normal(0, 1);
  beta_2 ~ normal(0, 1);
  beta_3 ~ normal(0, 1);
  sigma ~ exponential(0.3);
  
  //似然函数
  y ~ normal(beta_0 + beta_1 * X1 + beta_2 * X2 + beta_3 * Matching, sigma);
}
generated quantities {
 real y_rep[N];

 for (n in 1:N) {
 y_rep[n] = normal_rng(beta_0 + beta_1 * X1[n] + beta_2 * X2[n], sigma);
 }
}
"

# 准备数据列表
data_list2 <- list(
  N = nrow(df),
  y = df$RT_sec,
  X1 = X1,
  X2 = X2,
  Matching = Matching
)

# 采样
model2_fit <- stan(model_code = model_code2, data = data_list2, 
                   iter = 5000, chains = 4, 
                   warmup = 1000, thin = 1, 
                   seed = 84735)

# MCMC诊断
neff_ratio2 <- as.data.frame(bayesplot::neff_ratio(model2_fit))
head(neff_ratio2,5)

model_summary <- summary(model2_fit)
round(head(model_summary$summary, 5),3)

# HDI和ROPE
# 定义 ROPE 区间，根据研究的需要指定实际等效范围
rope_interval <- c(-0.05, 0.05)

# 提取后验样本
trace2 <- rstan::extract(model2_fit)


trace2_beta1 <- as.data.frame(trace2$beta_1)
trace2_beta2 <- as.data.frame(trace2$beta_2)
trace2_beta3 <- as.data.frame(trace2$beta_3)

# 计算HDI
beta1_hdi95 <- bayestestR::hdi(trace2_beta1)
beta2_hdi95 <- bayestestR::hdi(trace2_beta2)
beta3_hdi95 <- bayestestR::hdi(trace2_beta3)

# 计算ROPE
rope1 <- bayestestR::rope(trace2_beta1,range = c(-0.05, 0.05))
rope2 <- bayestestR::rope(trace2_beta2,range = c(-0.05, 0.05))
rope3 <- bayestestR::rope(trace2_beta3,range = c(-0.05, 0.05))

beta1_plot <- plot(rope1, rope_color = "grey70") + 
  ggplot2::annotate("text", # 添加标签显示 Rope 值
                    x = 0, y = 0.5,
                    label = paste0("ROPE: ", round(rope1$ROPE_Percentage * 100, 1), "%"), 
                    color = "black", size = 6) +
  ggplot2::annotate("text",  # 添加标签显示 HDI 区间
                    x = mean(trace2$beta_1), y = 0.1,
                    label = paste0("95% HDI:\n[", round(beta1_hdi95$CI_low, 3), ", ", round(beta1_hdi95$CI_high, 3), "]"),
                    color = "black", size = 6) +
  papaja::theme_apa() + ggplot2::scale_y_continuous(expand = c(0, 0)) +
  ggplot2::theme(legend.position = c(0.85, 0.75))  # 调整图例位置

beta2_plot <- plot(rope2, rope_color = "grey70") + 
  ggplot2::annotate("text", # 添加标签显示 Rope 值
                    x = 0, y = 0.5,
                    label = paste0("ROPE: ", round(rope2$ROPE_Percentage * 100, 1), "%"), 
                    color = "black", size = 6) +
  ggplot2::annotate("text",  # 添加标签显示 HDI 区间
                    x = mean(trace2$beta_2), y = 0.1,
                    label = paste0("95% HDI:\n[", round(beta2_hdi95$CI_low, 3), ", ", round(beta2_hdi95$CI_high, 3), "]"),
                    color = "black", size = 6) +
  papaja::theme_apa() + ggplot2::scale_y_continuous(expand = c(0, 0)) +
  ggplot2::theme(legend.position = c(0.85, 0.75))  # 调整图例位置

beta3_plot <- plot(rope3, rope_color = "grey70") + 
  ggplot2::annotate("text", # 添加标签显示 Rope 值
                    x = 0, y = 0.5,
                    label = paste0("ROPE: ", round(rope3$ROPE_Percentage * 100, 1), "%"), 
                    color = "black", size = 6) +
  ggplot2::annotate("text",  # 添加标签显示 HDI 区间
                    x = mean(trace2$beta_3), y = 0.1,
                    label = paste0("95% HDI:\n[", round(beta3_hdi95$CI_low, 3), ", ", round(beta3_hdi95$CI_high, 3), "]"),
                    color = "black", size = 6) +
  papaja::theme_apa() + ggplot2::scale_y_continuous(expand = c(0, 0)) +
  ggplot2::theme(legend.position = c(0.85, 0.75))  # 调整图例位置
# 显示图形
options(repr.plot.width=16, repr.plot.height=6) 
beta1_plot + beta2_plot + beta3_plot

# 定义先验模型
stan_prior_code2 <- "
data {
  int<lower=0> N;             // 样本数量
  vector[N] y;           // 响应变量
  vector[N] X1;               // 哑变量1（Friend）
  vector[N] X2;               // 哑变量2（Stranger）
  vector[N] Matching;         // 哑变量（Matching条件）
}

parameters {
  real beta_0;                // 截距
  real beta_1;                // Friend的主效应
  real beta_2;                // Stranger的主效应
  real beta_3;                // Matching的主效应
  real<lower=0> sigma;        // 误差项的标准差
}

model {
  // 先验分布
  beta_0 ~ normal(5, 2);
  beta_1 ~ normal(0, 1);
  beta_2 ~ normal(0, 1);
  beta_3 ~ normal(0, 1);
  sigma ~ exponential(0.3);
  
  // 不定义似然函数
}
"

# 从先验采样
model2_pri <- stan(model_code = stan_prior_code2, 
                   data = data_list2, 
                   iter = 5000, 
                   warmup = 1000,
                   chains = 4,
                   seed = 84735)

# 提取先验样本
prior_samples2 <- extract(model2_pri)

# 提取后验样本
post_samples2 <- extract(model2_fit)

# 可以使用bayestestR包中的bayesfactor_parameters函数计算贝叶斯因子（Savage-Dickey）
beta1_BF <- bayestestR::bayesfactor_parameters(post_samples2$beta_1, prior_samples2$beta_1, direction = "two-sided", null = 0)
beta2_BF <- bayestestR::bayesfactor_parameters(post_samples2$beta_2, prior_samples2$beta_2, direction = "two-sided", null = 0)
beta3_BF <- bayestestR::bayesfactor_parameters(post_samples2$beta_3, prior_samples2$beta_3, direction = "two-sided", null = 0)

print(beta1_BF)
print(beta2_BF)
print(beta3_BF)

# 配合see包可以对贝叶斯因子可视化
plot_BF1 <- plot(beta1_BF)+
  see::scale_fill_material_d(palette = "beta_1") +
  see::scale_color_material_d(palette = "beta_1") +
  xlim(-0.3,0.3) +
  papaja::theme_apa() +
  ggplot2::theme(legend.position = c(0.85, 0.75))  # 调整图例位置

plot_BF2 <- plot(beta2_BF)+
  see::scale_fill_material_d(palette = "beta_2") +
  see::scale_color_material_d(palette = "beta_2") +
  xlim(-0.3,0.3) +
  papaja::theme_apa() +
  ggplot2::theme(legend.position = c(0.85, 0.75))  # 调整图例位置

plot_BF3 <- plot(beta3_BF)+
  see::scale_fill_material_d(palette = "beta_3") +
  see::scale_color_material_d(palette = "beta_3") +
  xlim(-0.3,0.3) +
  papaja::theme_apa() +
  ggplot2::theme(legend.position = c(0.85, 0.75))  # 调整图例位置

options(repr.plot.width=20, repr.plot.height=6) 
plot_BF1 + plot_BF2 + plot_BF3

# 后验预测
y_rep2 <- as.matrix(model2_fit, pars = "y_rep")

pp_check2 <- bayesplot::ppc_dens_overlay(data_list2$y, y_rep[1:200, ]) + 
  papaja::theme_apa()

options(repr.plot.width=10, repr.plot.height=6) 
pp_check2

# 提取后验样本
post_samples2 <- extract(model2_fit)
# 先将列表转换为矩阵
temp_matrix2 <- do.call(rbind, post_samples2)

# 转置矩阵
posterior_samples2 <- t(temp_matrix2)
column2_means <- apply(posterior_samples2, 2, mean, na.rm = TRUE)

# 计算预测值
df$pred_mean2 <- column2_means[1] + column2_means[2] * data_list2$X1 + 
  column2_means[3] * data_list2$X2 + column2_means[4] * data_list2$Matching

# 创建箱线图风格的后验预测图
p2 <- real_boxp +
  ggplot2::geom_point(aes(y = df$pred_mean2), 
                      color = "red", 
                      size = 3, 
                      shape = 16,
                      position = position_dodge(width = 0.8))

options(repr.plot.width=18, repr.plot.height=6) 
p1 + p2


####模型三
# 准备数据
Interaction_1 <- X1 * Matching
Interaction_2 <- X2 * Matching
# 定义Stan模型
model_code3 <- 
  "
data {
  int<lower=0> N;                 // 样本数量
  vector[N] y;               // 响应变量
  vector[N] X1;                   // 哑变量1（Friend）
  vector[N] X2;                   // 哑变量2（Stranger）
  vector[N] Matching;             // 哑变量（Matching条件）
  vector[N] Interaction_1;        // Friend 和 Matching 的交互
  vector[N] Interaction_2;        // Stranger 和 Matching 的交互
}

parameters {
  real beta_0;                    // 截距
  real beta_1;                    // Friend的主效应
  real beta_2;                    // Stranger的主效应
  real beta_3;                    // Matching的主效应
  real beta_4;                    // Friend与Matching的交互效应
  real beta_5;                    // Stranger与Matching的交互效应
  real<lower=0> sigma;            // 误差项的标准差
}

model {
  // 先验分布
  beta_0 ~ normal(5, 2);
  beta_1 ~ normal(0, 1);
  beta_2 ~ normal(0, 1);
  beta_3 ~ normal(0, 1);
  beta_4 ~ normal(0, 1);
  beta_5 ~ normal(0, 1);
  sigma ~ exponential(0.3);
  
  // 似然函数
    y ~ normal(beta_0 + beta_1 * X1 + beta_2 * X2 + beta_3 * Matching + 
                       beta_4 * Interaction_1 + beta_5 * Interaction_2, sigma);
}
generated quantities {
 real y_rep[N];

 for (n in 1:N) {
 y_rep[n] = normal_rng(beta_0 + beta_1 * X1[n] + beta_2 * X2[n], sigma);
 }
}
"
# 准备数据列表
data_list3 <- list(
  N = nrow(df),
  y = df$RT_sec,
  X1 = X1,
  X2 = X2,
  Matching = Matching,
  Interaction_1 = Interaction_1,
  Interaction_2 = Interaction_2
)

# 采样
model3_fit <- stan(model_code = model_code3, data = data_list3, 
                   iter = 5000, chains = 4, 
                   warmup = 1000, thin = 1, 
                   seed = 84735)

# MCMC诊断
neff_ratio3 <- as.data.frame(bayesplot::neff_ratio(model3_fit))
head(neff_ratio3,7)

model_summary3 <- summary(model3_fit)
round(head(model_summary3$summary, 7),3)

# HDI+ROPE
# 定义 ROPE 区间，根据研究的需要指定实际等效范围
rope_interval <- c(-0.05, 0.05)

# 提取后验样本
trace3 <- rstan::extract(model3_fit)


trace3_beta4 <- as.data.frame(trace3$beta_4)
trace3_beta5 <- as.data.frame(trace3$beta_5)

# 计算HDI
beta4_hdi95 <- bayestestR::hdi(trace3_beta4)
beta5_hdi95 <- bayestestR::hdi(trace3_beta5)

# 计算ROPE
rope4 <- bayestestR::rope(trace3_beta4,range = c(-0.05, 0.05))
rope5 <- bayestestR::rope(trace3_beta5,range = c(-0.05, 0.05))

# 绘图
beta4_plot <- plot(rope4, rope_color = "grey70") + 
  ggplot2::annotate("text", # 添加标签显示 Rope 值
                    x = 0, y = 0.5,
                    label = paste0("ROPE: ", round(rope4$ROPE_Percentage * 100, 1), "%"), 
                    color = "black", size = 6) +
  ggplot2::annotate("text",  # 添加标签显示 HDI 区间
                    x = mean(trace3$beta_4), y = 0.1,
                    label = paste0("95% HDI:\n[", round(beta4_hdi95$CI_low, 3), ", ", round(beta4_hdi95$CI_high, 3), "]"),
                    color = "black", size = 6) +
  papaja::theme_apa() + ggplot2::scale_y_continuous(expand = c(0, 0)) +
  ggplot2::theme(legend.position = c(0.85, 0.75))  # 调整图例位置

beta5_plot <- plot(rope5, rope_color = "grey70") + 
  ggplot2::annotate("text", # 添加标签显示 Rope 值
                    x = 0, y = 0.5,
                    label = paste0("ROPE: ", round(rope5$ROPE_Percentage * 100, 1), "%"), 
                    color = "black", size = 6) +
  ggplot2::annotate("text",  # 添加标签显示 HDI 区间
                    x = mean(trace3$beta_5), y = 0.1,
                    label = paste0("95% HDI:\n[", round(beta5_hdi95$CI_low, 3), ", ", round(beta5_hdi95$CI_high, 3), "]"),
                    color = "black", size = 6) +
  papaja::theme_apa() + ggplot2::scale_y_continuous(expand = c(0, 0)) +
  ggplot2::theme(legend.position = c(0.85, 0.75))  # 调整图例位置

# 显示图形
options(repr.plot.width=14, repr.plot.height=6) 
beta4_plot + beta5_plot

# 贝叶斯因子

# 定义先验模型
stan_prior_code3  <- 
  "
data {
  int<lower=0> N;                 // 样本数量
  vector[N] y;               // 响应变量
  vector[N] X1;                   // 哑变量1（Friend）
  vector[N] X2;                   // 哑变量2（Stranger）
  vector[N] Matching;             // 哑变量（Matching条件）
  vector[N] Interaction_1;        // Friend 和 Matching 的交互
  vector[N] Interaction_2;        // Stranger 和 Matching 的交互
}

parameters {
  real beta_0;                    // 截距
  real beta_1;                    // Friend的主效应
  real beta_2;                    // Stranger的主效应
  real beta_3;                    // Matching的主效应
  real beta_4;                    // Friend与Matching的交互效应
  real beta_5;                    // Stranger与Matching的交互效应
  real<lower=0> sigma;            // 误差项的标准差
}

model {
  // 先验分布
  beta_0 ~ normal(5, 2);
  beta_1 ~ normal(0, 1);
  beta_2 ~ normal(0, 1);
  beta_3 ~ normal(0, 1);
  beta_4 ~ normal(0, 1);
  beta_5 ~ normal(0, 1);
  sigma ~ exponential(0.3);
  
  // 似然函数
}
"

# 从先验采样
model3_pri <- stan(model_code = stan_prior_code3, 
                   data = data_list3, 
                   iter = 5000, 
                   warmup = 1000,
                   chains = 4,
                   seed = 84735)

# 提取先验样本
prior_samples3 <- extract(model3_pri)

# 提取后验样本
post_samples3 <- extract(model3_fit)

# 可以使用bayestestR包中的bayesfactor_parameters函数计算贝叶斯因子（Savage-Dickey）
beta4_BF <- bayestestR::bayesfactor_parameters(post_samples3$beta_4, prior_samples3$beta_4, direction = "two-sided", null = 0)
beta5_BF <- bayestestR::bayesfactor_parameters(post_samples3$beta_5, prior_samples3$beta_5, direction = "two-sided", null = 0)

print(beta4_BF)
print(beta5_BF)

# 配合see包可以对贝叶斯因子可视化
plot_BF4 <- plot(beta4_BF)+
  see::scale_fill_material_d(palette = "beta_4") +
  see::scale_color_material_d(palette = "beta_4") +
  xlim(-0.3,0.3) +
  papaja::theme_apa() +
  ggplot2::theme(legend.position = c(0.85, 0.75))  # 调整图例位置

plot_BF5 <- plot(beta5_BF)+
  see::scale_fill_material_d(palette = "beta_5") +
  see::scale_color_material_d(palette = "beta_5") +
  xlim(-0.3,0.3) +
  papaja::theme_apa() +
  ggplot2::theme(legend.position = c(0.85, 0.75))  # 调整图例位置

options(repr.plot.width=20, repr.plot.height=6) 
plot_BF4 + plot_BF5 

#后验预测
y_rep3 <- as.matrix(model3_fit, pars = "y_rep")

pp_check3 <- bayesplot::ppc_dens_overlay(data_list$y, y_rep[1:200, ]) + 
  papaja::theme_apa()

options(repr.plot.width=10, repr.plot.height=6) 
pp_check3

# 提取后验样本
post_samples3 <- extract(model3_fit)
# 先将列表转换为矩阵
temp_matrix3 <- do.call(rbind, post_samples3)

# 转置矩阵
posterior_samples3 <- t(temp_matrix3)
column3_means <- apply(posterior_samples3, 2, mean, na.rm = TRUE)

# 计算预测值
df$pred_mean3 <- column3_means[1] + column3_means[2] * data_list3$X1 + 
  column3_means[3] * data_list3$X2 + column3_means[4] * data_list3$Matching +
  column3_means[5] * data_list3$Interaction_1 + column3_means[6] * data_list3$Interaction_2

# 创建箱线图风格的后验预测图
p3 <- real_boxp +
  ggplot2::geom_point(aes(y = df$pred_mean3), 
                      color = "red", 
                      size = 3, 
                      shape = 16,
                      position = position_dodge(width = 0.8))

options(repr.plot.width=18, repr.plot.height=6) 
p2 + p3
