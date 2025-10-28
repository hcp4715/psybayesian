# 安装和加载包
options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
if (!requireNamespace('pacman', quietly = TRUE)) {
  install.packages('pacman')
}
pacman::p_load("tidyverse","ggplot2", "dplyr","gridExtra","papaja", "patchwork","bayesplot","rstan",'readr',"bridgesampling")
options(warn = -1)  # 抑制警告
rstan_options(auto_write = TRUE)


# 加载数据
df_raw <- read.csv("data/Kolvoort_2020_HBM_Exp1_Clean.csv")  

# 筛选被试 "201" 和 Matching == "Matching"
df <- df_raw %>%
  filter(Subject == "201" & Matching == "Matching") %>%
  select(Label, RT_sec) %>%
  mutate(Label = ifelse(Label == 1, 0, 1))  # 重新编码 Label: 1->0, 2/3->1

# 添加索引
df$index <- 1:nrow(df)

# 如果需要保存
# write.csv(df, "Kolvoort_2020_HBM_Exp1_Clean_201.csv", row.names = FALSE)

# 查看前几行
head(df)

stan_model_code <- "
data {
  int<lower=0> N;          // 观测数量
  vector[N] x;             // 预测变量 (Label)
  vector[N] y;             // 响应变量 (RT_sec)
}
parameters {
  real beta_0;
  real beta_1;
  real<lower=0> sigma;
}
model {
  // 先验
  beta_0 ~ normal(5, 2);
  beta_1 ~ normal(0, 1);
  sigma ~ exponential(0.3);
  
  // 似然
  y ~ normal(beta_0 + beta_1 * x, sigma);
}
"

# 编译模型
linear_model <- stan_model(model_code = stan_model_code)

# 为Stan准备数据
stan_data <- list(
  N = nrow(df),
  x = df$Label,
  y = df$RT_sec
)

# 从后验采样
fit_posterior <- sampling(
  linear_model,
  data = stan_data,
  iter = 6000,  # draws=5000 + tune=1000
  warmup = 1000,
  chains = 4,
  seed = 84735,
  refresh = 0
)

print(fit_posterior, pars = c("beta_0", "beta_1", "sigma"))

trace_beta_0 <- bayesplot::mcmc_trace(
  fit_posterior,
  pars = "beta_0"
) +
  papaja::theme_apa()

#trace_beta_0

trace_beta_1 <- bayesplot::mcmc_trace(
  fit_posterior,
  pars = "beta_1"
) +
  papaja::theme_apa()

#trace_beta_1

trace_sigma <- bayesplot::mcmc_trace(
  fit_posterior,
  pars = "sigma"
) +
  papaja::theme_apa()

#trace_sigma

# =====================================
# 从先验采样（无数据似然）
# =====================================

# 修改Stan代码以采样先验（注释掉似然）
stan_prior_code <- "
data {
  int<lower=0> N;          // 虚拟
  vector[N] x;             // 虚拟
  vector[N] y;             // 虚拟
}
parameters {
  real beta_0;
  real beta_1;
  real<lower=0> sigma;
}
model {
  // 先验
  beta_0 ~ normal(5, 2);
  beta_1 ~ normal(0, 1);
  sigma ~ exponential(0.3);
  
  // 无似然
}
"

# 编译先验模型
prior_model <- stan_model(model_code = stan_prior_code)

# 从先验采样（使用虚拟数据）
fit_prior <- sampling(
  prior_model,
  data = stan_data,  # 虚拟
  iter = 6000,
  warmup = 1000,
  chains = 4,
  seed = 84735
)

# 提取beta_1的先验样本
posterior_samples <- extract(fit_prior)
beta1_prior <- posterior_samples$beta_1

# 提取后验样本
posterior_samples <- extract(fit_posterior)
beta1_posterior <- posterior_samples$beta_1

# 计算贝叶斯因子

# 使用核密度估计计算密度在 0 处的值
prior_density <- density(beta1_prior, from = -0.5, to = 0.5, n = 1024)
posterior_density <- density(beta1_posterior, from = -0.5, to = 0.5, n = 1024)

# 找到最接近 0 的点的密度值
idx_0_prior <- which.min(abs(prior_density$x))
idx_0_post <- which.min(abs(posterior_density$x))

prior_at_0 <- prior_density$y[idx_0_prior]
posterior_at_0 <- posterior_density$y[idx_0_post]

# 计算 Savage-Dickey 比
BF_01 <- posterior_at_0 / prior_at_0# 支持 H0 相对于 H1
BF_10 <- 1 / BF_01# 支持 H1（β₁ ≠ 0）相对于 H0（β₁ = 0）


#cat("The BF_10 is", round(BF_10, 2), "\n")
#cat("The BF_01 is", round(BF_01, 2), "\n")

# 绘制贝叶斯因子图
df_prior <- data.frame(
  beta_1 = prior_density$x,
  density = prior_density$y,
  type = "Prior"
)

df_posterior <- data.frame(
  beta_1 = posterior_density$x,
  density = posterior_density$y,
  type = "Posterior"
)

df_combined <- bind_rows(df_prior, df_posterior)

# 在 x=0 处标记点
point_data <- data.frame(
  beta_1 = 0,
  density = c(prior_at_0, posterior_at_0),
  type = c("Prior", "Posterior")
)

# 绘图
p <- ggplot(df_combined, aes(x = beta_1, y = density, color = type)) +
  geom_line(size = 1) +
  geom_point(data = point_data, aes(color = type), size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("Prior" = "steelblue", "Posterior" = "orange")) +
  labs(
    title = paste0("The BF_10 is ", round(BF_10, 2), "\nThe BF_01 is ", round(BF_01, 2)),
    x = expression(beta[1]),
    y = "Density",
    color = "Distribution"
  ) +
  xlim(-0.5, 0.5) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = c(0.7, 0.9),
    plot.title = element_text(hjust = 0.5)
  )

p

# H0: 仅截距
stan_H0 <- "
data {
  int<lower=0> N;
  vector[N] y;
}
parameters {
  real beta_0;
  real<lower=0> sigma;
}
model {
  // 先验
  beta_0 ~ normal(5, 2);
  sigma ~ exponential(0.3);  // 
  
  // 似然
  y ~ normal(beta_0, sigma);
}
"
# H1: 截距 + 斜率
stan_H1 <- "
data {
  int<lower=0> N;
  vector[N] x;
  vector[N] y;
}
parameters {
  real beta_0;
  real beta_1;
  real<lower=0> sigma;
}
model {
  // 先验
  beta_0 ~ normal(5, 2);
  beta_1 ~ normal(0, 1);
  sigma ~ exponential(0.3);  //
  
  // 似然
  y ~ normal(beta_0 + beta_1 * x, sigma);
}
"

# ==============================
# 2. 编译模型
# ==============================
cat("Compiling models...\n")
model_H0 <- stan_model(model_code = stan_H0)
model_H1 <- stan_model(model_code = stan_H1)

# ==============================
# 3. 采样（后验）
# ==============================
set.seed(84735)

cat("Sampling H0...\n")
fit_H0 <- sampling(
  model_H0,
  data = stan_data,
  iter = 5000,
  warmup = 1000,
  chains = 4,
  seed = 84735,
  refresh = 0
)

cat("Sampling H1...\n")
fit_H1 <- sampling(
  model_H1,
  data = stan_data,
  iter = 5000,
  warmup = 1000,
  chains = 4,
  seed = 84735,
  refresh = 0
)

bridge_H0 <- bridge_sampler(fit_H0)
bridge_H1 <- bridge_sampler(fit_H1)

# 计算 BF
BF_10 <- exp(bridge_H1$logml - bridge_H0$logml)
BF_01 <- 1 / BF_10

#cat("BF_10 =", round(BF_10, 3), "\n")
#cat("BF_01 =", round(BF_01, 3), "\n")

calculate_odds <- function(tace_samples, region = c(-0.05, 0.05)) {
  
  #计算区间 [-0.05, 0.05] 内的样本
  in_range <- tace_samples[tace_samples >= region[1] & tace_samples <= region[2]]
  
  #计算区间外的样本
  out_of_range <- tace_samples[tace_samples < region[1] | tace_samples > region[2]]
  
  #计算区间内外的比例
  P_in_range <- length(in_range) / length(tace_samples)
  P_out_of_range <- length(out_of_range) / length(tace_samples)
  
  #计算比率
  ratio <- P_out_of_range / P_in_range
  
  return(ratio)
}

prior_odds <- calculate_odds(beta1_prior, region<-c(-0.05, 0.05))

#cat("先验概率比(prior_odds) =", round(prior_odds, 4), "\n")

plot_region <- function(samples, region = c(-0.05, 0.05), dist_type = "Prior") {  
  df <- data.frame(x = samples)  
  
  # 计算密度用于 y 位置
  density_data <- density(samples, n = 1024)
  null_mask <- density_data$x >= region[1] & density_data$x <= region[2]
  alt_mask <- !null_mask
  
  null_density_vals <- density_data$y[null_mask]
  alt_density_vals <- density_data$y[alt_mask]
  
  # x 轴范围
  x_range <- range(samples)
  x_span <- diff(x_range)
  
  ggplot(df, aes(x = x)) +    
    stat_density(geom = "line", color = "black", size = 1) +    
    stat_density(      
      geom = "area",      
      data = df %>% filter(x >= region[1] & x <= region[2]),      
      fill = "blue", alpha = 0.3,      
      aes(y = ..density..)
    ) +    
    stat_density(      
      geom = "area",      
      data = df %>% filter(x < region[1] | x > region[2]),      
      fill = "yellow", alpha = 0.3,      
      aes(y = ..density..)
    ) +    
    # Null 标签
    annotate("text", 
             x = mean(region),  
             y = if(length(null_density_vals) > 0) max(null_density_vals) * 0.6 else 0,
             label = "Null", 
             color = "blue", 
             fontface = "bold",
             size = 4) +
    # Alternative 标签
    annotate("text", 
             x = if (abs(x_range[1]) > x_range[2]) {
               x_range[1] + 0.15 * x_span  
             } else {
               x_range[2] - 0.15 * x_span  
             },
             y = if(length(alt_density_vals) > 0) max(alt_density_vals) * 0.6 else 0,
             label = "Alternative", 
             color = "darkorange", 
             fontface = "bold",
             size = 4) +
    labs(      
      title = paste0(dist_type, " Distribution with Null Region"),      
      x = expression(beta[1]),      
      y = "Density"    
    ) +    
    scale_x_continuous(limits = c(x_range[1] - 0.05 * x_span, x_range[2] + 0.05 * x_span)) +    
    theme_minimal() +    
    theme(plot.title = element_text(hjust = 0.5)) 
}

plot_region(beta1_prior, region = c(-0.05, 0.05), dist_type = "Prior")

posterior_odds <- calculate_odds(beta1_posterior, region<-c(-0.05, 0.05))

#cat("后验概率比(posterior_odds) =", round(posterior_odds, 4), "\n")

plot_region(beta1_posterior, region = c(-0.05, 0.05), dist_type = "Posterior")

BF_10 = (posterior_odds)/(prior_odds)

#cat("贝叶斯因子（BF_10）:", BF_10,"\n")

# 创建长格式数据框
df_plot <- tibble(
  prior = beta1_prior,
  posterior = beta1_posterior
) %>%
  pivot_longer(
    cols = everything(),
    names_to = "distribution",
    values_to = "beta1"
  )

# 定义零区域
null_region <- c(-0.05, 0.05)

dens_prior <- density(beta1_prior, from = -1, to = 1)
dens_post <- density(beta1_posterior, from = -1, to = 1)
max_dens <- max(dens_prior$y, dens_post$y)

# 绘图
ggplot(df_plot, aes(x = beta1, fill = distribution, color = distribution)) +
  # 密度曲线 + 填充
  geom_density(alpha = 0.3, size = 1) +
  
  # 高亮零假设区域（[-0.05, 0.05]）
  annotate("rect", 
           xmin = null_region[1], xmax = null_region[2],
           ymin = -Inf, ymax = Inf,
           fill = "blue", alpha = 0.2, inherit.aes = FALSE) +
  
  # 垂直虚线 at 0
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey", size = 1) +
  
  annotate("text",
           x = 0,  # 放在零区域中心
           y = max_dens * 0.9,
           label = "Null",
           color = "blue",
           fontface = "bold",
           size = 4) +
  
  annotate("text",
           x = 0.4,  
           y = max_dens * 0.7,
           label = "Alternative",
           color = "darkorange",  # 比 coral 更清晰
           fontface = "bold",
           size = 4) +
  
  # 坐标轴范围
  xlim(-1, 1) +
  
  # 标签和标题
  labs(
    title = "Prior and Posterior Distribution",
    x = expression(beta[1]),
    y = "Density",
    fill = "Distribution",
    color = "Distribution"
  ) +
  
  # 颜色设置（cyan = 先验，coral = 后验）
  scale_fill_manual(values = c("prior" = "cyan", "posterior" = "coral")) +
  scale_color_manual(values = c("prior" = "cyan", "posterior" = "coral")) +
  
  # 移除顶部和右侧边框
  theme_minimal() +
  theme(
    panel.border = element_rect(fill = NA, colour = "black"),
    panel.grid.minor = element_blank(),
    legend.position = "upper right"
  )

# 请直接运行，无需修改
calculate_odds <- function(tace_samples, region = c(-0.05, 0.05)) {
  
  #计算区间 [-0.05, 0.05] 内的样本
  in_range <- tace_samples[tace_samples >= region[1] & tace_samples <= region[2]]
  
  #计算区间外的样本
  out_of_range <- tace_samples[tace_samples < region[1] | tace_samples > region[2]]
  
  #计算区间内外的比例
  P_in_range <- length(in_range) / length(tace_samples)
  P_out_of_range <- length(out_of_range) / length(tace_samples)
  
  #计算比率
  ratio <- P_out_of_range / P_in_range
  
  return(ratio)
}

plot_region <- function(samples, region = c(-0.05, 0.05), dist_type = "Prior") {  
  df <- data.frame(x = samples)  
  
  # 计算密度用于 y 位置
  density_data <- density(samples, n = 1024)
  null_mask <- density_data$x >= region[1] & density_data$x <= region[2]
  alt_mask <- !null_mask
  
  null_density_vals <- density_data$y[null_mask]
  alt_density_vals <- density_data$y[alt_mask]
  
  # x 轴范围
  x_range <- range(samples)
  x_span <- diff(x_range)
  
  ggplot(df, aes(x = x)) +    
    stat_density(geom = "line", color = "black", size = 1) +    
    stat_density(      
      geom = "area",      
      data = df %>% filter(x >= region[1] & x <= region[2]),      
      fill = "blue", alpha = 0.3,      
      aes(y = ..density..)
    ) +    
    stat_density(      
      geom = "area",      
      data = df %>% filter(x < region[1] | x > region[2]),      
      fill = "yellow", alpha = 0.3,      
      aes(y = ..density..)
    ) +    
    # Null 标签
    annotate("text", 
             x = mean(region),  
             y = if(length(null_density_vals) > 0) max(null_density_vals) * 0.6 else 0,
             label = "Null", 
             color = "blue", 
             fontface = "bold",
             size = 4) +
    # Alternative 标签
    annotate("text", 
             x = if (abs(x_range[1]) > x_range[2]) {
               x_range[1] + 0.15 * x_span  
             } else {
               x_range[2] - 0.15 * x_span  
             },
             y = if(length(alt_density_vals) > 0) max(alt_density_vals) * 0.6 else 0,
             label = "Alternative", 
             color = "darkorange", 
             fontface = "bold",
             size = 4) +
    labs(      
      title = paste0(dist_type, " Distribution with Null Region"),      
      x = expression(beta[1]),      
      y = "Density"    
    ) +    
    scale_x_continuous(limits = c(x_range[1] - 0.05 * x_span, x_range[2] + 0.05 * x_span)) +    
    theme_minimal() +    
    theme(plot.title = element_text(hjust = 0.5)) 
}

#=====================================
#      基于方向的贝叶斯因子计算
#      自行练习
#=====================================

# 获取 beta_1的先验分布的采样
# beta_1_prior = ...


# 获取 beta_1的后验分布的采样
# beta_1_posterior = ...

# 定义区间
# region = ...

# 计算先验比
# prior_odds = ...

# 计算后验比
# prior_odds = ...

# 计算贝叶斯因子
# BF_10 = ...

#cat("贝叶斯因子（BF_10）:", BF_10,"\n")

df_plot <- tibble(
  prior = ...,
  posterior = ...
) %>%
  pivot_longer(
    cols = everything(),
    names_to = "distribution",
    values_to = "beta1"
  )

# === 方向性假设：H1: β₁ < 0 ===
h0_boundary <- 0  # H₀: β₁ >= 0

# 计算最大密度用于标签定位
dens_prior <- density(beta1_prior, from = -1, to = 1)
dens_post <- density(beta1_posterior, from = -1, to = 1)
max_dens <- max(dens_prior$y, dens_post$y)

# 绘图
ggplot(df_plot, aes(x = beta1, fill = distribution, color = distribution)) +
  geom_density(alpha = 0.3, size = 1) +
  
  # 高亮 H₀ 区域：β₁ >= 0（右侧）
  annotate("rect", 
           xmin = h0_boundary, xmax = Inf,
           ymin = -Inf, ymax = Inf,
           fill = "blue", alpha = 0.2, inherit.aes = FALSE) +
  
  # 垂直线在 0 处（假设边界）
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey", size = 1) +
  
  # 标签：H₀ 在右侧，H₁ 在左侧
  annotate("text",
           x = 0.5,  # H₀ 区域（右侧）
           y = max_dens * 0.8,
           label = "H0: beta[1] >= 0",
           color = "blue",
           fontface = "bold",
           size = 4,
           parse = TRUE) +
  
  annotate("text",
           x = -0.5,  # H₁ 区域（左侧）
           y = max_dens * 0.6,
           label = "H1: beta[1] < 0",
           color = "darkorange",
           fontface = "bold",
           size = 4,
           parse = TRUE) +
  
  xlim(-1, 1) +
  
  labs(
    title = "Directional Hypothesis: Testing for Negative Effect",
    x = expression(beta[1]),
    y = "Density",
    fill = "Distribution",
    color = "Distribution"
  ) +
  
  scale_fill_manual(values = c("prior" = "cyan", "posterior" = "coral")) +
  scale_color_manual(values = c("prior" = "cyan", "posterior" = "coral")) +
  
  theme_minimal() +
  theme(
    panel.border = element_rect(fill = NA, colour = "black"),
    panel.grid.minor = element_blank(),
    legend.position = "upper right"
  )

