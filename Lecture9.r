# 导入所需库
options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
if (!requireNamespace('pacman', quietly = TRUE)) {
  install.packages('pacman')
}
pacman::p_load("tidyverse","ggplot2", "dplyr","gridExtra","papaja", "patchwork","bayesplot","rstan","brms")

# 加载数据
tryCatch({
  df_raw <- read_csv("/home/mw/input/bayes3797/Kolvoort_2020_HBM_Exp1_Clean.csv")
}, error = function(e) {
  df_raw <- read_csv("2024/data/Kolvoort_2020_HBM_Exp1_Clean.csv")
})

# 显示数据前几行
head(df_raw)

# 筛选出被试"201"，匹配类型为"Matching"的数据
df_raw$Subject <- as.character(df_raw$Subject)
df <- df_raw %>%
  dplyr::filter(Subject == "201" & Matching == "Matching") %>%
  # 选择需要的两列
  dplyr::select(Label, RT_sec) %>%
  # 重新编码标签（Label）
  dplyr::mutate(Label = case_when(
    Label == 1 ~ 0,
    Label == 2 ~ 1,
    Label == 3 ~ 1
  )) %>%
  # 设置索引
  dplyr::mutate(index = row_number()) %>%
  column_to_rownames("index")

# 显示处理后的数据前几行
head(df)

# 计算每个Label条件下的均值
mean_values <- df %>%
  dplyr::group_by(Label) %>%
  dplyr::summarise(mean_RT = mean(RT_sec), .groups = "drop")

# 绘制符合APA格式的箱线图
ggplot2::ggplot(df, aes(x = factor(Label), y = RT_sec)) +
  ggplot2::geom_boxplot() +
  ggplot2::geom_line(data = mean_values, aes(x = factor(Label), y = mean_RT, group = 1), 
                     color = "red", linewidth = 1) +
  ggplot2::geom_point(data = mean_values, aes(x = factor(Label), y = mean_RT), 
                      color = "red", size = 3) +
  # 使用APA格式主题
  papaja::theme_apa() +
  # 添加标签（APA格式通常要求清晰简洁的标签）
  labs(x = "Label Condition (0 = self, 1 = other)",
       y = "Reaction Time (sec)") +
  # 调整图形大小（APA建议图形比例协调）
  ggplot2::theme(plot.width = unit(5, "in"),
                 plot.height = unit(3.2, "in"))

# 定义先验分布的参数
mu_beta0 <- 5         
sigma_beta0 <- 2     
mu_beta1 <- 0       
sigma_beta1 <- 1   
lambda_sigma <- 0.3      

# 生成 beta_0 的先验分布数据
x_beta0 <- seq(-5, 15, length.out = 1000)
y_beta0 <- dnorm(x_beta0, mean = mu_beta0, sd = sigma_beta0)
df_beta0 <- data.frame(x = x_beta0, y = y_beta0)

# 生成 beta_1 的先验分布数据
x_beta1 <- seq(-5, 5, length.out = 1000)
y_beta1 <- dnorm(x_beta1, mean = mu_beta1, sd = sigma_beta1)
df_beta1 <- data.frame(x = x_beta1, y = y_beta1)

# 生成 sigma 的先验分布数据（指数分布）
x_sigma <- seq(0, 10, length.out = 1000)
y_sigma <- dexp(x_sigma, rate = lambda_sigma)  # R中指数分布参数为rate=λ
df_sigma <- data.frame(x = x_sigma, y = y_sigma)

# 自定义despine函数（作为ggplot图层使用，无需传递参数）
despine <- function() {
  ggplot2::theme(
    panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.ticks = element_line(color = "black")
  )
}

# 绘制 beta_0 的先验分布
p1 <- ggplot2::ggplot(df_beta0, aes(x = x, y = y)) +
  ggplot2::geom_line(color = "black") +
  ggplot2::ggtitle(expression(N(5, 2^2))) +
  ggplot2::xlab(expression(beta[0])) +
  ggplot2::ylab("pdf") +
  papaja::theme_apa() +
  despine()  # 作为图层直接添加

# 绘制 beta_1 的先验分布
p2 <- ggplot2::ggplot(df_beta1, aes(x = x, y = y)) +
  ggplot2::geom_line(color = "black") +
  ggplot2::ggtitle(expression(N(0, 1^2))) +
  ggplot2::xlab(expression(beta[1])) +
  ggplot2::ylab("pdf") +
  papaja::theme_apa() +
  despine()

# 绘制 sigma 的先验分布
p3 <- ggplot2::ggplot(df_sigma, aes(x = x, y = y)) +
  ggplot2::geom_line(color = "black") +
  ggplot2::ggtitle(expression(Exp(0.3))) +
  ggplot2::xlab(expression(sigma)) +
  ggplot2::ylab("pdf") +
  papaja::theme_apa() +
  despine()


# 组合图形
p1 + p2 + p3 + plot_layout(ncol = 3)

# 设置随机种子确保结果可重复
set.seed(84735)

# 根据设定的先验分布，各抽取200个样本
beta0_200 <- rnorm(200, mean = 5, sd = 2)       # 正态分布抽样（均值5，标准差2）
beta1_200 <- rnorm(200, mean = 0, sd = 1)       # 正态分布抽样（均值0，标准差1）
sigma_200 <- rexp(200, rate = 0.3)             # 指数分布抽样（率参数0.3，对应scale=1/0.3）

# 将结果存入数据框
prior_pred_sample <- data.frame(
  beta0 = beta0_200,
  beta1 = beta1_200,
  sigma = sigma_200
)

# 查看抽样结果
head(prior_pred_sample)

# 设置Label，0代表Self，1代表other
x_sim <- c(0, 1)

# 查看自变量值
x_sim

# 根据设定的先验分布，各抽取200个样本
beta0_200 <- rnorm(200, mean = 5, sd = 2)       # 正态分布抽样（均值5，标准差2）
beta1_200 <- rnorm(200, mean = 0, sd = 1)       # 正态分布抽样（均值0，标准差1）
sigma_200 <- rexp(200, rate = 0.3)              # 指数分布抽样（率参数0.3）

# 将结果存入数据框
prior_pred_sample <- data.frame(
  beta0 = beta0_200,
  beta1 = beta1_200,
  sigma = sigma_200
)

# 查看抽样结果
# prior_pred_sample

# 获取第一组采样参数
beta_0 <- prior_pred_sample$beta0[1]
beta_1 <- prior_pred_sample$beta1[1]

# 打印第一组采样参数值（保留两位小数）
cat(sprintf("获取的第一组采样参数值，beta_0:%.2f, beta_1:%.2f\n", beta_0, beta_1))

#===========================
# 根据回归公式 μ = β₀ + β₁X 预测μ的值
# 已知：自变量（标签），self = 1, other = 2
#===========================
x_sim <- c(1, 2)
mu <- beta_0 + beta_1 * x_sim
cat("预测值 μ:", mu, "\n")

#===========================
# 绘制回归线（完善练习部分）
#===========================
# 准备绘图数据（将x和对应的mu值组合成数据框）
plot_data <- data.frame(
  x_axis = ...,
  y_axis = ...
)

# 绘制回归线
ggplot2::ggplot(plot_data, aes(x = x_axis, y = y_axis)) +
  ggplot2::geom_line(color = "black", linewidth = 1) +  # 绘制回归线
  ggplot2::geom_point(color = "black", size = 3) +      # 添加数据点
  ggplot2::xlab("Label Condition") +                   # x轴标签
  ggplot2::ylab("RT (sec)") +                          # y轴标签
  ggplot2::theme_minimal() +
  # 移除顶部和右侧边框（模拟sns.despine效果）
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(color = "black")
  )

# 设置实验条件的取值范围，self=0，other=1
x_sim <- c(0, 1)

# 初始化空列表储存预测结果
mu_outcome <- list()

# 循环生成200次先验预测回归线
for (i in 1:nrow(prior_pred_sample)) {
  mu <- prior_pred_sample$beta0[i] + prior_pred_sample$beta1[i] * x_sim
  mu_outcome[[i]] <- mu
}

# 生成200种不同的颜色（使用hcl色空间，确保颜色差异明显）
n_lines <- length(mu_outcome)
colors <- hcl(
  h = seq(0, 360, length.out = n_lines + 1)[-1],  # 色相从0到360度循环
  c = 60,                                         # 饱和度
  l = 60,                                         # 亮度
  alpha = 0.6                                     # 半透明，避免重叠过深
)

# 准备绘图数据
plot_data <- data.frame(
  x = rep(x_sim, n_lines),
  y = unlist(mu_outcome),
  line_id = factor(rep(1:n_lines, each = length(x_sim)))  # 转换为因子用于分组着色
)

# 绘制带不同颜色的先验预测回归线
ggplot2::ggplot(plot_data, aes(x = x, y = y, group = line_id)) +
  ggplot2::geom_line(aes(color = line_id), linewidth = 0.5) +  # 按line_id分配颜色
  ggplot2::scale_color_manual(values = colors) +               # 使用自定义颜色
  ggplot2::ggtitle("prior predictive check") +
  ggplot2::xlab("Label Condition") +
  ggplot2::ylab("RT (sec)") +
  scale_x_continuous(breaks = seq(0,1),labels = c("Self","Other"))+ 
  papaja::theme_apa() +
  ggplot2::theme(
    legend.position = "none"  # 隐藏图例（200条线的图例无意义）
  )

prior_predictive_plot <- function(beta0_mean = 0.5, beta0_sd = 0.3, 
                                  beta1_mean = -0.1, beta1_sd = 0.04, 
                                  sigma_rate = 0.2, samples = 200, seed = 84735) {
  # 生成先验预测图。
  # 
  # 参数：
  # - beta0_mean: 数值，beta0的均值
  # - beta0_sd: 数值，beta0的标准差
  # - beta1_mean: 数值，beta1的均值
  # - beta1_sd: 数值，beta1的标准差
  # - sigma_rate: 数值，控制sigma的指数分布率参数（lambda = 1/scale）
  # - samples: 整数，生成的样本数量
  # - seed: 整数，随机种子，默认为84735，确保结果可重复
  # 
  # 输出：
  # - 一个先验预测图
  
  # 设置随机种子
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # 根据设定的先验分布抽样
  beta0_samples <- rnorm(samples, mean = beta0_mean, sd = beta0_sd)
  beta1_samples <- rnorm(samples, mean = beta1_mean, sd = beta1_sd)
  sigma_samples <- rexp(samples, rate = sigma_rate)
  
  # 创建数据框存储样本
  prior_pred_sample <- data.frame(
    beta0 = beta0_samples,
    beta1 = beta1_samples,
    sigma = sigma_samples
  )
  
  # 定义实验条件（self=0，other=1）
  x_sim <- c(0, 1)
  
  # 生成先验预测结果
  mu_outcome <- lapply(1:samples, function(i) {
    prior_pred_sample$beta0[i] + prior_pred_sample$beta1[i] * x_sim
  })
  
  # 生成多样化颜色
  colors <- hcl(
    h = seq(0, 360, length.out = samples + 1)[-1],
    c = 60,
    l = 60,
    alpha = 0.6
  )
  
  # 准备绘图数据
  plot_data <- data.frame(
    x = rep(x_sim, samples),
    y = unlist(mu_outcome),
    line_id = factor(rep(1:samples, each = length(x_sim)))
  )
  
  # 绘图
  ggplot(plot_data, aes(x = x, y = y, group = line_id)) +
    geom_line(aes(color = line_id), linewidth = 0.5) +
    scale_color_manual(values = colors) +
    ggtitle("Prior Predictive Check") +
    xlab("Label Condition") +
    ylab("RT (sec)") +
    scale_x_continuous(breaks = seq(0,1),labels = c("Self","Other"))+ 
    papaja::theme_apa() +
    theme(
      legend.position = "none",
    )
  
  # 显示图形
  print(last_plot())
}

# 使用示例
prior_predictive_plot()

#===============================================================
#     请完善代码中...的部分，设置3个参数的值，使先验分布更符合实际情况
#===============================================================
# 调用函数并设置符合实际情况的先验参数
prior_predictive_plot(
  beta0_mean = ...,    # beta0的均值
  beta0_sd = ...,      # beta0的标准差
  beta1_mean = ...,    # beta1的均值
  beta1_sd = ...,      # beta1的标准差
  sigma_rate = ...,    # sigma的指数分布率参数
  samples = 200,
  seed = 84735
)

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

lm_model <- "data {
  int<lower=1> N;         // number of observations
  vector[N] Label;        // predictor
  vector[N] RT;           // outcome
}
parameters {
  real beta0;
  real beta1;
  real<lower=0> sigma;
}
model {
  // Priors
  beta0 ~ normal(5, 2);      // beta0 ~ N(5, 2^2)
  beta1 ~ normal(0, 1);      // beta1 ~ N(0, 1^2)
  sigma ~ exponential(0.3);  // sigma ~ Exp(rate = 0.3)

  // Likelihood
  RT ~ normal(beta0 + beta1 .* Label, sigma);
}
generated quantities {
  vector[N] y_rep;
  vector[N] log_lik;
  for (n in 1:N) {
    y_rep[n] = normal_rng(beta0 + beta1 * Label[n], sigma);
    log_lik[n] = normal_lpdf(RT[n] | beta0 + beta1 * Label[n], sigma);
  }
}

"

lm_data <- list(
  N = nrow(df),
  Label = df$Label,
  RT = df$RT_sec
)

lm1_fit <- rstan::stan(
  model_code = lm_model,             # 定义的模型或模型文件路径
  data = lm_data,                    # 输入数据
  chains = 4,                        # 马尔可夫链数量
  iter = 2000,                       # 总迭代次数（每个链）
  warmup = 1000,                     # 热身迭代次数（不保存）
  seed = 84735
)

print(lm1_fit, pars = c("beta0", "beta1", "sigma"))

par_post <- rstan::extract(lm1_fit)
lm1_fit

# 查看后验分布（对应trace.posterior）
par_post <- rstan::extract(lm1_fit)
print(head(par_post))

# 提取beta_0的后验样本（对应trace.posterior['beta_0']）
beta0_posterior <- par_post$beta0
print("beta_0的后验样本（前10个）：")
print(head(beta0_posterior, 10))

# 提取第1条链的第11个样本（R索引从1开始，对应Python的[0,10]）
# 注：brms默认将所有链的样本合并，按顺序排列
chain1_sample11 <- beta0_posterior[11]  # 第1条链的第11个样本（前1000个是warmup，已自动丢弃）
cat(sprintf("第1条链的第11个beta_0样本值：%.4f\n", chain1_sample11))

par_trace <- rstan::traceplot(lm1_fit, pars = c("beta0", "beta1", "sigma"))
par_dist <- bayesplot::mcmc_dens_overlay(lm1_fit, pars = c("beta0", "beta1", "sigma"))
par_trace <- par_trace + papaja::theme_apa()
par_dist <- par_dist + papaja::theme_apa()

par_trace + par_dist + plot_layout(ncol = 1)

summary(lm1_fit, par=c("beta0","beta1","sigma"))$summary

print(bayesplot::rhat(lm1_fit, par=c("beta0","beta1","sigma")))

print(bayesplot::neff_ratio(lm1_fit, par=c("beta0","beta1","sigma")))

bayesplot::mcmc_acf(lm1_fit, par=c("beta0","beta1","sigma")) + papaja::theme_apa()

par_post <- data.frame(rstan::extract(lm1_fit, par=c("beta0","beta1","sigma")))
head(par_post)

# 定义x轴代表的Label（0=Self，1=Other）
x_sim <- c(0, 1)

# 提取后验样本并转换数据框
par_post <- data.frame(rstan::extract(lm1_fit, par=c("beta0","beta1","sigma")))

# 选取前2个样本用于预测（对应Python代码的[:2]）
beta_0 <- head(par_post$beta0, 2)
beta_1 <- head(par_post$beta1, 2)

# 生成回归线（每个样本对应一条线）
y_sim_re <- lapply(1:2, function(i) {
  beta_0[i] + beta_1[i] * x_sim
})

# 转换为绘图数据框
pred_data <- data.frame(
  x = rep(x_sim, 2),
  y = unlist(y_sim_re),
  sample = factor(rep(1:2, each = length(x_sim)))
)

# 提取观测数据（真实数据）
observed_data <- data.frame(
  x = df$Label,  # 原始Label数据
  y = df$RT_sec  # 原始反应时间
)

# 绘制后验预测检查图
ggplot() +
  # 绘制真实数据散点图
  geom_point(data = observed_data, aes(x = x, y = y), 
             color = "red", alpha = 0.6, size = 2, label = "observed data",
             position = position_jitter(width = 0.1)) +
  # 绘制回归线
  geom_line(data = pred_data, aes(x = x, y = y, group = sample), 
            color = "grey50", linewidth = 1) +
  # 绘制回归线端点
  geom_point(data = pred_data, aes(x = x, y = y), 
             color = "black", size = 3) +
  # 设置坐标轴范围和刻度
  xlim(-0.5, 1.5) +
  scale_x_continuous(breaks = c(0, 1)) +
  # 添加标题和标签
  ggtitle("posterior predictive check") +
  xlab("Label") +
  ylab("RT (sec)") +
  # 添加图例
  scale_color_manual(values = c("red", "grey50", "black"),
                     labels = c("observed data", "Predicted Mean", "")) +
  guides(color = guide_legend(override.aes = list(
    shape = c(16, NA, 16),
    linetype = c(0, 1, 0)
  ))) +
  # 美化主题
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "bottom"
  )

# 显示图形
# print(last_plot())

# 计算观测数据的分组均值
df <- df %>%
  group_by(Label) %>%
  mutate(`Mean RT` = mean(RT_sec)) %>%
  ungroup()


# 生成y_model预测值
x_values <- c(0, 1)
y_model <- lapply(1:nrow(par_post), function(i) {
  par_post$beta0[i] + par_post$beta1[i] * x_values
})

# 转换为数据框并计算均值和95%可信区间
y_model_df <- do.call(rbind, y_model) %>%
  as.data.frame() %>%
  setNames(paste0("x=", x_values)) %>%
  pivot_longer(everything(), names_to = "x", values_to = "y") %>%
  mutate(x = as.numeric(sub("x=", "", x))) %>%
  group_by(x) %>%
  summarize(
    mean = mean(y),
    lower = quantile(y, 0.025),
    upper = quantile(y, 0.975)
  )

# 提取观测均值数据
observed_mean <- df %>%
  select(Label, `Mean RT`) %>%
  distinct() %>%
  rename(x = Label, y = `Mean RT`)

# 绘制后验预测线性模型（修正图例设置）
ggplot() +
  # 不确定性区间
  geom_ribbon(data = y_model_df, 
              aes(x = x, ymin = lower, ymax = upper, fill = "Uncertainty in mean"), 
              alpha = 0.5) +
  # 后验均值线
  geom_line(data = y_model_df, 
            aes(x = x, y = mean, color = "Mean"), 
            linewidth = 2) +
  # 观测均值点
  geom_point(data = observed_mean, 
             aes(x = x, y = y, color = "observed mean"), 
             size = 3) +
  # 坐标轴设置
  xlim(-0.5, 1.5) +
  scale_x_continuous(breaks = c(0, 1)) +
  xlab("Label") +
  ylab("RT (sec)") +
  # 图例样式设置（修正order参数错误）
  scale_fill_manual(values = "grey70", name = NULL) +
  scale_color_manual(values = c("black", "black"), name = NULL) +
  guides(
    fill = guide_legend(order = 2),
    color = guide_legend(order = 1,  # 单个order值，解决尺寸错误
                         override.aes = list(
                           shape = c(16, NA),  # 观测点为圆点，线为无形状
                           linetype = c(0, 1)  # 观测点无线条，线为实线
                         ))
  ) +
  # 主题设置
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "bottom",
    text = element_text(size = 16)
  )

# print(last_plot())

# 抽取第一组参数组合（R索引从1开始，对应Python的row_i=0）
row_i <- 1  
X_i <- 1   

# 计算正态分布的均值mu_i
mu_i <- par_post$beta0[row_i] + par_post$beta1[row_i] * X_i           
sigma_i <- par_post$sigma[row_i]

# 从正态分布中随机抽取一个值，作为预测值
prediction_i <- rnorm(n = 1, mean = mu_i, sd = sigma_i)

# 打印结果（可多次运行，观察相同参数下的预测值变化）
cat(sprintf("mu_i: %.2f, 预测值：%.2f\n", mu_i, prediction_i))

# 生成两个空列，用于储存均值mu和预测值y_new
par_post$mu <- NA
par_post$y_new <- NA

# 设置X_i的值和随机种子（保持与原代码一致）
X_i <- 1
set.seed(84735)

# 循环计算均值并生成预测值（共20000次，与后验样本数量一致）
for (row_i in 1:nrow(par_post)) {
  # 计算均值mu_i
  mu_i <- par_post$beta0[row_i] + par_post$beta1[row_i] * X_i
  par_post$mu[row_i] <- mu_i
  
  # 从正态分布中抽取预测值y_new
  par_post$y_new[row_i] <- rnorm(
    n = 1,
    mean = mu_i,
    sd = par_post$sigma[row_i]
  )
}

# 查看结果（可选）
head(par_post)

# 复制数据框
df2 <- df %>%
  filter(Label == 1)      # 筛选Label=1的行

# 查看x=1时y的取值
cat("x=1时y的取值有:", "\n")
print(df2$RT_sec)  # 输出Label=1对应的RT_sec值

# 计算X轴全局范围（覆盖mu和y_new）
x_min <- min(par_post$mu, par_post$y_new)
x_max <- max(par_post$mu, par_post$y_new)

# 计算Y轴全局范围（覆盖两个分布的密度最大值）
# 先分别计算两个分布的密度值
density_mu <- density(par_post$mu)
density_ynew <- density(par_post$y_new)
y_max <- max(density_mu$y, density_ynew$y)  # 取密度最大值
y_min <- 0  # 密度从0开始

# 设置图形布局（1行2列）
par(mfrow = c(1, 2))

# 第一个图：mu的分布（统一X和Y轴范围）
p1 <- ggplot(par_post, aes(x = mu)) +
  geom_density(color = "black", fill = "grey80", alpha = 0.5) +
  xlim(x_min, x_max) +  # 统一X轴
  ylim(y_min, y_max) +  # 统一Y轴
  ggtitle("mu distribution") +
  xlab("Value") +
  ylab("Density") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

# 第二个图：y_new的分布（完全一致的轴范围）
p2 <- ggplot(par_post, aes(x = y_new)) +
  geom_density(color = "black", fill = "grey80", alpha = 0.5) +
  xlim(x_min, x_max) +  # 与第一个图X轴一致
  ylim(y_min, y_max) +  # 与第一个图Y轴一致
  ggtitle("y_new distribution") +
  xlab("Value") +
  ylab("Density") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

# 显示图形
p1+p2

# 基于模型和后验样本生成后验预测分布
ppc_data <- rstan::extract(lm1_fit, par=c("y_rep"))
# 查看后验预测结果
ppc_data

pp_samples <- data.frame(rstan::extract(lm1_fit,par="y_rep"))
nrow(pp_samples)
ncol(pp_samples)

color_scheme_set("brightblue")

y <- df$RT_sec # observed data

ppc_plpt <- bayesplot::pp_check(
  y,
  yrep=data.matrix(pp_samples[1:100,]),
  ppc_dens_overlay
) +
  papaja::theme_apa()

ppc_plot


#-------------------------------------------------------
# 计算“平均 posterior predictive density”
#   对每个 draw 单独跑 density，然后对 y 值取 average
#-------------------------------------------------------
dens_list <- apply(pp_samples, 1, density)

# x 轴来自第一条 density（所有 density 的 x 都一致）
x_vals <- dens_list[[1]]$x

# y 轴为所有 density 的平均
y_vals <- Reduce("+", lapply(dens_list, function(d) d$y)) / length(dens_list)

df_avg <- data.frame(x = x_vals, y = y_vals)

#-------------------------------------------------------
# 把橙色虚线的“平均 posterior predictive density”叠加到 pp_check 图上
#-------------------------------------------------------
ppc_plot +
  geom_line(
    data = df_avg,
    aes(x = x, y = y),
    color = "orange",
    linetype = "dashed",
    linewidth = 1
  ) +
  labs(
    title = "Posterior Predictive Check with Mean Predictive Density",
    x = "RT",
    y = "Density"
  ) 

summary(lm1_fit,par=c("beta0","beta1","sigma"))$summary

rope_res <- bayestestR::rope(par_post$beta1,range = c(-0.05, 0.05))
rope_res

plot(rope_res, rope_color = "grey70") + papaja::theme_apa()

# 更简单的方法来构建贝叶斯线性模型
lm1_fit <- brms::brm(
  formula = RT_sec ~ Label,  # 公式：RT_sec ~ beta0 + beta1*Label
  data = df,                 # 数据框（包含Label和RT_sec列）
  family = gaussian(),       # 似然函数：正态分布
  
  # 定义先验分布（对应PyMC的先验设置）
  prior = c(
    prior(normal(5, 2), class = Intercept),  # beta0：截距项，对应Normal(mu=5, sigma=2)
    prior(normal(0, 1), class = b),          # beta1：Label的系数，对应Normal(mu=0, sigma=1)
    prior(exponential(3), class = sigma)     # sigma：误差项，对应Exponential(3)
  ),
  
  # MCMC采样参数
  iter = 2000,               # 总迭代次数（draws + tune = 5000 + 1000）
  warmup = 1000,             # 调参迭代次数（对应tune）
  chains = 4,                # 马尔可夫链数量
  cores = 4,                 # 并行计算核心数（加速采样）
  seed = 84735,              # 随机种子，确保结果可重复
  refresh = 0                # 不输出采样过程信息
)
