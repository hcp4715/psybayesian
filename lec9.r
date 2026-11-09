# Synchronized from lec9.qmd. Run from the project root.

# lec9.qmd R chunk 1
knitr::opts_chunk$set(fig.path = "figs/lec9/", fig.width = 12, fig.height = 5, dpi = 144)

# lec9.qmd R chunk 2
# 导入所需库
options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
if (!requireNamespace('pacman', quietly = TRUE)) {
    install.packages('pacman')
}
pacman::p_load("tidyverse","ggplot2", "dplyr","gridExtra","papaja", "patchwork","bayesplot","rstan","brms")

# lec9.qmd R chunk 3
# 加载数据
df_raw <- utils::read.csv(file.path("data", "Kolvoort_2020_HBM_Exp1_Clean.csv"))

# 显示数据前几行
head(df_raw)

# lec9.qmd R chunk 4
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
    Label == 3 ~ 1, .default = NA
  )) %>%
  # 设置索引
  dplyr::mutate(index = row_number()) %>%
  column_to_rownames("index")

# 显示处理后的数据前几行
head(df)

# lec9.qmd R chunk 5
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
       y = "Reaction Time (sec)")

# lec9.qmd R chunk 6
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

# lec9.qmd R chunk 7
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

# lec9.qmd R chunk 8
# 设置Label，0代表Self，1代表other
x_sim <- c(0, 1)

# 查看自变量值
x_sim

# lec9.qmd R chunk 9
#===============================================================
# 左图：展示200组先验参数抽样结果
#===============================================================

# 整理三个参数的抽样结果
prior_draws_long <- tidyr::pivot_longer(
  data.frame(
    draw = seq_len(nrow(prior_pred_sample)),
    prior_pred_sample
  ),
  cols = c(beta0, beta1, sigma),
  names_to = "Parameter",
  values_to = "Value"
)

# 设置参数名称及绘图顺序
prior_draws_long$Parameter <- factor(
  prior_draws_long$Parameter,
  levels = c("beta0", "beta1", "sigma"),
  labels = c("beta_0", "beta_1", "sigma")
)

# 绘制三个参数的采样变化曲线
p_parameters <- ggplot2::ggplot(
  prior_draws_long,
  ggplot2::aes(x = draw, y = Value, color = Parameter)
) +
  ggplot2::geom_line(linewidth = 0.65) +
  ggplot2::scale_color_manual(
    values = c(
      "beta_0" = "#1f77b4",
      "beta_1" = "#ff7f0e",
      "sigma" = "#2ca02c"
    )
  ) +
  ggplot2::scale_x_continuous(
    breaks = c(0, 50, 100, 150, 200)
  ) +
  ggplot2::ggtitle(
    "Sampled Parameters: beta_0, beta_1, sigma"
  ) +
  ggplot2::xlab("Sample Index") +
  ggplot2::ylab("Value") +
  ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      hjust = 0.5, size = 13
    ),
    legend.title = ggplot2::element_blank(),
    legend.position = c(0.03, 0.97),
    legend.justification = c(0, 1),
    legend.background = ggplot2::element_rect(
      fill = "white", color = "grey80"
    )
  )

#===============================================================
# 右图：展示第一组参数对应的预测均值回归线
#===============================================================

# 选择第一组先验抽样参数
sample_index <- 1

# 提取同一组参数中的beta_0和beta_1
beta_0 <- prior_pred_sample$beta0[sample_index]
beta_1 <- prior_pred_sample$beta1[sample_index]

# 根据回归公式计算两个条件下的预测均值
first_mu <- beta_0 + beta_1 * x_sim

# 生成回归方程标签
equation_label <- sprintf(
  "mu = %.2f %s %.2f * X",
  beta_0,
  ifelse(beta_1 >= 0, "+", "-"),
  abs(beta_1)
)

# 准备绘图数据
line_data <- data.frame(
  Label = x_sim,
  mu = first_mu,
  Equation = equation_label
)

# 绘制预测均值回归线
p_line <- ggplot2::ggplot(
  line_data,
  ggplot2::aes(x = Label, y = mu)
) +
  ggplot2::geom_line(
    ggplot2::aes(color = Equation),
    linewidth = 1
  ) +
  ggplot2::geom_point(
    color = "red",
    size = 2.5
  ) +
  ggplot2::scale_color_manual(
    values = stats::setNames("blue", equation_label)
  ) +
  ggplot2::scale_x_continuous(
    breaks = c(0, 1),
    labels = c("self (0)", "other (1)")
  ) +
  ggplot2::ggtitle("Sampled Regression Line") +
  ggplot2::xlab("X (Condition)") +
  ggplot2::ylab("Predicted Mean RT (sec)") +
  ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      hjust = 0.5, size = 13
    ),
    legend.title = ggplot2::element_blank(),
    legend.position = c(0.97, 0.97),
    legend.justification = c(1, 1),
    legend.background = ggplot2::element_rect(
      fill = "white", color = "grey80"
    )
  )

#===============================================================
# 组合两张图
#===============================================================

p_parameters + p_line +
  patchwork::plot_layout(
    ncol = 2,
    widths = c(1.1, 1)
  )

# lec9.qmd R chunk 10
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
sigma_1 <- prior_pred_sample$sigma[1]

# 打印第一组采样参数值（保留两位小数）
cat(sprintf(
  "第一组参数：beta_0: %.2f, beta_1: %.2f, sigma: %.2f\n",
  beta_0, beta_1, sigma_1
))

# lec9.qmd R chunk 11
#===========================
# 根据回归公式 μ = β₀ + β₁X 预测μ的值
# 已知：自变量（标签），self = 0, other = 1
#===========================
x_sim <- c(0, 1)
mu <- beta_0 + beta_1 * x_sim
cat("预测值 μ:", mu, "\n")


#===========================
# 根据预测均值和sigma生成模拟观测值
#===========================

# 每个条件分别抽取一个预测观测值
y_rep <- rnorm(
  n = length(x_sim),
  mean = mu,
  sd = sigma_1
)

# 整理预测结果
prior_pred_one <- data.frame(
  Condition = factor(
    x_sim,
    levels = c(0, 1),
    labels = c("Self", "Other")
  ),
  mu = mu,
  y_rep = y_rep
)

# 查看预测结果
prior_pred_one

# lec9.qmd R chunk 12
if (FALSE) { # Incomplete exercise in lec9.qmd
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
}

# lec9.qmd R chunk 13

# 设置实验条件的取值范围，self=0，other=1
x_sim <- c(0, 1)

# 初始化空列表储存预测结果
mu_outcome <- list()
y_rep_outcome <- list()

# 循环生成200次先验预测结果
for (i in 1:nrow(prior_pred_sample)) {

  # 第一步：计算两个条件下的预测均值
  mu <- prior_pred_sample$beta0[i] +
        prior_pred_sample$beta1[i] * x_sim

  mu_outcome[[i]] <- mu

  # 第二步：结合sigma，每个条件抽取一个预测观测值
  y_rep_outcome[[i]] <- rnorm(
    n = length(x_sim),
    mean = mu,
    sd = prior_pred_sample$sigma[i]
  )
}

#===============================================================
# 图1：绘制200条先验预测均值回归线
#===============================================================

# 生成200种不同的颜色
n_lines <- length(mu_outcome)

colors <- hcl(
  h = seq(0, 360, length.out = n_lines + 1)[-1],
  c = 60,
  l = 60,
  alpha = 0.6
)

# 准备绘图数据
plot_data <- data.frame(
  x = rep(x_sim, n_lines),
  y = unlist(mu_outcome),
  line_id = factor(
    rep(1:n_lines, each = length(x_sim))
  )
)

# 绘制带不同颜色的先验预测均值回归线
p_mu <- ggplot2::ggplot(
  plot_data,
  ggplot2::aes(x = x, y = y, group = line_id)
) +
  ggplot2::geom_line(
    ggplot2::aes(color = line_id),
    linewidth = 0.5
  ) +
  ggplot2::scale_color_manual(values = colors) +
  ggplot2::ggtitle("Prior Predictive Means") +
  ggplot2::xlab("Label Condition") +
  ggplot2::ylab("Mean RT (sec)") +
  ggplot2::scale_x_continuous(
    breaks = c(0, 1),
    labels = c("Self", "Other")
  ) +
  papaja::theme_apa() +
  ggplot2::theme(
    legend.position = "none"
  )

#===============================================================
# 图2：绘制先验预测观测值分布
#===============================================================

# 准备模拟观测值的绘图数据
y_rep_data <- data.frame(
  x = rep(x_sim, n_lines),
  y_rep = unlist(y_rep_outcome)
)

# 转换实验条件为因子
y_rep_data$Condition <- factor(
  y_rep_data$x,
  levels = c(0, 1),
  labels = c("Self", "Other")
)

# 绘制先验预测观测值分布
p_y_rep <- ggplot2::ggplot(
  y_rep_data,
  ggplot2::aes(x = Condition, y = y_rep)
) +
  ggplot2::geom_violin(
    fill = "lightblue",
    alpha = 0.6,
    trim = FALSE
  ) +
  ggplot2::geom_jitter(
    width = 0.08,
    alpha = 0.4,
    size = 1.5
  ) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray50"
  ) +
  ggplot2::ggtitle("Prior Predictive Distribution") +
  ggplot2::xlab("Label Condition") +
  ggplot2::ylab("Predicted RT (sec)") +
  papaja::theme_apa()

#===============================================================
# 组合两张图
#===============================================================

p_mu + p_y_rep +
  patchwork::plot_layout(ncol = 2)

# lec9.qmd R chunk 14
prior_predictive_plot <- function(beta0_mean = 0.5, beta0_sd = 0.3,
                                  beta1_mean = -0.1, beta1_sd = 0.04,
                                  sigma_rate = 0.2, samples = 200,
                                  seed = 84735) {

  # 生成先验预测图。
  #
  # 参数：
  # - beta0_mean: 数值，beta0的均值
  # - beta0_sd: 数值，beta0的标准差
  # - beta1_mean: 数值，beta1的均值
  # - beta1_sd: 数值，beta1的标准差
  # - sigma_rate: 数值，控制sigma的指数分布率参数
  # - samples: 整数，先验参数抽样次数
  # - seed: 整数，随机种子
  #
  # 输出：
  # - 先验预测均值回归线
  # - 先验预测观测值分布

  # 设置随机种子
  if (!is.null(seed)) {
    set.seed(seed)
  }

  #===========================
  # 1. 根据设定的先验分布抽样
  #===========================

  beta0_samples <- rnorm(
    samples,
    mean = beta0_mean,
    sd = beta0_sd
  )

  beta1_samples <- rnorm(
    samples,
    mean = beta1_mean,
    sd = beta1_sd
  )

  sigma_samples <- rexp(
    samples,
    rate = sigma_rate
  )

  # 创建数据框存储参数抽样结果
  prior_pred_sample <- data.frame(
    beta0 = beta0_samples,
    beta1 = beta1_samples,
    sigma = sigma_samples
  )

  #===========================
  # 2. 定义实验条件
  #===========================

  x_sim <- c(0, 1)

  #===========================
  # 3. 生成先验预测结果
  #===========================

  mu_outcome <- list()
  y_rep_outcome <- list()

  for (i in 1:samples) {

    # 计算预测均值
    mu <- prior_pred_sample$beta0[i] +
          prior_pred_sample$beta1[i] * x_sim

    mu_outcome[[i]] <- mu

    # 每个条件结合sigma抽取一个预测观测值
    y_rep_outcome[[i]] <- rnorm(
      n = length(x_sim),
      mean = mu,
      sd = prior_pred_sample$sigma[i]
    )
  }

  #===========================
  # 4. 绘制先验预测均值回归线
  #===========================

  colors <- hcl(
    h = seq(0, 360, length.out = samples + 1)[-1],
    c = 60,
    l = 60,
    alpha = 0.6
  )

  # 准备均值回归线绘图数据
  mu_plot_data <- data.frame(
    x = rep(x_sim, samples),
    y = unlist(mu_outcome),
    line_id = factor(
      rep(1:samples, each = length(x_sim))
    )
  )

  # 绘制均值回归线
  p_mu <- ggplot2::ggplot(
    mu_plot_data,
    ggplot2::aes(x = x, y = y, group = line_id)
  ) +
    ggplot2::geom_line(
      ggplot2::aes(color = line_id),
      linewidth = 0.5
    ) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::ggtitle("Prior Predictive Means") +
    ggplot2::xlab("Label Condition") +
    ggplot2::ylab("Mean RT (sec)") +
    ggplot2::scale_x_continuous(
      breaks = c(0, 1),
      labels = c("Self", "Other")
    ) +
    papaja::theme_apa() +
    ggplot2::theme(
      legend.position = "none"
    )

  #===========================
  # 5. 绘制先验预测观测值分布
  #===========================

  y_rep_plot_data <- data.frame(
    x = rep(x_sim, samples),
    y_rep = unlist(y_rep_outcome)
  )

  # 转换实验条件为因子
  y_rep_plot_data$Condition <- factor(
    y_rep_plot_data$x,
    levels = c(0, 1),
    labels = c("Self", "Other")
  )

  # 绘制先验预测分布
  p_y_rep <- ggplot2::ggplot(
    y_rep_plot_data,
    ggplot2::aes(x = Condition, y = y_rep)
  ) +
    ggplot2::geom_violin(
      fill = "lightblue",
      alpha = 0.6,
      trim = FALSE
    ) +
    ggplot2::geom_jitter(
      width = 0.08,
      alpha = 0.4,
      size = 1.5
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed",
      color = "gray50"
    ) +
    ggplot2::ggtitle("Prior Predictive Distribution") +
    ggplot2::xlab("Label Condition") +
    ggplot2::ylab("Predicted RT (sec)") +
    papaja::theme_apa()

  #===========================
  # 6. 组合并显示两张图
  #===========================

  combined_plot <- p_mu + p_y_rep +
    patchwork::plot_layout(ncol = 2)

  print(combined_plot)

  # 返回抽样结果
  invisible(list(
    parameters = prior_pred_sample,
    means = mu_outcome,
    predictions = y_rep_outcome,
    plot = combined_plot
  ))
}

# lec9.qmd R chunk 15
if (FALSE) { # Incomplete exercise in lec9.qmd
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
}

# lec9.qmd R chunk 16
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

# lec9.qmd R chunk 17
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

# lec9.qmd R chunk 18
print(lm1_fit, pars = c("beta0", "beta1", "sigma"))

# lec9.qmd R chunk 19
par_post <- rstan::extract(lm1_fit)

# lec9.qmd R chunk 20
# 查看后验分布的数据结构
par_post <- rstan::extract(lm1_fit)
str(par_post, max.level = 1)

# lec9.qmd R chunk 21
# 提取 beta_0 的后验样本
beta0_posterior <- par_post$beta0
print("beta_0的后验样本（前10个）：")
print(head(beta0_posterior, 10))

# lec9.qmd R chunk 22
# 提取第 1 条链的第 11 个样本（R 索引从 1 开始）
# 注：rstan 将所有链的样本按顺序拼接
chain1_sample11 <- beta0_posterior[11]  # 第1条链的第11个样本（前1000个是warmup，已自动丢弃）
cat(sprintf("第1条链的第11个beta_0样本值：%.4f\n", chain1_sample11))

# lec9.qmd R chunk 23
par_trace <- rstan::traceplot(lm1_fit, pars = c("beta0", "beta1", "sigma"))
par_dist <- bayesplot::mcmc_dens_overlay(lm1_fit, pars = c("beta0", "beta1", "sigma"))
par_trace <- par_trace + papaja::theme_apa()
par_dist <- par_dist + papaja::theme_apa()

par_trace + par_dist + plot_layout(ncol = 1)

# lec9.qmd R chunk 24
summary(lm1_fit, par=c("beta0","beta1","sigma"))$summary

# lec9.qmd R chunk 25
print(bayesplot::rhat(lm1_fit, par=c("beta0","beta1","sigma")))

# lec9.qmd R chunk 26
print(bayesplot::neff_ratio(lm1_fit, par=c("beta0","beta1","sigma")))

# lec9.qmd R chunk 27
bayesplot::mcmc_acf(lm1_fit, par=c("beta0","beta1","sigma")) + papaja::theme_apa()

# lec9.qmd R chunk 28
par_post <- data.frame(rstan::extract(lm1_fit, par=c("beta0","beta1","sigma")))
head(par_post)

# lec9.qmd R chunk 29
# 定义x轴代表的Label（0=Self，1=Other）
x_sim <- c(0, 1)

# 提取后验样本并转换数据框
par_post <- data.frame(rstan::extract(lm1_fit, par=c("beta0","beta1","sigma")))

# 选取前 200 个样本用于预测
beta_0 <- head(par_post$beta0, 200)
beta_1 <- head(par_post$beta1, 200)

# 生成回归线（每个样本对应一条线）
y_sim_re <- lapply(1:200, function(i) {
  beta_0[i] + beta_1[i] * x_sim
})

# 转换为绘图数据框
pred_data <- data.frame(
  x = rep(x_sim, 200),
  y = unlist(y_sim_re),
  sample = factor(rep(1:200, each = length(x_sim)))
)

# 提取观测数据（真实数据）
observed_data <- data.frame(
  x = df$Label,  # 原始Label数据
  y = df$RT_sec  # 原始反应时间
)

# lec9.qmd R chunk 30
# 绘制后验预测检查图
ggplot() +
  # 观测值（红点，横向抖动避免重叠）
  geom_point(data = observed_data, aes(x = x, y = y),
             color = "red", alpha = 0.6, size = 2,
             position = position_jitter(width = 0.1)) +
  # 200 条后验预测回归线
  geom_line(data = pred_data, aes(x = x, y = y, group = sample),
            color = "grey50", linewidth = 0.4, alpha = 0.6) +
  # 每条线的两个端点（预测均值）
  geom_point(data = pred_data, aes(x = x, y = y),
             color = "black", size = 1.5, alpha = 0.4) +
  scale_x_continuous(breaks = c(0, 1), limits = c(-0.5, 1.5)) +
  ggtitle("posterior predictive check") +
  xlab("Label") +
  ylab("RT (sec)") +
  papaja::theme_apa()

# 显示图形
# print(last_plot())

# lec9.qmd R chunk 31
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

# lec9.qmd R chunk 32
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
  scale_x_continuous(breaks = c(0, 1), limits = c(-0.5, 1.5)) +
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
  papaja::theme_apa()

# print(last_plot())

# lec9.qmd R chunk 33
# 抽取第一组参数组合
row_i <- 1  
X_i <- 1   

# 计算正态分布的均值mu_i
mu_i <- par_post$beta0[row_i] + par_post$beta1[row_i] * X_i           
sigma_i <- par_post$sigma[row_i]

# 从正态分布中随机抽取一个值，作为预测值
prediction_i <- rnorm(n = 1, mean = mu_i, sd = sigma_i)

# 打印结果（可多次运行，观察相同参数下的预测值变化）
cat(sprintf("mu_i: %.2f, 预测值：%.2f\n", mu_i, prediction_i))

# lec9.qmd R chunk 34
# 生成两个空列，用于储存均值mu和预测值y_new
par_post$mu <- NA
par_post$y_new <- NA

# 设置X_i的值和随机种子（保持与原代码一致）
X_i <- 1
set.seed(84735)

# 循环计算均值并生成预测值（次数由后验样本数量决定）
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

# lec9.qmd R chunk 35
# 复制数据框
df2 <- df %>%
  filter(Label == 1)      # 筛选Label=1的行

# 查看x=1时y的取值
cat("x=1时y的取值有:", "\n")
head(df2$RT_sec, 10)  # 只显示前 10 个，避免刷屏

# lec9.qmd R chunk 36
# 计算X轴全局范围（覆盖mu和y_new）
x_min <- min(par_post$mu, par_post$y_new)
x_max <- max(par_post$mu, par_post$y_new)

# 计算Y轴全局范围（覆盖两个分布的密度最大值）
# 先分别计算两个分布的密度值
density_mu <- density(par_post$mu)
density_ynew <- density(par_post$y_new)
y_max <- max(density_mu$y, density_ynew$y)  # 取密度最大值
y_min <- 0  # 密度从0开始

# 第一个图：mu的分布（统一X和Y轴范围）
p1 <- ggplot(par_post, aes(x = mu)) +
  geom_density(color = "black", fill = "grey80", alpha = 0.5) +
  xlim(x_min, x_max) +  # 统一X轴
  ylim(y_min, y_max) +  # 统一Y轴
  ggtitle("mu distribution") +
  xlab("Value") +
  ylab("Density") +
  papaja::theme_apa() +
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
  papaja::theme_apa() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

# 显示图形
p1+p2

# lec9.qmd R chunk 37
# 基于模型和后验样本生成后验预测分布
ppc_data <- rstan::extract(lm1_fit, par=c("y_rep"))
# 查看后验预测矩阵的维度及前五次抽样的前五个观测值
dim(ppc_data$y_rep)
ppc_data$y_rep[1:5, 1:5]

# lec9.qmd R chunk 38
pp_samples <- data.frame(rstan::extract(lm1_fit,par="y_rep"))
nrow(pp_samples)
ncol(pp_samples)

# lec9.qmd R chunk 39
color_scheme_set("brightblue")

y <- df$RT_sec # observed data

ppc_plot <- bayesplot::pp_check(
  y,
  yrep=data.matrix(pp_samples[1:100,]),
  ppc_dens_overlay
) +
papaja::theme_apa()

ppc_plot

# lec9.qmd R chunk 40
#-------------------------------------------------------
# 计算“平均 posterior predictive density”
#   对每个 draw 单独跑 density，然后对 y 值取 average
#-------------------------------------------------------
# 在统一网格上计算每条 draw 的密度，再逐点平均
rng <- range(as.matrix(pp_samples))
x_vals <- seq(rng[1], rng[2], length.out = 512)
dens_mat <- vapply(seq_len(nrow(pp_samples)), function(i) {
  density(unlist(pp_samples[i, ]), from = rng[1], to = rng[2], n = 512)$y
}, numeric(512))
y_vals <- rowMeans(dens_mat)
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

# lec9.qmd R chunk 41
# 观测值与后验预测的检验统计量
T_mean_obs <- mean(y)
T_q95_obs  <- unname(quantile(y, 0.95))

T_mean_rep <- rowMeans(as.matrix(pp_samples))
T_q95_rep  <- apply(pp_samples, 1, quantile, probs = 0.95)

ppp_mean <- mean(T_mean_rep >= T_mean_obs)
ppp_q95  <- mean(T_q95_rep  >= T_q95_obs)

c(mean = ppp_mean, q95 = ppp_q95)

# lec9.qmd R chunk 42
summary(lm1_fit,par=c("beta0","beta1","sigma"))$summary

# lec9.qmd R chunk 43
hdi_beta1 <- bayestestR::hdi(par_post$beta1, ci = 0.95)
# ci = 1：使用全部后验样本计算落入 ROPE 的比例
rope_res <- bayestestR::rope(par_post$beta1, range = c(-0.05, 0.05), ci = 1)
hdi_beta1
rope_res

# lec9.qmd R chunk 44
plot(rope_res, rope_color = "grey70") + papaja::theme_apa()

# lec9.qmd R chunk 45
dir.create("tmpdata", showWarnings = FALSE)
# 更简单的方法来构建贝叶斯线性模型
lm1_brms <- brms::brm(
    formula = RT_sec ~ 0 + Intercept + Label,  # 截距直接对应 self 条件的 beta0
    data = df,                 # 数据框（包含Label和RT_sec列）
    family = gaussian(),       # 似然函数：正态分布
  
    # 定义先验分布（与 Stan 写法一致）
    prior = c(
      prior(normal(5, 2), class = b, coef = Intercept),  # beta0：self 条件的截距
      prior(normal(0, 1), class = b, coef = Label),      # beta1：Label 的系数
      prior(exponential(0.3), class = sigma)             # sigma：误差项
    ),
  
    # MCMC采样参数
    iter = 2000,               # 每条链总迭代 2000 次，保留 1000 次；4 条链共 4000 次
    warmup = 1000,             # 调参迭代次数
    chains = 4,                # 马尔可夫链数量
    cores = 4,                 # 并行计算核心数（加速采样）
    seed = 84735,              # 随机种子，确保结果可重复
    refresh = 0,               # 不输出采样过程信息
    file = "tmpdata/lec9_brms_fit",
    file_refit = "never"
)
print(lm1_brms)
