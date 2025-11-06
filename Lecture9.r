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

# 显示图形
# print(last_plot())

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
  x_axis = x_sim,
  y_axis = mu
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

# 显示图形
# print(last_plot())

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

# 调用函数并设置符合实际情况的先验参数
prior_predictive_plot(
  beta0_mean = 0.8,    # beta0的均值：self条件下的平均反应时约0.8秒（符合常见RT范围）
  beta0_sd = 0.2,      # beta0的标准差：控制self条件下的变异（较小的标准差使先验更集中）
  beta1_mean = 0.1,    # beta1的均值：other条件比self条件平均慢0.1秒（符合自我参照效应）
  beta1_sd = 0.05,     # beta1的标准差：组间差异的变异（较小值表示预期差异稳定）
  sigma_rate = 5,      # sigma的指数分布率参数：对应scale=0.2，控制残差变异（较小残差更合理）
  samples = 200,
  seed = 84735
)

# 构建贝叶斯线性模型
linear_model <- brm(
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
  iter = 6000,               # 总迭代次数（draws + tune = 5000 + 1000）
  warmup = 1000,             # 调参迭代次数（对应tune）
  chains = 4,                # 马尔可夫链数量
  cores = 4,                 # 并行计算核心数（加速采样）
  seed = 84735,              # 随机种子，确保结果可重复
  refresh = 0                # 不输出采样过程信息
)

# 查看后验分布（对应trace.posterior）
posterior <- posterior_samples(linear_model)
print(head(posterior))

# 提取beta_0的后验样本（对应trace.posterior['beta_0']）
beta0_posterior <- posterior$b_Intercept
print("beta_0的后验样本（前10个）：")
print(head(beta0_posterior, 10))

# 提取第1条链的第11个样本（R索引从1开始，对应Python的[0,10]）
# 注：brms默认将所有链的样本合并，按顺序排列
chain1_sample11 <- beta0_posterior[11]  # 第1条链的第11个样本（前1000个是warmup，已自动丢弃）
cat(sprintf("第1条链的第11个beta_0样本值：%.4f\n", chain1_sample11))

# 1. 提取后验样本并添加链信息
posterior <- posterior_samples(linear_model) %>%
  select(
    beta0 = b_Intercept,  # 截距项
    beta1 = b_Label,      # Label系数
    sigma = sigma         # 残差标准差
  )

n_chains <- 4                  # 4条链
samples_per_chain <- nrow(posterior) / n_chains  # 每条链样本数

# 标记链编号和迭代序号
posterior_with_chain <- posterior %>%
  mutate(
    chain = rep(1:n_chains, each = samples_per_chain),
    iteration = rep(1:samples_per_chain, times = n_chains)
  )

# 2. 绘制APA格式迹线图（采样轨迹）
trace_data <- posterior_with_chain %>%
  pivot_longer(cols = c(beta0, beta1, sigma), names_to = "parameter", values_to = "value")

trace_plot <- ggplot(trace_data, aes(x = iteration, y = value, color = factor(chain))) +
  geom_line(size = 0.1) +
  facet_wrap(~parameter, ncol = 1, scales = "free_y") +  # 纵向排列参数
  scale_color_brewer(palette = "Set1", name = "Chain") +  # APA推荐配色
  labs(
    x = "Iteration", 
    y = "Parameter Value", 
    title = "MCMC Sampling Traces"
  ) +
  papaja::theme_apa() +  # 应用APA格式主题
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),  # 标题居中
    legend.position = "bottom",                        # 图例在底部
    panel.border = element_rect(color = "black", fill = NA),  # 边框可见
    strip.text.x = element_text(size = 10)             # 分面标签大小
  )

# 3. 绘制APA格式后验分布图（密度曲线）
density_data <- posterior_with_chain %>%
  pivot_longer(cols = c(beta0, beta1, sigma), names_to = "parameter", values_to = "value")

density_plot <- ggplot(density_data, aes(x = value, color = factor(chain), fill = factor(chain))) +
  geom_density(alpha = 0.2, linewidth = 0.8) +  # 线条加粗，符合APA规范
  facet_wrap(~parameter, ncol = 1, scales = "free_x") +
  scale_color_brewer(palette = "Set1", name = "Chain") +
  scale_fill_brewer(palette = "Set1", name = "Chain") +
  labs(
    x = "Parameter Value", 
    y = "Density", 
    title = "Posterior Distributions"
  ) +
  papaja::theme_apa() +  # 应用APA格式主题
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA),
    strip.text.x = element_text(size = 10)
  )

# 4. 组合图形（2行1列，APA格式布局）
par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))  # 调整边距，符合APA留白规范
print(trace_plot)
print(density_plot)
par(mfrow = c(1, 1))  # 重置布局

# 1. 获取模型完整摘要（兼容所有brms版本）
full_summary <- summary(linear_model)

# 2. 提取固定效应参数（beta0和beta1）的诊断信息
# 动态匹配有效样本量列名（可能是Bulk_ESS或ESS）
fixed_cols <- colnames(full_summary$fixed)
ess_col <- if ("Bulk_ESS" %in% fixed_cols) "Bulk_ESS" else "ESS"

fixed_diag <- as.data.frame(full_summary$fixed) %>%
  select(
    r_hat = Rhat,          # 收敛指标r_hat（列名固定）
    ess_bulk = all_of(ess_col)  # 动态匹配有效样本量列名
  ) %>%
  rownames_to_column("parameter") %>%
  mutate(
    parameter = recode(
      parameter,
      "(Intercept)" = "beta0",  # 截距项重命名
      "Label" = "beta1"         # Label系数重命名
    )
  )

# 3. 提取sigma参数的诊断信息
sigma_cols <- colnames(full_summary$spec_pars)
sigma_ess_col <- if ("Bulk_ESS" %in% sigma_cols) "Bulk_ESS" else "ESS"

sigma_diag <- as.data.frame(full_summary$spec_pars) %>%
  select(
    r_hat = Rhat,
    ess_bulk = all_of(sigma_ess_col)
  ) %>%
  mutate(parameter = "sigma")  # 指定sigma参数名

# 4. 合并诊断信息并计算有效样本量占比
diagnostics <- bind_rows(fixed_diag, sigma_diag) %>%
  mutate(
    # 提取尾部有效样本量（动态匹配列名）
    ess_tail = if ("Tail_ESS" %in% fixed_cols) {
      c(full_summary$fixed[, "Tail_ESS"], full_summary$spec_pars[, "Tail_ESS"])
    } else {
      ess_bulk  # 若不存在则用批量有效样本量近似
    },
    ess_bulk_ratio = ess_bulk / 20000  # 总采样量=4链×5000=20000
  ) %>%
  select(parameter, r_hat, ess_bulk, ess_tail, ess_bulk_ratio)

# 显示诊断结果
print(diagnostics)

# 定义x轴代表的Label（0=Self，1=Other）
x_sim <- c(0, 1)

# 提取后验样本并转换为长格式
posterior <- posterior_samples(linear_model) %>%
  select(b_Intercept, b_Label) %>%  # 选择beta0和beta1
  rename(beta0 = b_Intercept, beta1 = b_Label)

# 选取前2个样本用于预测（对应Python代码的[:2]）
beta_0 <- head(posterior$beta0, 2)
beta_1 <- head(posterior$beta1, 2)

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
             color = "red", alpha = 0.6, size = 2, label = "observed data") +
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

# 提取后验样本中的beta0和beta1
posterior <- posterior_samples(linear_model) %>%
  select(b_Intercept, b_Label) %>%
  rename(beta0 = b_Intercept, beta1 = b_Label)

# 生成y_model预测值
x_values <- c(0, 1)
y_model <- lapply(1:nrow(posterior), function(i) {
  posterior$beta0[i] + posterior$beta1[i] * x_values
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

# 提取后验样本并转换为数据框（包含所有链和采样结果）
# brms的posterior_samples()已自动合并所有链的样本，共4链×5000采样=20000个样本
df_pos_sample <- posterior_samples(linear_model) %>%
  # 选择需要的参数并按原代码命名
  select(
    beta_0 = b_Intercept,  # 截距项对应beta_0
    beta_1 = b_Label,      # 斜率项对应beta_1
    sigma = sigma          # 残差标准差
  )

# 查看参数数据框
df_pos_sample

# 抽取第一组参数组合（R索引从1开始，对应Python的row_i=0）
row_i <- 1  
X_i <- 1   

# 计算正态分布的均值mu_i
mu_i <- df_pos_sample$beta_0[row_i] + df_pos_sample$beta_1[row_i] * X_i           
sigma_i <- df_pos_sample$sigma[row_i]

# 从正态分布中随机抽取一个值，作为预测值
prediction_i <- rnorm(n = 1, mean = mu_i, sd = sigma_i)

# 打印结果（可多次运行，观察相同参数下的预测值变化）
cat(sprintf("mu_i: %.2f, 预测值：%.2f\n", mu_i, prediction_i))

# 生成两个空列，用于储存均值mu和预测值y_new
df_pos_sample$mu <- NA
df_pos_sample$y_new <- NA

# 设置X_i的值和随机种子（保持与原代码一致）
X_i <- 1
set.seed(84735)

# 循环计算均值并生成预测值（共20000次，与后验样本数量一致）
for (row_i in 1:nrow(df_pos_sample)) {
  # 计算均值mu_i
  mu_i <- df_pos_sample$beta_0[row_i] + df_pos_sample$beta_1[row_i] * X_i
  df_pos_sample$mu[row_i] <- mu_i
  
  # 从正态分布中抽取预测值y_new
  df_pos_sample$y_new[row_i] <- rnorm(
    n = 1,
    mean = mu_i,
    sd = df_pos_sample$sigma[row_i]
  )
}

# 查看结果（可选）
head(df_pos_sample)

# 复制数据框
df2 <- df %>%
  filter(Label == 1)      # 筛选Label=1的行

# 查看x=1时y的取值
cat("x=1时y的取值有:", "\n")
print(df2$RT_sec)  # 输出Label=1对应的RT_sec值

# 计算X轴全局范围（覆盖mu和y_new）
x_min <- min(df_pos_sample$mu, df_pos_sample$y_new)
x_max <- max(df_pos_sample$mu, df_pos_sample$y_new)

# 计算Y轴全局范围（覆盖两个分布的密度最大值）
# 先分别计算两个分布的密度值
density_mu <- density(df_pos_sample$mu)
density_ynew <- density(df_pos_sample$y_new)
y_max <- max(density_mu$y, density_ynew$y)  # 取密度最大值
y_min <- 0  # 密度从0开始

# 设置图形布局（1行2列）
par(mfrow = c(1, 2))

# 第一个图：mu的分布（统一X和Y轴范围）
p1 <- ggplot(df_pos_sample, aes(x = mu)) +
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
p2 <- ggplot(df_pos_sample, aes(x = y_new)) +
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
print(p1)
print(p2)

# 重置图形布局
par(mfrow = c(1, 1))

# 基于模型和后验样本生成后验预测分布
ppc_data <- posterior_predict(linear_model)
# 查看后验预测结果
ppc_data

#-------------------------------------------------------
# 1. pp_check: 蓝色 posterior predictive + 黑色 observed
#-------------------------------------------------------
p <- pp_check(
  linear_model,
  type = "dens_overlay",
  ndraws = 300
)

#-------------------------------------------------------
# 2. posterior predictive draws
#    结构： (draw × n_obs)
#-------------------------------------------------------
pp_samples <- posterior_predict(linear_model)

#-------------------------------------------------------
# 3. 计算“平均 posterior predictive density”
#   对每个 draw 单独跑 density，然后对 y 值取 average
#-------------------------------------------------------
dens_list <- apply(pp_samples, 1, density)

# x 轴来自第一条 density（所有 density 的 x 都一致）
x_vals <- dens_list[[1]]$x

# y 轴为所有 density 的平均
y_vals <- Reduce("+", lapply(dens_list, function(d) d$y)) / length(dens_list)

df_avg <- data.frame(x = x_vals, y = y_vals)

#-------------------------------------------------------
# 4. 把橙色虚线的“平均 posterior predictive density”叠加到 pp_check 图上
#-------------------------------------------------------
p +
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
  ) +
  theme_bw(base_size = 14)

# 1. 预处理：统一 Label 编码为 0 / 1
df <- df_raw %>%
  mutate(
    Subject = as.character(Subject),
    Label = case_when(
      Label == 1 ~ 0,
      Label == 2 ~ 1,
      Label == 3 ~ 1,
      TRUE ~ Label   # 其他值保持（若数据结构不符，可再处理）
    )
  )

# 筛选特定被试和条件的数据
df_201 <- df %>% filter(Subject == "201", Matching == "Matching") %>% select(Label, RT_sec)
df_205 <- df %>% filter(Subject == "205", Matching == "Matching") %>% select(Label, RT_sec)

# 稳健后验整理与 APA 绘图函数（含灰色边界线）
plot_posterior_apa <- function(model, df_sub, title = "Posterior Predictive") {
  # 1) 计算观测均值及误差
  df_mean <- df_sub %>%
    group_by(Label) %>%
    summarise(
      Mean_RT = mean(RT_sec),
      SD_RT = sd(RT_sec),
      N = n(),
      SE_RT = SD_RT / sqrt(N),
      .groups = "drop"
    ) %>%
    arrange(Label)
  
  # 2) 生成后验预测
  linpred <- posterior_linpred(model, newdata = df_mean, transform = TRUE)
  
  # 3) 处理后验预测矩阵
  linpred_mat <- as.matrix(linpred)
  if (is.null(colnames(linpred_mat)) || any(colnames(linpred_mat) == "")) {
    colnames(linpred_mat) <- paste0("V", seq_len(ncol(linpred_mat)))
  }
  
  # 4) 转换为长格式数据
  pred_df <- as.data.frame(linpred_mat)
  pred_df$draw <- seq_len(nrow(pred_df))
  
  pred_long <- pred_df %>%
    pivot_longer(
      cols = -draw,
      names_to = "LabelIndex",
      values_to = "y_model"
    ) %>%
    mutate(
      idx = as.integer(gsub("\\D", "", LabelIndex)),
      Label = df_mean$Label[idx]
    )
  
  # 5) 汇总后验统计量
  pred_summary <- pred_long %>%
    group_by(Label) %>%
    summarise(
      y_mean = mean(y_model),
      y_lower = quantile(y_model, 0.025),
      y_upper = quantile(y_model, 0.975),
      .groups = "drop"
    ) %>%
    arrange(Label)
  
  # 6) 绘图（含灰色边界线）
  p <- ggplot() +
    # 后验95%区间（带灰色边界）
    geom_ribbon(
      data = pred_summary,
      aes(x = Label, ymin = y_lower, ymax = y_upper),
      alpha = 0.25,          # 填充透明度
      fill = "gray70",       # 填充色
      color = "gray50",      # 边界线颜色（灰色）
      linewidth = 0.6        # 边界线粗细
    ) +
    # 后验均值线
    geom_line(
      data = pred_summary,
      aes(x = Label, y = y_mean),
      linewidth = 1
    ) +
    # 观测均值点
    geom_point(
      data = df_mean,
      aes(x = Label, y = Mean_RT),
      size = 3
    ) +
    # 观测均值误差条
    geom_errorbar(
      data = df_mean,
      aes(x = Label, ymin = Mean_RT - SE_RT, ymax = Mean_RT + SE_RT),
      width = 0.05,
      linewidth = 0.8
    ) +
    # APA风格主题
    papaja::theme_apa() +
    labs(
      x = "Label (0 = Self, 1 = Other)",
      y = "Reaction Time (s)",
      title = title
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    ) +
    scale_x_continuous(breaks = c(0, 1), limits = c(-0.1, 1.1)) +
    scale_y_continuous(limits = c(0.65, 0.95))
  
  return(p)
}

# 生成两个被试的图
p1 <- plot_posterior_apa(linear_model, df_201, "Subject 201")
p2 <- plot_posterior_apa(linear_model, df_205, "Subject 205")

# 并排显示
library(patchwork)  # 确保已安装patchwork包用于组合图形
(p1 + p2) & theme(plot.margin = unit(rep(8, 4), "pt"))

summary(linear_model)

# 1. 提取β₁的后验样本
posterior_beta1 <- as_draws_df(linear_model) %>%
  select(beta1 = b_Label) %>%
  pull(beta1)

# 2. 定义ROPE区间
rope_interval <- c(-0.05, 0.05)

# 3. 计算95% HDI
hdi <- function(x, prob = 0.95) {
  x_sorted <- sort(x)
  n <- length(x_sorted)
  window_size <- ceiling(prob * n)
  min_width <- Inf
  hdi_low <- x_sorted[1]
  hdi_high <- x_sorted[window_size]
  
  for (i in 1:(n - window_size + 1)) {
    current_low <- x_sorted[i]
    current_high <- x_sorted[i + window_size - 1]
    current_width <- current_high - current_low
    if (current_width < min_width) {
      min_width <- current_width
      hdi_low <- current_low
      hdi_high <- current_high
    }
  }
  c(hdi_low, hdi_high)
}

hdi_95 <- hdi(posterior_beta1, prob = 0.95)

# 4. 计算ROPE内的后验概率
rope_prob <- mean(posterior_beta1 >= rope_interval[1] & posterior_beta1 <= rope_interval[2]) * 100

# 5. 绘制后验分布（去除黑色均值线）
density_data <- density(posterior_beta1)
density_df <- data.frame(x = density_data$x, y = density_data$y)

ggplot(density_df, aes(x = x, y = y)) +
  # 后验分布填充（浅蓝色）
  geom_area(fill = "#3498db", alpha = 0.3) +
  # 95% HDI区间（深蓝色竖线）
  geom_vline(xintercept = hdi_95, color = "#2980b9", linetype = "solid", linewidth = 0.7) +
  # ROPE区间（灰色虚线边框）
  annotate(
    "rect",
    xmin = rope_interval[1], xmax = rope_interval[2],
    ymin = 0, ymax = Inf,
    fill = "grey80", alpha = 0.5,
    color = "grey50", linetype = "dashed", linewidth = 0.5
  ) +
  # （已去除：后验均值黑色竖线）
  # 标注ROPE内比例
  annotate(
    "text",
    x = mean(posterior_beta1), y = max(density_df$y) * 0.9,
    label = paste0("ROPE: ", round(rope_prob, 1), "%"),
    color = "black", size = 4
  ) +
  # 标注95% HDI范围
  annotate(
    "text",
    x = hdi_95[2], y = max(density_df$y) * 0.8,
    label = paste0("95% HDI: [", round(hdi_95[1], 3), ", ", round(hdi_95[2], 3), "]"),
    color = "#2980b9", size = 4, hjust = 1
  ) +
  # 坐标轴与标题
  xlab(expression(beta[1])) +
  ylab("Density") +
  ggtitle(expression("Posterior of" ~ beta[1])) +
  # 主题设置
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    panel.grid = element_blank()
  )
