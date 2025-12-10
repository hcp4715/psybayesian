# 安装和加载包
options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
if (!requireNamespace('pacman', quietly = TRUE)) {
    install.packages('pacman')
}
pacman::p_load("tidyverse","ggplot2", "dplyr","gridExtra","patchwork","papaja", "bayesplot",
               "rstan","bridgesampling", 'logspline', "easystats", "brms", "RColorBrewer") 
options(warn = -1)  # 抑制警告

# 导入示例数据
df_raw <- tryCatch({
  read.csv('/home/mw/input/bayes3797/evans2020JExpPsycholLearn_exp1_full_data.csv')
}, error = function(e) {
  read.csv('data/evans2020JExpPsycholLearn_exp1_full_data.csv')
})

df_raw[1:10, c("subject", "RT")] # 显示前十行的数据

options(repr.plot.width=15, repr.plot.height=6) #自定义画布大小

# 计算每个被试的平均反应时间和标准误差
subject_stats <- df_raw %>%
  dplyr::group_by(subject) %>%
  dplyr::summarise(
    mean = mean(RT, na.rm = TRUE),
    std = sd(RT, na.rm = TRUE),
    count = n()
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    sem = std / sqrt(count),
    subject = as.character(subject)  # 转换为字符型
  ) %>%
  dplyr::arrange(dplyr::desc(mean))  # 按平均反应时间从高到低排序

# 绘制平均反应时间的可视化
p <- ggplot2::ggplot(data = subject_stats, ggplot2::aes(x = reorder(subject, -mean), y = mean)) +
  ggplot2::geom_bar(stat = "identity", fill = "skyblue") +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = mean - sem, ymax = mean + sem),
    width = 0.2,
    size = 0.8,
    color = "darkblue",
    position = ggplot2::position_dodge(width = 0.9)
  ) +
  ggplot2::labs(
    title = "Average Reaction Time by Subject",
    x = "Subject",
    y = "Mean Reaction Time (RT)"
  ) +
  papaja::theme_apa() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      angle = 45, # 使横坐标数值倾斜，避免重叠
      hjust = 1,
      vjust = 1
    )
  )

# 显示图形
print(p)

# 筛选出特定被试并创建索引
df_first5 <- df_raw %>%
  dplyr::filter(
    subject %in% c(81844, 83956, 83824, 66670, 80941),  # 筛选特定被试
    percentCoherence == 5  # 筛选percentCoherence等于5的数据
  ) %>%
  dplyr::group_by(subject) %>%
  dplyr::mutate(
    subj_id = subject,
    obs_id = dplyr::row_number(),
    log_RTs = log(RT)
  ) %>%
  dplyr::ungroup()

# 查看数据
head(df_first5)

# 设置画布大小
options(repr.plot.width = 15, repr.plot.height = 6)

# 绘制原始RT的直方图（第一行）
p_rt <- ggplot2::ggplot(data = df_first5, ggplot2::aes(x = RT)) +
  ggplot2::geom_histogram(
    fill = "skyblue",
    color = "black",
    bins = 30,
    na.rm = TRUE # 忽略缺失值
  ) +
  ggplot2::facet_wrap(
    ~subject,
    nrow = 1, ncol = 5,
    scales = "free"
  ) +
  ggplot2::theme_bw() +
  papaja::theme_apa()

# 绘制logRT的直方图（第二行）
p_logrt <- ggplot2::ggplot(data = df_first5, ggplot2::aes(x = log_RTs)) +
  ggplot2::geom_histogram(
    fill = "skyblue",
    color = "black",
    na.rm = TRUE
  ) +
  ggplot2::facet_wrap(
    ~subject,
    nrow = 1, ncol = 5,
    scales = "free"
  ) +
  ggplot2::theme_bw() +
  papaja::theme_apa()

# 组合图形
combined_plot <- p_rt / p_logrt +
  patchwork::plot_layout(heights = c(1, 1)) +
  patchwork::plot_annotation(
    title = "Histograms of RT and logRT by Subject",
    theme = ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  )

# 显示图形
print(combined_plot)

# 绘制所有被试的反应时间 (RT) 的分布图
ggplot2::ggplot(data = df_first5, ggplot2::aes(x = RT)) +
  ggplot2::geom_histogram(
    bins = 50,
    color = "black",
    fill = "skyblue",
  ) +
  papaja::theme_apa() +
  ggplot2::labs(
    title = "Distribution of Reaction Times (RT)",
    x = "Reaction Time (ms)",
    y = "Frequency"
  )

# 准备数据：对RT进行log转换
df_first5 <- df_first5 %>% 
  dplyr::mutate(log_RTs = log(RT))

# 拟合完全池化模型
complete_pooled_model <- brms::brm(
  formula = log_RTs ~ 1,  # 只包含截距项，完全池化（忽略被试间差异）
  data = df_first5,
  family = gaussian(),
  # 可以通过 brms::get_prior()查看所对应的class
  prior = c(
    prior(normal(7.5, 5), class = Intercept),  # mu的先验：N(7.5, 5)
    prior(exponential(1), class = sigma)       # sigma的先验：Exp(1)
  ),
  chains = 4,
  iter = 2000,
  warmup = 1000,
  seed = 84735,
  control = list(adapt_delta = 0.95)
)

# 从 brms 模型中提取后验样本
posterior_samples <- as.array(complete_pooled_model)

# 绘制迹图（trace plot）
trace_plot <- bayesplot::mcmc_trace(
  posterior_samples,  # 后验样本数组
  pars = c("b_Intercept", "sigma"),  # 要绘制的参数
  facet_args = list(nrow = 2, ncol = 1)
) +
  ggplot2::labs(title = "Trace Plots") +
  papaja::theme_apa()

# 绘制后验分布图
dens_plot <- bayesplot::mcmc_dens(
  posterior_samples,
  pars = c("b_Intercept", "sigma"),
  facet_args = list(nrow = 2, ncol = 1),
) +
  ggplot2::labs(title = "Posterior Distributions") +
  papaja::theme_apa()


# 显示图形
trace_plot + dens_plot

summary(complete_pooled_model)

# 定义逆对数转换函数
inv_log <- function(mu, sigma) {
  exp(mu + (sigma^2) / 2)
}

# 计算预测的RT均值
pred_rt <- inv_log(6.95, 0.76)

# 打印结果（保留3位小数）
cat("The estimated mean of RT is: ", round(pred_rt, 3), "\n")

# 从brms模型中提取mu的后验均值
mu_posterior_mean <- brms::fixef(complete_pooled_model)["Intercept", "Estimate"]

# 计算真实log RT的均值
truth_log_rt_mean <- mean(log(df_first5$RT))

# 打印结果
cat("The posterior mean of mu is: ", round(mu_posterior_mean, 3), "\n")
cat("The truth log RT mean is:", round(truth_log_rt_mean, 3), "\n")

# 进行后验预测检查
bayesplot::pp_check(complete_pooled_model) +
papaja::theme_apa()

ppc_sum <- function(ppc, data, y = "RT") {
  # 计算每个观测值的后验预测均值
  ppc_mean <- apply(ppc, 2, mean)
  
  # 计算每个观测值的后验预测标准差
  ppc_sd <- apply(ppc, 2, sd)
  
  # 计算每个观测值的95% HDI
  ppc_hdi <- t(apply(ppc, 2, function(x) {
    hdi_result <- bayestestR::hdi(x, ci = 0.95) 
    # 直接提取 CI_low 和 CI_high 列
    return(c(hdi_result$CI_low, hdi_result$CI_high))
  }))
  
  # 设置HDI矩阵的列名
  colnames(ppc_hdi) <- c("Q2.5", "Q97.5")
  
  # 构建结果数据框
  hdi_sum <- base::data.frame(
    mean = ppc_mean,              # 后验预测均值
    sd = ppc_sd,                  # 后验预测标准差
    Q2.5 = ppc_hdi[, "Q2.5"],     # 95% HDI下限
    Q97.5 = ppc_hdi[, "Q97.5"],   # 95% HDI上限
    y = data[[y]],                # 原始观测值
    obs_id = 1:nrow(data),        # 观测ID
    subject = data[["subject"]]   # 被试ID
  )
  
  return(hdi_sum)
}

# 生成后验预测
ppc <- brms::posterior_predict(complete_pooled_model)

# 调用函数计算HDI
hdi_sum <- ppc_sum(ppc, data = df_first5, y = "RT")

# 查看结果
head(hdi_sum)

inv_log_hdi_sum <- function(hdi_sum) {
  # 对均值、HDI下限和上限进行逆对数转换
  df <- dplyr::mutate(
    hdi_sum,
    mean = inv_log(mean, sd),  # 转换均值
    Q2.5 = inv_log(Q2.5, sd),   # 转换2.5%分位数（HDI下限）
    Q97.5 = inv_log(Q97.5, sd)  # 转换97.5%分位数（HDI上限）
  )
  
  return(df)
}
complete_hdi_sum <- inv_log_hdi_sum(hdi_sum)
head(complete_hdi_sum)

# 定义PPC绘图函数
ppc_plot <- function(hdi_sum, show_plot = TRUE) {
  df <- hdi_sum
  
  # 重置索引，创建obs_id列
  df <- dplyr::mutate(df, obs_id = dplyr::row_number() - 1)
  
  # 确保subject列是因子类型
  df <- dplyr::mutate(df, subject = as.factor(subject))
  
  # 选择Seaborn调色板
  n_subjects <- length(unique(df$subject))
  palette <- RColorBrewer::brewer.pal(n = min(8, n_subjects), name = "Set2")
  if (n_subjects > 8) {
    palette <- grDevices::colorRampPalette(palette)(n_subjects)
  }
  
  # 创建颜色映射向量（为每个subject分配一个颜色）
  subject_colors <- stats::setNames(palette, levels(df$subject))
  
  # 映射颜色到新列
  df <- dplyr::mutate(df,
    # 深色：用于散点
    color_dark = subject_colors[as.character(subject)],
    # 浅色：用于HDI区间
    color_light = scales::alpha(subject_colors[as.character(subject)], 0.3)
  )
  
  # 根据是否落在可信区间内分配颜色
  df <- dplyr::mutate(df,
    color_dark = ifelse(
      (y >= Q2.5) & (y <= Q97.5), 
      color_dark, "#C00000"  # 红色表示超出HDI
    )
  )
  
  # 计算每个被试的中心位置，用于x轴刻度
  subject_centers <- dplyr::group_by(df, subject) %>%
    dplyr::summarise(min_id = min(obs_id), max_id = max(obs_id), center = (min_id + max_id) / 2,
    .groups = "drop"
    )
  
  # 创建ggplot对象
  p <- ggplot2::ggplot(df, ggplot2::aes(x = obs_id)) +
    
    # 绘制94%可信区间
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = Q2.5, ymax = Q97.5),
      color = df$color_light, alpha = 0.1, size = 3, show.legend = FALSE
    ) +
    
    # 各被试散点图数据
    ggplot2::geom_point(
      ggplot2::aes(y = y), color = df$color_dark, alpha = 0.8, size = 3
    ) +
    
    # 绘制后验预测均值（使用横线标记）
    ggplot2::geom_point(
      ggplot2::aes(y = mean), shape = "_", color = "black", alpha = 0.7,
      size = 5, show.legend = FALSE
    ) +
    
    # 设置x轴刻度（按被试分组
    ggplot2::scale_x_continuous(
      breaks = subject_centers$center,
      labels = subject_centers$subject,
      expand = ggplot2::expansion(add = c(0.5, 0.5))
    ) +
    
    # 设置图形标题和标签
    ggplot2::labs(
      title = "Posterior Predictive Check (PPC) by Subject",
      x = "Subject ID",
      y = "Reaction Time (ms)"
    ) +
    papaja::theme_apa() +
    
    # 添加图例文本
    ggplot2::annotate(
      "text",
      x = Inf, y = Inf,
      label = "Outside HDI: Red points\n94% HDI: Light lines\nPosterior mean: Black dashes",
      hjust = 1.1, vjust = 1.1, size = 3.5
    )
  
  # 显示图形或返回ggplot对象
  if (show_plot) {
    print(p)
    return(invisible(p))
  } else {
    return(p)
  }
}

ppc_plot(hdi_sum=complete_hdi_sum)

# 将subject转换为因子型
df_first5_processed <- dplyr::mutate(
  df_first5,
  subject = as.factor(subject)
)

# 创建左侧箱线图：显示每个被试的反应时间分布
boxplot_plot <- ggplot2::ggplot(df_first5_processed, ggplot2::aes(x = subject, y = RT, fill = subject)) +
  ggplot2::geom_boxplot(alpha = 0.7) +
  ggplot2::scale_fill_brewer(palette = "Set2") +
  ggplot2::labs(
    title = "Reaction Time Distribution by Subject",
    x = "Subject ID",
    y = "Reaction Time (ms)"
  ) +
  papaja::theme_apa()

# 创建右侧核密度估计图：显示每个被试的反应时间密度分布
kde_plot <- ggplot2::ggplot(df_first5_processed, ggplot2::aes(x = RT, color = subject)) +
  ggplot2::geom_density(alpha = 0.5, common_norm = FALSE) +
  ggplot2::scale_color_brewer(palette = "Set2") +
  ggplot2::scale_fill_brewer(palette = "Set2") +
  ggplot2::labs(
    title = "Reaction Time Density by Subject",
    x = "Reaction Time (ms)",
    y = "Density"
  ) +
  papaja::theme_apa()

# 组合图形
boxplot_plot + kde_plot

prior_structure <- get_prior(
  formula = bf(
    log(RT) ~ 0 + subject,  # 均值模型：每个subject独立截距（mu）
    sigma ~ 0 + subject      # 标准差模型：每个subject独立标准差（sigma）
  ), 
  sigma ~ 0 + subject,
  data = df_first5,
  family = gaussian()
)
print(prior_structure)

df_first5 <- df_first5 %>% 
  dplyr::mutate(subject = as.factor(subject))

# 定义非池化模型
no_pooled_model <- brm(
  # log(RT)作为响应变量，每个subject有自己的截距
  formula = bf(
    log(RT) ~ 0 + subject,
    sigma ~ 0 + subject
  ),
  data = df_first5,
  family = gaussian(),
  # 为使模型收敛，此处不自定义先验
  chains = 4,
  iter = 2000,
  warmup = 1000,
  seed = 84735,
  control = list(adapt_delta = 0.95)
)

options(repr.plot.width = 15, repr.plot.height = 18)

# 从 brms 模型中提取后验样本
posterior_samples <- as.array(no_pooled_model)

# 选择要绘制的参数（以其中三个被试为例）
target_pars <- c(
  "b_subject66670",
  "b_subject80941",
  "b_subject81844",
  "b_sigma_subject66670",
  "b_sigma_subject80941",
  "b_sigma_subject81844")

# 绘制迹图（trace plot）
trace_plot <- bayesplot::mcmc_trace(
  posterior_samples,  # 后验样本数组
  pars = target_pars,  # 要绘制的参数
  facet_args = list(nrow = 6, ncol = 1)
) +
  ggplot2::labs(title = "Trace Plots") +
  papaja::theme_apa()

# 绘制后验分布图
dens_plot <- bayesplot::mcmc_dens(
  posterior_samples,
  pars = target_pars,
  facet_args = list(nrow = 6, ncol = 1),
) +
  ggplot2::labs(title = "Posterior Distributions") +
  papaja::theme_apa()


# 显示图形
trace_plot + dens_plot

summary(no_pooled_model)

options(repr.plot.width = 15, repr.plot.height = 6)

bayesplot::pp_check(no_pooled_model) + 
papaja::theme_apa()

# 生成后验预测
ppc <- brms::posterior_predict(no_pooled_model)

# 调用函数计算HDI
no_hdi_sum <- ppc_sum(ppc, data = df_first5, y = "RT")

# 查看结果
head(no_hdi_sum)

no_hdi_sum <- inv_log_hdi_sum(no_hdi_sum)

ppc_plot(hdi_sum=no_hdi_sum)

# 定义部分池化模型
partial_pooled_model <- brms::brm(
  log(RT) ~ 0 + (1|subject),
  data = df_first5,
  family = gaussian(),
  iter = 5000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  seed = 84735,
  control = list(adapt_delta = 0.95)  # 提高采样稳定性
)

summary(partial_pooled_model)

subject_mu <- brms::ranef(partial_pooled_model)$subject[, , "Intercept"]

hyperparams <- brms::VarCorr(partial_pooled_model)

cat("=== 每个被试的均值 ===\n")
print(subject_mu)
cat("\n=== 超参数 ===\n")
print(hyperparams)

options(repr.plot.width = 15, repr.plot.height = 18)

# 从 brms 模型中提取后验样本
posterior_samples <- as.array(partial_pooled_model)

target_pars <- c(
  # 每个被试的均值（随机截距）
  "r_subject[66670,Intercept]",
  "r_subject[80941,Intercept]",
  "r_subject[81844,Intercept]",
  "r_subject[83824,Intercept]",
  "r_subject[83956,Intercept]",
  "sd_subject__Intercept"
)

# 绘制迹图
trace_plot <- bayesplot::mcmc_trace(
  posterior_samples,
  pars = target_pars,
  facet_args = list(nrow = 6, ncol = 1)
) +
  papaja::theme_apa()

# 绘制后验分布图
dens_plot <- bayesplot::mcmc_dens(
  posterior_samples,
  pars = target_pars,
  facet_args = list(nrow = 6, ncol = 1)
) +
  papaja::theme_apa()

# 组合图形
trace_plot + dens_plot

options(repr.plot.width = 15, repr.plot.height = 6)
bayesplot::pp_check(partial_pooled_model) +
papaja::theme_apa()

ppc <- brms::posterior_predict(partial_pooled_model)
partial_hdi_sum <- ppc_sum(ppc, data = df_first5, y = "RT")
partial_hdi_sum <- inv_log_hdi_sum(partial_hdi_sum)

ppc_plot(hdi_sum=partial_hdi_sum)

options(repr.plot.width = 15, repr.plot.height = 15)

p1 <- ppc_plot(hdi_sum = complete_hdi_sum, show_plot = FALSE) +
  ggplot2::ggtitle("Complete pooling model") +
  papaja::theme_apa()

p2 <- ppc_plot(hdi_sum = partial_hdi_sum, show_plot = FALSE) +
  ggplot2::ggtitle("Partial pooling model") +
  papaja::theme_apa()


p3 <- ppc_plot(hdi_sum = no_hdi_sum, show_plot = FALSE) +
  ggplot2::ggtitle("No pooling model") +
  papaja::theme_apa()

p1 / p2 / p3

# 定义转换函数
inv_log_hdi_sum <- function(posterior_stats) {
  posterior_stats %>%
    dplyr::mutate(
      # 对所有数值列进行指数转换（逆对数）
      dplyr::across(where(is.numeric), exp)
    )
}

# 通用统计量计算函数
summarise_param <- function(data) {
  data %>%
    dplyr::summarise(
      mean = mean(value),
      sd = sd(value),
      q5 = quantile(value, 0.05),
      q95 = quantile(value, 0.95)
    )
}

# 提取模型参数
# ========== 完全池化模型========== 
complete_stats <- complete_pooled_model %>%
  # 提取截距（mu）
  tidybayes::spread_draws(Intercept) %>%  
  dplyr::rename(value = Intercept) %>%
  dplyr::mutate(
    param_type = "mu",
    subject = "All subjects"
  ) %>%
  summarise_param() %>%  # 通用统计函数
  inv_log_hdi_sum() %>%  # 逆对数转换
  dplyr::mutate(
    source = "Complete pool",
    param_type = "mu",
    subject = "All subjects"
  )

# ========== 部分池化模型 ==========
# 提取被试水平mu参数
partial_mu_stats <- partial_pooled_model %>%
  tidybayes::spread_draws(r_subject[subject_idx, Intercept]) %>%
  dplyr::rename(value = r_subject) %>%
  dplyr::mutate(
    param_type = "mu",
    subject = paste0("Subject_", subject_idx)
  ) %>%
  dplyr::group_by(subject) %>%
  summarise_param() %>% 
  inv_log_hdi_sum() %>%
  dplyr::mutate(
    source = "Partial pool",
    param_type = "mu"
  )

# 提取超参数sd_subject__Intercept
partial_hyper_stats <- partial_pooled_model %>%
  tidybayes::spread_draws(sd_subject__Intercept) %>%
  dplyr::rename(value = sd_subject__Intercept) %>%
  dplyr::mutate(
    param_type = "sd_subject__Intercept",  # 标记参数类型
    subject = "All subjects"  # 超参数属于全局（All subjects）
  ) %>%
  summarise_param() %>%
  inv_log_hdi_sum() %>%
  dplyr::mutate(
    source = "Partial pool",
    param_type = "mu",
    subject = "All subjects"
  )

# 合并部分池化模型的mu + 超参数
partial_stats <- dplyr::bind_rows(partial_mu_stats, partial_hyper_stats)

# ========== 无池化模型 ==========
no_stats <- no_pooled_model %>%
  tidybayes::gather_draws(`b_subject[0-9]+`, regex = TRUE) %>%
  dplyr::mutate(
    value = .value,
    param_type = "mu",
    subject = gsub("b_subject", "", .variable)
  ) %>%
  dplyr::group_by(subject) %>%
  summarise_param() %>%  # 通用统计函数
  inv_log_hdi_sum() %>%
  dplyr::mutate(
    source = "No pool",
    param_type = "mu"
  )

# ========== 合并所有结果 ==========
df_compare <- dplyr::bind_rows(
  complete_stats, partial_stats, no_stats) %>%
  dplyr::arrange(source, param_type, subject) %>%
  dplyr::select(
    source, param_type, subject,
    mean, sd, q5, q95
  )

print(df_compare)

# 提取模型参数
summarise_mu <- function(data) {
  data %>%
    dplyr::summarise(
      mean = mean(mu),
      lower = quantile(mu, 0.025),
      upper = quantile(mu, 0.975)
    )
}

# 提取完全池化模型
complete_mu <- complete_pooled_model %>%
  tidybayes::spread_draws(Intercept) %>%
  dplyr::rename(mu = Intercept) %>%
  summarise_mu() %>%
  dplyr::mutate(
    subject = "All subjects",
    model = "Complete Pooling"
  )

# 提取部分池化模型
partial_mu <- partial_pooled_model %>%
  tidybayes::spread_draws(r_subject[subject_idx, Intercept]) %>%
  dplyr::rename(mu = r_subject) %>%
  dplyr::mutate(subject = paste0("Subject_", subject_idx)) %>%
  dplyr::group_by(subject) %>%
  summarise_mu() %>%
  dplyr::mutate(model = "Partial Pooling")

partial_hyper <- partial_pooled_model %>%
  tidybayes::spread_draws(sd_subject__Intercept) %>%
  dplyr::rename(mu = sd_subject__Intercept) %>%
  summarise_mu() %>%
  dplyr::mutate(
    subject = "hyper_mu",
    model = "Partial Pooling",
  )

# 提取无池化模型
no_mu <- no_pooled_model %>%
  tidybayes::gather_draws(`b_subject[0-9]+`, regex = TRUE) %>%
  dplyr::mutate(subject = paste0("Subject_", gsub("b_subject", "", .variable))) %>%  # 统一被试名称格式
  dplyr::rename(mu = .value) %>%
  dplyr::group_by(subject) %>%
  summarise_mu() %>%
  dplyr::mutate(model = "No Pooling")

# 合并所有数据，统一被试顺序
all_subjects <- c(partial_mu$subject, "All subjects", "hyper_mu") %>% unique()  # 包含所有被试+完全池化的"All subjects"
all_mu <- dplyr::bind_rows(partial_mu, partial_hyper, no_mu, complete_mu) %>%
  dplyr::mutate(subject = factor(subject, levels = all_subjects))  # 统一被试顺序

all_mu

#  计算最大索引（即被试数量）
max_index <- all_mu %>% 
  dplyr::filter(model != "Complete Pooling") %>% 
  dplyr::pull(subject) %>% 
  unique() %>% 
  length()

# --------------------------
# 绘制森林图
# --------------------------
options(repr.plot.width = 15, repr.plot.height = 6)
plot_forest_ggplot <- function(data, title, max_index) { 
  data <- data %>%
    dplyr::arrange(subject) %>%  # 按统一的被试顺序排列
    dplyr::mutate(index = dplyr::row_number())
  ggplot2::ggplot(data, ggplot2::aes(y = index, x = mean, xmin = lower, xmax = upper)) +
    ggplot2::geom_point(size = 3, color = "#0686b9") +
    ggplot2::geom_errorbarh(height = 0.2, color = "#0686b9") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::scale_y_continuous(
      name = "", breaks = data$index, labels = data$subject, expand = c(0.02, 0.02),
      limits = c(0.5, max_index + 0.5)  # 强制y轴范围和前两个图一致，增加0.5的边距使点居中
    ) +
    ggplot2::scale_x_continuous(
      name = "mu", limits = c(3, 13), breaks = seq(3, 13, by = 1)
    ) +
    ggplot2::labs(title = title) +
    papaja::theme_apa()
}

p1 <- all_mu %>% 
  dplyr::filter(model == "Partial Pooling") %>% 
  plot_forest_ggplot("Partial Pooling", max_index) 

p2 <- all_mu %>% 
  dplyr::filter(model == "No Pooling") %>% 
  plot_forest_ggplot("No Pooling", max_index)

p3 <- all_mu %>% 
  dplyr::filter(model == "Complete Pooling") %>% 
  plot_forest_ggplot("Complete Pooling", max_index)

p1 + p2 + p3

# 检查并转换subject列类型
complete_hdi_sum <- complete_hdi_sum %>%
  dplyr::mutate(subject = as.factor(subject))  # 转换为因子型

no_hdi_sum <- no_hdi_sum %>%
  dplyr::mutate(subject = as.factor(subject))

partial_hdi_sum <- partial_hdi_sum %>%
  dplyr::mutate(subject = as.factor(subject))

df_first5 <- df_first5 %>%
  dplyr::mutate(subject = as.factor(subject))

# 合并三个模型的后验预测数据
combined_data <- bind_rows(
  complete_hdi_sum %>% dplyr::mutate(model = "Complete pooling"),
  no_hdi_sum %>% dplyr::mutate(model = "No pooling"),
  partial_hdi_sum %>% dplyr::mutate(model = "Partial pooling")
)

# 计算每个被试的数据量和x轴刻度位置
count_per_subject <- df_first5 %>%
  dplyr::group_by(subject) %>%
  dplyr::summarise(count = n())

cumulative_count <- cumsum(count_per_subject$count)
xtick <- cumulative_count - count_per_subject$count / 2
subject_labels <- count_per_subject$subject

# 绘图
ggplot2::ggplot(combined_data, aes(x = obs_id, y = mean, color = model)) +
  ggplot2::geom_point(alpha = 0.15, size = 4, shape = 16) +
  ggplot2::scale_color_manual(values = c("Complete pooling" = "#F8766D",
                                "No pooling" = "#00BFC4",
                                "Partial pooling" = "#7CAE00")) +
  ggplot2::scale_x_continuous(
    breaks = xtick,
    labels = subject_labels
  ) +
  ggplot2::labs(
    title = "Posterior mean of observed data",
    x = "Subject",
    y = "Posterior Mean",
    color = "Model"
  ) +
  papaja::theme_apa()

calculate_mae <- function(hdi_sum, obs = "y", pred = "mean") {
  observed_data <- hdi_sum[[obs]]
  posterior_predictive <- hdi_sum[[pred]]
  mae <- stats::median(abs(observed_data - posterior_predictive))
  return(mae)
}

comolete_mae <- calculate_mae(complete_hdi_sum)
no_mae <- calculate_mae(no_hdi_sum)
partial_mae <- calculate_mae(partial_hdi_sum)

# 比较结果
mae_comparison <- data.frame(
  model = c("Complete Model", "No Model", "Partial Model"),
  mae = c(comolete_mae, no_mae, partial_mae)
)

print(mae_comparison)

# 选择被试为"31727"且percentCoherence为5的数据
new_data <- df_raw %>%
  dplyr::filter(subject == 31727 & percentCoherence == 5)

# 建立索引 'subj_id' 和 'obs_id'
new_data <- new_data %>%
  dplyr::mutate(
    subj_id = subject,
    obs_id = dplyr::row_number()
  )

head(new_data)

# 拟合brms模型
partial_pooled_pred <- brms::brm(
  formula = log(RT) ~ 0 + (1 | subject),  # 1|subject表示随机截距，对应new_mu
  data = new_data,
  chains = 4,
  iter = 5000,
  warmup = 1000,
  seed = 84735,
)

# 进行后验预测（基于拟合的模型）
pred_trace <- brms::posterior_predict(
  partial_pooled_pred,
  newdata = new_data,
  seed = 84735
)

# 查看后验预测结果
# pred_trace是一个矩阵，行对应后验样本，列对应观测值
dim(pred_trace)  # 查看维度
head(pred_trace[, 1:5])  # 查看前5个观测值的前几个后验预测样本

inv_log_hdi_sum <- function(hdi_sum) {
  # 对均值、HDI下限和上限进行逆对数转换
  df <- dplyr::mutate(
    hdi_sum,
    mean = inv_log(mean, sd),  # 转换均值
    Q2.5 = inv_log(Q2.5, sd),   # 转换2.5%分位数（HDI下限）
    Q97.5 = inv_log(Q97.5, sd)  # 转换97.5%分位数（HDI上限）
  )
  
  return(df)
}

ppc <- brms::posterior_predict(partial_pooled_pred)
pred_hdi_sum = ppc_sum(ppc,data=new_data,y = "RT")
pred_hdi_sum = inv_log_hdi_sum(pred_hdi_sum)
ppc_plot(pred_hdi_sum)

# 安装和加载包
options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
if (!requireNamespace('pacman', quietly = TRUE)) {
    install.packages('pacman')
}
pacman::p_load("tidyverse","ggplot2", "dplyr","gridExtra","patchwork","papaja", "bayesplot",
               "rstan","bridgesampling", 'logspline', "easystats", "brms", "RColorBrewer") 
options(warn = -1)  # 抑制警告

# 导入数据
df_raw <- tryCatch({
  read.csv('/home/mw/input/bayes3797/Data_Sum_HPP_Multi_Site_Share.csv')
}, error = function(e) {
  read.csv('data/Data_Sum_HPP_Multi_Site_Share.csv')
})

df_raw[1:10, c("Site","scontrol")] # 显示前十行的数据

options(repr.plot.width = 15, repr.plot.height = 6)
plot <- ggplot2::ggplot(data = df_raw, aes(x = Site, y = scontrol, fill = Site)) +
  ggplot2::geom_boxplot() +
  ggplot2::theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +  # 旋转x轴标签
  papaja::theme_apa() 
# 显示图形
print(plot)

# 选取5个站点
first5_site <- c('Southampton', 'METU', 'Kassel', 'Tsinghua', 'Oslo')
df_first5 <- df_raw %>%
  dplyr::filter(Site %in% first5_site)

# 为site生成索引
df_first5 <- df_first5 %>%
  dplyr::mutate(
    site_idx = as.integer(factor(Site, levels = first5_site)) - 1,  # R因子从1开始，减1以匹配Python的0索引
    obs_id = dplyr::row_number() - 1  # R行号从1开始，减1以匹配Python的0索引
  )

# 设置索引，方便之后调用数据
df_first5 <- df_first5 %>%
  dplyr::mutate(
    Site = factor(Site, levels = first5_site)  # 确保Site按指定顺序排列
  ) %>%
  tibble::column_to_rownames(var = "obs_id")  # 将obs_id设为行名

# 查看前10行数据
head(df_first5, 10)

##--------------------------------------------------
#      提示：对...中的内容进行修改
#---------------------------------------------------

# 拟合完全池化模型，也可以尝试自定义先验~
complete_pooled_model <- brms::brm(
  formula = ...,  # 只包含截距项
  data = df_first5,
  family = gaussian(),
  # prior = ...,
  chains = 4,
  iter = 2000,
  warmup = 1000,
  seed = 84735,
  control = list(adapt_delta = 0.95)
)

# 查看采样结果
summary(...) # 填入拟合的模型

posterior_samples <- as.array(...) # 填入拟合的模型

bayesplot::mcmc_trace(
  posterior_samples,  # 后验样本数组
  pars = c("Intercept", "sigma"),  # 要绘制的参数
  facet_args = list(nrow = 2, ncol = 1)
) +
  ggplot2::labs(title = "Trace Plots") +
  papaja::theme_apa()

# 进行后验预测
bayesplot::pp_check(...) + # 填入拟合的模型
papaja::theme_apa()

##--------------------------------------------------
#      提示：直接运行即可
#---------------------------------------------------

# 定义函数，计算 95%hdi
ppc_sum <- function(ppc, data, y = "scontrol") {
  # 计算每个观测值的后验预测均值
  ppc_mean <- apply(ppc, 2, mean)
  
  # 计算每个观测值的后验预测标准差
  ppc_sd <- apply(ppc, 2, sd)
  
  # 计算每个观测值的95% HDI
  ppc_hdi <- t(apply(ppc, 2, function(x) {
    hdi_result <- bayestestR::hdi(x, ci = 0.95)
    # 直接提取 CI_low 和 CI_high 列
    return(c(hdi_result$CI_low, hdi_result$CI_high))
  }))
  
  # 设置HDI矩阵的列名
  colnames(ppc_hdi) <- c("Q2.5", "Q97.5")
  
  # 构建结果数据框
  hdi_sum <- base::data.frame(
    mean = ppc_mean,              # 后验预测均值
    sd = ppc_sd,                  # 后验预测标准差
    Q2.5 = ppc_hdi[, "Q2.5"],     # 95% HDI下限
    Q97.5 = ppc_hdi[, "Q97.5"],   # 95% HDI上限
    y = data[[y]],                # 原始观测值
    obs_id = 1:nrow(data),        # 观测ID
    Site = data[["Site"]]   # 站点ID
  )
  
  return(hdi_sum)
}

# 计算后验预测的 95%hdi
# 生成后验预测
ppc <- brms::posterior_predict(complete_pooled_model)

# 调用函数计算HDI
hdi_sum <- ppc_sum(ppc, data = df_first5, y = "scontrol")

# 查看结果
head(hdi_sum)

##--------------------------------------------------
#      提示：对...中的内容进行修改
#---------------------------------------------------

no_pooled_model <- brm(
  formula = ...,
  data = ...,
  family = gaussian(),
  # 为使模型收敛，此处j建议不自定义先验
  iter = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  seed = 84735,
  control = list(adapt_delta = 0.95)
)

summary(no_pooled_model)

##--------------------------------------------------
#      提示：对...中的内容进行修改
#---------------------------------------------------

options(repr.plot.width = 15, repr.plot.height = 20)

# 从 brms 模型中提取后验样本
posterior_samples <- as.array(...)

# 选择要绘制的参数（以其中三个站点为例）
target_pars <- c(
  "b_SiteSouthampton",
  "b_SiteMETU",
  "b_SiteKassel",
  "b_sigma_SiteSouthampton",
  "b_sigma_SiteMETU",
  "b_sigma_SiteKassel")

# 绘制迹图（trace plot）
bayesplot::mcmc_trace(
  posterior_samples,  # 后验样本数组
  pars = target_pars,  # 要绘制的参数
  facet_args = list(nrow = 6, ncol = 1)
) +
  ggplot2::labs(title = "Trace Plots") +
  papaja::theme_apa()

##--------------------------------------------------
#      提示：对...中的内容进行修改
#---------------------------------------------------

options(repr.plot.width = 15, repr.plot.height = 8)

bayesplot::pp_check(no_pooled_model) +
papaja::theme_apa()

# 生成后验预测
ppc <- brms::posterior_predict(...)

# 调用函数计算HDI
no_hdi_sum <- ppc_sum(ppc, data = df_first5)

# 查看结果
head(no_hdi_sum)

##--------------------------------------------------
#      提示：对...中的内容进行修改
#---------------------------------------------------

ppc_plot(hdi_sum=...)

# --------------------------------------------------
#      提示：对...中的内容进行修改
#---------------------------------------------------

# 定义部分池化模型
partial_pooled_model <- brms::brm(
  formula = ...,
  data = ...,
  family = gaussian(),
  iter = 5000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  seed = 84735,
  control = list(adapt_delta = 0.95)
)

##--------------------------------------------------
#      提示：对...中的内容进行修改
#---------------------------------------------------
summary(partial_pooled_model)

##--------------------------------------------------
#      提示：对...中的内容进行修改
#---------------------------------------------------

options(repr.plot.width = 15, repr.plot.height = 18)

# 从 brms 模型中提取后验样本
posterior_samples <- as.array(...)

target_pars <- c(
  # 每个被试的均值（随机截距）
  "r_Site[Southampton,Intercept]",
  "r_Site[METU,Intercept]",
  "r_Site[Kassel,Intercept]",
  "r_Site[Tsinghua,Intercept]",
  "r_Site[Oslo,Intercept]",
  "sd_Site__Intercept"
)

# 绘制迹图（调整nrow为参数数量）
bayesplot::mcmc_trace(
  posterior_samples,
  pars = target_pars,
  facet_args = list(nrow = 6, ncol = 1)
) +
  papaja::theme_apa()

##--------------------------------------------------
#      提示：对...中的内容进行修改
#---------------------------------------------------

options(repr.plot.width = 15, repr.plot.height = 6)
bayesplot::pp_check(...) 

##--------------------------------------------------
#      提示：对...中的内容进行修改
#---------------------------------------------------

ppc <- brms::posterior_predict(...)
partial_hdi_sum <- ppc_sum(ppc, data = df_first5, y = "scontrol")
ppc_plot(hdi_sum=partial_hdi_sum)