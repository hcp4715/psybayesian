# 安装和加载包
options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
if (!requireNamespace('pacman', quietly = TRUE)) {
    install.packages('pacman')
}
pacman::p_load("tidyverse","ggplot2", "dplyr","gridExtra","patchwork","papaja", "bayesplot",
               "rstan","bridgesampling", 'logspline', "easystats", "brms", "RColorBrewer","loo") 
options(warn = -1)  # 抑制警告

# 导入数据
df_raw <- tryCatch({
  read.csv('/home/mw/input/bayes3797/Data_Sum_HPP_Multi_Site_Share.csv')
}, error = function(e) {
  read.csv('data/Data_Sum_HPP_Multi_Site_Share.csv')
})

# 选取5个站点
first5_site <- c('Southampton','Portugal','Kassel','Tsinghua','UCSB') 
df_first5 <- df_raw %>%
  dplyr::filter(Site %in% first5_site)

# 为site生成索引 + 生成obs_id列
df_first5 <- df_first5 %>%
  dplyr::mutate(
    site_idx = as.integer(factor(Site, levels = first5_site)) - 1, 
    obs_id = dplyr::row_number() - 1  # 保留为列
  ) %>%
  dplyr::mutate(
    Site = factor(Site, levels = first5_site)  # 确保Site按指定顺序排列
  )

# 查看前5行数据
head(df_first5, 5)

ggplot2::ggplot(data = df_first5, aes(x = stress, y = scontrol)) +
  # 散点层
  ggplot2::geom_point(alpha = 0.6, size = 2) +
  # 线性回归拟合层
  ggplot2::geom_smooth(method = "lm", se = TRUE, color = "#0072B2", fill = "#56B4E9") +
  papaja::theme_apa()

# 构建完全池化模型
complete_pooled_model <- brms::brm(
  scontrol ~ 1 + stress,
  data = df_first5,
  family = gaussian(),
  iter = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  seed = 84735,
  control = list(adapt_delta = 0.95)
)

summary(complete_pooled_model)

# 定义函数，用来按站点绘制后验预测回归线
# 获取站点列表
get_group_index <- function(data) {
  # 直接返回排序后的站点唯一值
  groups <- unique(data$Site)
  return(groups)
}

# 按站点绘制后验预测回归线
plot_regression <- function(data, model, groups) {
  # 提取后验预测的期望值（mu），返回每个观测的后验样本
  posterior_mu <- brms::posterior_epred(model, summary = FALSE)
  
  # 将后验预测值与原始数据合并（按行匹配）
  mu_df <- as.data.frame(posterior_mu) %>%
    tidyr::pivot_longer(cols = everything(), names_to = "row_idx", values_to = "mu") %>%
    dplyr::mutate(row_idx = as.integer(gsub("V", "", row_idx))) %>%
    dplyr::group_by(row_idx) %>%
    dplyr::summarise(
      mu_mean = mean(mu),
      mu_hdi_lower = bayestestR::hdi(mu, ci = 0.95)$CI_low,
      mu_hdi_upper = bayestestR::hdi(mu, ci = 0.95)$CI_high
    ) %>%
    dplyr::bind_cols(data %>% dplyr::mutate(row_idx = dplyr::row_number()))
  
  # 定义变量名
  x_var <- "stress"
  y_var <- "scontrol"
  site_var <- "Site"
  n_groups <- length(groups)
  
  site_colors <- RColorBrewer::brewer.pal(n_groups, "Set1")

  plot_list <- list()
  
  # 按站点循环绘图
  for (i in seq_along(groups)) {
    group <- groups[i]
    # 筛选当前站点的数据
    group_data <- mu_df %>%
      dplyr::filter(!!sym(site_var) == group)
    
    # 绘制当前站点的图形
    p <- ggplot2::ggplot(group_data, aes(x = !!sym(x_var), y = !!sym(y_var))) +
      # 原始数据散点
      ggplot2::geom_point(color = site_colors[i], alpha = 0.5) +
      # 后验均值回归线
      ggplot2::geom_line(
        aes(y = mu_mean), color = site_colors[i], alpha = 0.5) +
      # 95%HDI填充带
      ggplot2::geom_ribbon(aes(ymin = mu_hdi_lower, ymax = mu_hdi_upper),
        fill = site_colors[i], alpha = 0.25) +
      # 站点名称作为标题
      ggplot2::ggtitle(label = group) +
      # 统一坐标轴范围
      ggplot2::xlim(range(mu_df[[x_var]])) +
      ggplot2::ylim(range(mu_df[[y_var]])) +
      papaja::theme_apa() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank()
      )
    
    plot_list[[i]] <- p
  }
  
  combined_plot <- patchwork::wrap_plots(plot_list, nrow = 1) +
    patchwork::plot_annotation(
      title = "Posterior regression models",
      theme = theme(plot.title = element_text(hjust = 0.5, size = 15))
    )
  
  # 打印图形并添加全局标签
  print(combined_plot)
  grid::grid.text("Stress", x = 0.5, y = 0.02, gp = grid::gpar(fontsize = 12))
  grid::grid.text("Self control", x = 0.02, y = 0.5, rot = 90, gp = grid::gpar(fontsize = 12))
}

options(repr.plot.width = 15, repr.plot.height = 6)

# 获取站点列表
groups <- get_group_index(df_first5)

# 进行可视化
plot_regression(data = df_first5, model = complete_pooled_model, groups = groups)

prior_list <- c(
  brms::prior(normal(40, 20), class = Intercept),
  brms::prior(normal(0, 5), class = b, coef = stress),
  brms::prior(exponential(1), class = sd, group = Site, coef = Intercept),
  brms::prior(exponential(1), class = sigma)
)

var_inter_model <- brms::brm(
    scontrol ~ stress + (1 | Site), # 仅随机截距
    data = df_first5,
    family = gaussian(),
    prior = prior_list,
    iter = 5000,          # 模型变复杂之后加大采样量
    warmup = 1000,
    chains = 4,
    seed = 84735,
    control = list(adapt_delta = 0.95)
  )

prior_only_model <- brms::brm(
  formula = scontrol ~ stress + (1 | Site),
  data = df_first5,
  family = gaussian(),
  prior = prior_list,
  sample_prior = "only",  # 仅从先验采样，不拟合数据
  iter = 1000,  # 仅先验采样，可减少采样数量
  chains = 4,
  seed = 84735
)

# 定义函数：绘制分站点的先验预测回归线集合
plot_prior <- function(prior_only_model, data) {
  # 提取先验预测的mu值
  prior_mu_samples <- brms::posterior_linpred(
    prior_only_model,
    newdata = data,
    draws = 200,
    return_matrix = TRUE  # 强制返回矩阵，方便处理
  )
  
  # 给矩阵添加行名（先验样本ID）和列名（观测值ID）
  rownames(prior_mu_samples) <- paste0("sample_", 1:nrow(prior_mu_samples))
  colnames(prior_mu_samples) <- paste0("obs_", 1:ncol(prior_mu_samples))
  
  prior_mu_df <- as.data.frame(prior_mu_samples) %>%
    # 添加先验样本ID列
    tibble::rownames_to_column("sample_id") %>%
    # 转长格式
    tidyr::pivot_longer(
      cols = -sample_id,
      names_to = "obs_id",
      values_to = "prior_mu"
    ) %>%
    # 提取观测值的数字索引
    dplyr::mutate(
      obs_idx = as.integer(gsub("obs_", "", obs_id)),
      sample_id = factor(sample_id)  # 转为因子，方便分组
    ) %>%
    # 匹配原始数据的Site、stress
    dplyr::left_join(
      data %>% dplyr::mutate(obs_idx = dplyr::row_number()),
      by = "obs_idx"
    ) %>%
    # 按Site和sample_id分组，对stress排序
    dplyr::group_by(Site, sample_id) %>%
    dplyr::arrange(stress, .by_group = TRUE) %>%
    dplyr::ungroup()
  
  # 分站点绘图
  p <- ggplot2::ggplot(prior_mu_df, aes(x = stress, y = prior_mu, group = sample_id)) +
    # 绘制多条先验预测回归线
    ggplot2::geom_line(color = "gray50", alpha = 0.3) +

    ggplot2::facet_wrap(~ Site, nrow = 1, scales = "fixed") +
    ggplot2::labs(x = "Stress", y = "Self control", title = "Prior regression models"
    ) +
    papaja::theme_apa() 
    
  print(p)
  return(invisible(p))
}

options(repr.plot.width = 15, repr.plot.height = 6)
# 调用绘图函数
plot_prior(
  prior_only_model = prior_only_model,
  data = df_first5
)

summary(var_inter_model)

site_coef <- as.data.frame(stats::coef(var_inter_model))

#整理数据
coef_df <- as.data.frame(summary(var_inter_model)$fixed)

plot_hdi <- site_coef %>%
  select(1:5) %>%
  rename(Intercept = 1, 
         Intercept_error = 2, 
         Intercept_Q2.5 = 3, 
         Intercept_Q97.5 = 4) %>%
  bind_rows(
    data.frame(Intercept = coef_df$Estimate, Intercept_error = coef_df$Est.Error, 
               Intercept_Q2.5 = coef_df$`l-95% CI`, Intercept_Q97.5 = coef_df$`u-95% CI`) %>%
      slice(1)
  )
rownames(plot_hdi)[nrow(plot_hdi)] <- "beta0"

plot_hdi <- plot_hdi %>%
  rownames_to_column("site")
plot_hdi

#可视化
ggplot2::ggplot(plot_hdi, aes(x = Intercept, y = site)) +
  # 绘制HDI区间
  ggplot2::geom_errorbarh(
    aes(xmin = plot_hdi$Intercept_Q2.5, xmax = plot_hdi$Intercept_Q97.5),
    height = 0.2, linewidth = 1, color = "steelblue"
  ) +
  # 绘制后验均值点
  ggplot2::geom_point(
    size = 4, color = "white", stroke = 1.2, shape = 21, fill = "steelblue"
  ) +
  papaja::theme_apa()+
  ggplot2::labs(title = "95% HDI")

# 获取站点列表
groups <- get_group_index(df_first5)

# 进行可视化
p1 <- plot_regression(data = df_first5, model = var_inter_model, groups = groups)
p1

# 提取随机效应的变异信息
model_var <- brms::VarCorr(var_inter_model)

# 提取组间变异
sd_intercept <- model_var$Site$sd[1]  
between_var_intercept <- as.numeric(sd_intercept)^2

# 提取组内变异（从后验样本计算）
sigma_samples <- brms::posterior_samples(var_inter_model, pars = "sigma")
residual_sd <- mean(sigma_samples$sigma)
within_var <- residual_sd^2

# 计算变异解释比例
total_var_intercept <- between_var_intercept + within_var
prop_intercept_between <- (between_var_intercept / total_var_intercept) * 100  # 组间解释百分比
prop_intercept_within <- (within_var / total_var_intercept) * 100            # 组内解释百分比

cat("组间方差：", round(between_var_intercept, 4), "\n")
cat("组内方差：", round(within_var, 4), "\n")
cat("被组间方差所解释的部分：", round(prop_intercept_between, 2), "%\n")
cat("被组内方差所解释的部分：", round(prop_intercept_within, 2), "%\n\n")

var_slope_model <- brms::brm(
  scontrol ~ 1 + stress + (0 + stress | Site), # 仅随机斜率
  data = df_first5,
  family = gaussian(),
  iter = 5000,
  warmup = 1000,
  chains = 4,
  seed = 84735,
  control = list(adapt_delta = 0.95)
)

summary(var_slope_model)

site_coef2 <- as.data.frame(stats::coef(var_slope_model))

#整理数据
coef_df2 <- as.data.frame(summary(var_slope_model)$fixed)

plot_hdi2 <- site_coef2 %>%
  select(1:5) %>%
  rename(Slope = 1, 
         Slope_error = 2, 
         Slope_Q2.5 = 3, 
         Slope_Q97.5 = 4) %>%
  bind_rows(
    data.frame(Slope = coef_df$Estimate, Slope_error = coef_df$Est.Error, 
               Slope_Q2.5 = coef_df$`l-95% CI`, Slope_Q97.5 = coef_df$`u-95% CI`) %>%
      slice(2)
  )
rownames(plot_hdi2)[nrow(plot_hdi2)] <- "beta1"

plot_hdi2 <- plot_hdi2 %>%
  rownames_to_column("site")
plot_hdi2

# 可视化
ggplot2::ggplot(plot_hdi2, aes(x = Slope, y = site)) +
  # 绘制HDI区间
  ggplot2::geom_errorbarh(
    aes(xmin = plot_hdi2$Slope_Q2.5, xmax = plot_hdi2$Slope_Q97.5),
    height = 0.2, linewidth = 1, color = "steelblue"
  ) +
  # 绘制后验均值点
  ggplot2::geom_point(
    size = 4, color = "white", stroke = 1.2, shape = 21, fill = "steelblue"
  ) +
  papaja::theme_apa()+
  ggplot2::labs(title = "95% HDI")

  # 可视化
ggplot2::ggplot(plot_hdi2, aes(x = Slope, y = site)) +
  # 绘制HDI区间
  ggplot2::geom_errorbarh(
    aes(xmin = plot_hdi2$Slope_Q2.5, xmax = plot_hdi2$Slope_Q97.5),
    height = 0.2, linewidth = 1, color = "steelblue"
  ) +
  # 绘制后验均值点
  ggplot2::geom_point(
    size = 4, color = "white", stroke = 1.2, shape = 21, fill = "steelblue"
  ) +
  papaja::theme_apa()+
  ggplot2::labs(title = "95% HDI")

  # 获取站点列表
groups <- get_group_index(df_first5)

# 进行可视化
p2 <- plot_regression(data = df_first5, model = var_slope_model, groups = groups)
p2

# 提取随机效应的变异信息
model_var <- brms::VarCorr(var_slope_model)

# 提取组间变异
sd_slope <- model_var$Site$sd[2]      
between_var_slope <- as.numeric(sd_slope)^2

# 3. 提取组内变异（从后验样本计算）
sigma_samples <- brms::posterior_samples(var_inter_model, pars = "sigma")
residual_sd <- mean(sigma_samples$sigma)
within_var <- residual_sd^2

# # 计算变异解释比例
total_var_slope <- between_var_slope + within_var
prop_slope_between <- (between_var_slope / total_var_slope) * 100            # 组间解释百分比
prop_slope_within <- (within_var / total_var_slope) * 100                    # 组内解释百分比


cat("组间方差：", round(between_var_slope, 4), "\n")
cat("组内方差：", round(within_var, 4), "\n")
cat("被组间方差所解释的部分：", round(prop_slope_between, 2), "%\n")
cat("被组内方差所解释的部分：", round(prop_slope_within, 2), "%\n\n")

var_both_model <- brms::brm(
  scontrol ~ 1 + stress + (stress | Site), # 随机截距 + 随机斜率
  data = df_first5,
  family = gaussian(),
  iter = 5000,
  warmup = 1000,
  chains = 4,
  seed = 84735,
  control = list(adapt_delta = 0.95)
)

summary(var_both_model)

site_coef3 <- as.data.frame(stats::coef(var_both_model))

#整理数据
coef_df3 <- as.data.frame(summary(var_both_model)$fixed)
#截距
Intercept_hdi <- site_coef3 %>%
  dplyr::select(1:4) %>%
  dplyr::rename(Intercept = 1, 
         Intercept_error = 2, 
         Intercept_Q2.5 = 3, 
         Intercept_Q97.5 = 4) %>%
  dplyr::bind_rows(
    data.frame(Intercept = coef_df3$Estimate, Intercept_error = coef_df3$Est.Error, 
               Intercept_Q2.5 = coef_df3$`l-95% CI`, Intercept_Q97.5 = coef_df3$`u-95% CI`) %>%
      slice(1)
  )
rownames(Intercept_hdi)[nrow(Intercept_hdi)] <- "beta0"
Intercept_hdi <- Intercept_hdi %>%
  tibble::rownames_to_column("site")

#斜率
Slope_hdi <- site_coef3 %>%
  dplyr::select(5:8) %>%
  dplyr::rename(slope = 1, 
         slope_error = 2, 
         slope_Q2.5 = 3, 
         slope_Q97.5 = 4) %>%
  dplyr::bind_rows(
    data.frame(slope = coef_df3$Estimate, slope_error = coef_df3$Est.Error, 
               slope_Q2.5 = coef_df3$`l-95% CI`, slope_Q97.5 = coef_df3$`u-95% CI`) %>%
      slice(2)
  )
rownames(Slope_hdi)[nrow(Slope_hdi)] <- "beta1"
Slope_hdi <- Slope_hdi %>%
  tibble::rownames_to_column("site")

coef_combined <- Intercept_hdi %>%
  # 按site列内连接Slope_hdi
  dplyr::inner_join(Slope_hdi, by = "site") # %>%

# 查看合并后的结果
print(coef_combined)

# 可视化
options(repr.plot.width = 15, repr.plot.height = 6)

int_plot <- ggplot2::ggplot(Intercept_hdi, aes(x = Intercept, y = site)) +
  # 绘制HDI区间
  ggplot2::geom_errorbarh(
    aes(xmin = Intercept_hdi$Intercept_Q2.5, xmax = Intercept_hdi$Intercept_Q97.5),
    height = 0.2, linewidth = 1, color = "steelblue"
  ) +
  # 绘制后验均值点
  ggplot2::geom_point(
    size = 4, color = "white", stroke = 1.2, shape = 21, fill = "steelblue"
  ) +
  papaja::theme_apa()+
  ggplot2::labs(title = "95% HDI")

slop_plot <- ggplot2::ggplot(Slope_hdi, aes(x = slope, y = site)) +
  # 绘制HDI区间
  ggplot2::geom_errorbarh(
    aes(xmin = Slope_hdi$slope_Q2.5, xmax = Slope_hdi$slope_Q97.5),
    height = 0.2, linewidth = 1, color = "steelblue"
  ) +
  # 绘制后验均值点
  ggplot2::geom_point(
    size = 4, color = "white", stroke = 1.2, shape = 21, fill = "steelblue"
  ) +
  papaja::theme_apa()+
  ggplot2::labs(title = "95% HDI")

int_plot + slop_plot

# 获取站点列表
groups <- get_group_index(df_first5)

# 进行可视化
p3 <- plot_regression(data = df_first5, model = var_both_model, groups = groups)
p3

# 提取随机效应的变异信息
model_var <- brms::VarCorr(var_both_model)

# 提取组间变异
sd_intercept <- model_var$Site$sd[1]  
between_var_intercept <- as.numeric(sd_intercept)^2

sd_slope <- model_var$Site$sd[2]      
between_var_slope <- as.numeric(sd_slope)^2

# 提取组内变异
sigma_samples <- brms::posterior_samples(var_both_model, pars = "sigma")
residual_sd <- mean(sigma_samples$sigma)
within_var <- residual_sd^2

# 计算截距的变异解释比例
total_var_intercept <- between_var_intercept + within_var
prop_intercept_between <- (between_var_intercept / total_var_intercept) * 100  # 组间解释百分比
prop_intercept_within <- (within_var / total_var_intercept) * 100            # 组内解释百分比

# 计算斜率的变异解释比例
total_var_slope <- between_var_slope + within_var
prop_slope_between <- (between_var_slope / total_var_slope) * 100            # 组间解释百分比
prop_slope_within <- (within_var / total_var_slope) * 100                    # 组内解释百分比

# 打印结果
cat("=== 截距的变异解释比例 ===\n")
cat("截距的组间方差：", round(between_var_intercept, 4), "\n")
cat("组内方差：", round(within_var, 4), "\n")
cat("截距的组间解释比例：", round(prop_intercept_between, 2), "%\n")
cat("截距的组内解释比例：", round(prop_intercept_within, 2), "%\n\n")

cat("=== 斜率的变异解释比例 ===\n")
cat("斜率的组间方差：", round(between_var_slope, 4), "\n")
cat("组内方差：", round(within_var, 4), "\n")
cat("斜率的组间解释比例：", round(prop_slope_between, 2), "%\n")
cat("斜率的组内解释比例：", round(prop_slope_within, 2), "%\n")

# 对完全池化模型进行后验预测
complete_ppc <- posterior_predict(
  object = complete_pooled_model,
  seed = 84735
)

# 对仅截距随机模型进行后验预测
var_inter_ppc <- posterior_predict(
  object = var_inter_model,
  seed = 84735
)

# 对仅斜率随机模型进行后验预测
var_slope_ppc <- posterior_predict(
  object = var_slope_model,
  seed = 84735
)

# 对截距+斜率随机模型进行后验预测
var_both_ppc <- posterior_predict(
  object = var_both_model,
  seed = 84735
)

# 定义计算MAE的函数
calculate_MAE <- function(model_ppc, original_data, y_var = "scontrol") {
  # 计算每个观测的后验预测均值
  pre_y_mean <- colMeans(model_ppc)
  
  # 构建数据框：预测均值 + 原始观测值
  MAE_df <- data.frame(
    scontrol_ppc_mean = pre_y_mean,
    scontrol_original = original_data[[y_var]]  # 提取指定的响应变量观测值
  )
  
  # 计算绝对预测误差
  MAE_df <- MAE_df %>%
    mutate(pre_error = abs(scontrol_original - scontrol_ppc_mean))
  
  # 计算误差的中位数
  MAE_value <- median(MAE_df$pre_error, na.rm = TRUE)
  
  return(MAE_value)
}

complete_pooled_mae <- calculate_MAE(complete_ppc, complete_pooled_model$data, "scontrol")
var_inter_mae <- calculate_MAE(var_inter_ppc, var_inter_model$data, "scontrol")
var_slope_mae <- calculate_MAE(var_slope_ppc, var_slope_model$data, "scontrol")
var_both_mae <- calculate_MAE(var_both_ppc, var_both_model$data, "scontrol")

# 汇总所有模型的MAE
mae_summary <- data.frame(
  model = c("complete_pooled", "var_inter", "var_slope", "var_both"),
  mae = c(complete_pooled_mae, var_inter_mae, var_slope_mae, var_both_mae)
)
print(mae_summary)

# 计算各模型的LOO
loo0 <- loo::loo(complete_pooled_model)
loo1 <- loo::loo(var_inter_model)
loo2 <- loo::loo(var_slope_model)
loo3 <- loo::loo(var_both_model)

# 比较模型的LOO
loo::loo_compare(loo0, loo1, loo2, loo3)

# 生成Zurich站点数据并添加索引
new_group <- df_raw %>%
  dplyr::filter(Site == "Zurich") %>%
  dplyr::mutate(
    obs_id = 0:(dplyr::n() - 1),
    site_idx = as.integer(factor(Site)) - 1
  )

hier_pred_model <- brms::brm(
  scontrol ~ 1 + stress + (1 + stress | Site),
  data = new_group,
  family = gaussian(),
  chains = 4,
  iter = 5000,
  warmup = 1000,
  seed = 84735
)

summary(hier_pred_model)

# 构造新数据：stress=40，包含所有站点
new_data <- expand.grid(
  stress = 40,
  Site = base::unique(df_first5$Site)
)

# 生成后验预测值（后验样本×观测值）
post_pred <- brms::posterior_predict(
  object = var_both_model,
  newdata = new_data,
  seed = 84735
)

pred_old <- as.data.frame(post_pred)
colnames(pred_old) <- new_data$Site

# 整理结果
scontrol_by_site <- data.frame(
  Site = new_data$Site,
  mean_pred = colMeans(post_pred),
  l95 = apply(post_pred, 2, quantile, 0.025),
  u95 = apply(post_pred, 2, quantile, 0.975)
)

# 构造新站点新数据（stress=40）
new_data <- expand.grid(
  stress = 40,
  Site = base::unique(new_group$Site)
)

# 生成后验预测值（后验样本×观测值）
post_pred_new <- brms::posterior_predict(
  object = hier_pred_model,
  newdata = new_data,
  seed = 84735
)

pred_new <- as.data.frame(post_pred_new)
colnames(pred_new) <- new_data$Site

# 整理结果
scontrol_by_newSite <- data.frame(
  Site = new_data$Site,
  mean_pred = colMeans(post_pred_new),
  l95 = apply(post_pred_new, 2, quantile, 0.025),
  u95 = apply(post_pred_new, 2, quantile, 0.975)
)

# 合并数据
result_combined <- dplyr::bind_rows(scontrol_by_site, scontrol_by_newSite)

result_combined

# 合并绘图数据
pred_combined <- dplyr::bind_cols(pred_old, pred_new)

# 转换为长格式
pred_long <- pred_combined %>%
  dplyr::mutate(sample_id = dplyr::row_number()) %>%
  tidyr::pivot_longer(
    cols = -sample_id,
    names_to = "Site",
    values_to = "scontrol_pred"
  ) %>%
  dplyr::select(-sample_id)

# 进行可视化
p <- ggplot2::ggplot(pred_long, ggplot2::aes(x = scontrol_pred)) +
  ggplot2::geom_density(fill = "#3498db", alpha = 0.5, adjust = 1.2) +
  ggplot2::facet_grid(Site ~ ., scales = "free_y") +
  ggplot2::scale_x_continuous(limits = c(20, 60), expand = c(0, 0)) +
  ggplot2::labs(
    x = "Self Control",
    title = "Posterior predictive models for Self Control(X = 40)"
  ) +
  papaja::theme_apa()
p

# 选择变量并重置索引
df_temp <- df_first5 %>%
  dplyr::select(site_idx, obs_id, Site, stress, scontrol) %>%
  tibble::remove_rownames()

# 定义站点-气温映射
levels <- base::unique(df_temp$Site)
level_mapping <- base::c(17.6, 24.8, 19.9, 30.3, 19)
names(level_mapping) <- levels

# 添加气温列
df_temp <- df_temp %>%
  dplyr::mutate(
    avetemp = dplyr::recode(Site, !!!level_mapping)
  )

# 查看结果
df_temp[1:5, ]

# 按站点分组求均值
df_grouped <- df_temp %>%
  dplyr::group_by(Site) %>%
  dplyr::summarise(
    avetemp = base::mean(avetemp, na.rm = TRUE),
    scontrol = base::mean(scontrol, na.rm = TRUE),
    .groups = "drop"
  )

# 绘制回归散点图
p <- ggplot2::ggplot(df_grouped, ggplot2::aes(x = avetemp, y = scontrol)) +
  # 绘制散点
  ggplot2::geom_point(
    size = 3,
    color = "#6BAED6"
  ) +
  # 绘制线性回归拟合线
  ggplot2::geom_smooth(
    method = "lm",
    color = "#2171B5",
    linewidth = 1.2,
  ) +
  papaja::theme_apa()

# 显示图形
p

group_pred_model <- brms::brm(
  scontrol ~ 1 + stress + avetemp + (1 | Site),
  data = df_temp,
  family = gaussian(),
  iter = 2000,
  warmup = 1000,
  chains = 4,
  cores = 4,
  seed = 84735
)

summary(group_pred_model)

#===========================================
# 练习

# 安装和加载包
options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
if (!requireNamespace('pacman', quietly = TRUE)) {
  install.packages('pacman')
}
pacman::p_load("tidyverse","ggplot2", "dplyr","gridExtra","patchwork","papaja", "bayesplot",
               "rstan","bridgesampling", 'logspline', "easystats", "brms", "RColorBrewer", "loo") 
options(warn = -1)  # 抑制警告

# 导入示例数据
df_raw <- tryCatch({
  read.csv('/home/mw/input/bayes3797/evans2020JExpPsycholLearn_exp1_full_data.csv')
}, error = function(e) {
  read.csv('data/evans2020JExpPsycholLearn_exp1_full_data.csv')
})
df_raw[1:5, c("subject", "RT")] # 显示前五行的数据

# 筛选出特定被试并创建索引
df_first5 <- df_raw %>%
  dplyr::filter(
    subject %in% c(81844, 83956, 83824, 66670, 80941) ,
    percentCoherence %in% c(5, 10) # 筛选特定被试
  ) %>%
  dplyr::group_by(subject) %>%
  dplyr::mutate(
    subj_id = subject,
    obs_id = dplyr::row_number(),
    log_RTs = log(RT),
    global_id = 1 : n()
  ) %>%
  dplyr::ungroup()

# 查看数据
head(df_first5)

# =================================================
#                    补充...部分
# =================================================

df_first5 <- df_first5 %>% 
  dplyr::mutate(subject = as.factor(subject))

# Model1
var_inter_model <- brms::brm(
  # 模型公式
  formula = ...,
  # 数据
  data = ...,
  # 先验分布设置
  prior = c(
    prior(normal(7, 1), class = Intercept),        # beta0 
    prior(normal(7.5, 5), class = b, coef = percentCoherence), #beta1
    prior(exponential(1), class = sd, group = subject, coef = Intercept), # beta0_sigma           #beta_0_sigma
    prior(exponential(1), class = sigma)           #sigma_y
  ),
  chains = 4,
  iter = 2000,
  warmup = 1000,
  seed = 84735,
  control = list(adapt_delta = 0.95)
)

# =================================================
#                    补充...部分
# =================================================

# 定义先验模型
prior_only_model1 <- brms::brm(
  formula = ...,
  data = ...,
  prior = c(
    prior(normal(7, 1), class = Intercept),        
    prior(normal(7.5, 5), class = b, coef = percentCoherence), 
    prior(exponential(1), class = sd, group = subject, coef = Intercept),             #beta_0_sigma
    prior(exponential(1), class = sigma)  
  ),
  chains = 4,
  iter = 1000,
  seed = 84735,
  sample_prior = "only"
)
# 提取先验样本
# 从先验模型中生成预测值
prior_samples1 <- brms::posterior_predict(
  prior_only_model1,
  newdata = df_first5,
  draws = 1000
)
summary(prior_only_model1)

# 定义函数：绘制分站点的先验预测回归线集合
plot_prior1 <- function(prior_only_model, data) {
  # 提取先验预测的mu值
  prior_mu_samples <- brms::posterior_linpred(
    prior_only_model,
    newdata = data,
    draws = 200,
    return_matrix = TRUE  # 强制返回矩阵，方便处理
  )
  
  # 给矩阵添加行名（先验样本ID）和列名（观测值ID）
  rownames(prior_mu_samples) <- paste0("sample_", 1:nrow(prior_mu_samples))
  colnames(prior_mu_samples) <- paste0("obs_", 1:ncol(prior_mu_samples))
  
  prior_mu_df <- as.data.frame(prior_mu_samples) %>%
    # 添加先验样本ID列
    tibble::rownames_to_column("sample_id") %>%
    # 转长格式
    tidyr::pivot_longer(
      cols = -sample_id,
      names_to = "obs_id",
      values_to = "prior_mu"
    ) %>%
    # 提取观测值的数字索引
    dplyr::mutate(
      obs_idx = as.integer(gsub("obs_", "", obs_id)),
      sample_id = factor(sample_id)  # 转为因子，方便分组
    ) %>%
    # 匹配原始数据
    dplyr::left_join(
      data %>% dplyr::mutate(obs_idx = dplyr::row_number()),
      by = "obs_idx"
    ) %>%

    dplyr::group_by(subject, sample_id) %>%
    dplyr::arrange(percentCoherence, .by_group = TRUE) %>%
    dplyr::ungroup()
  
  # 分站点绘图
  p <- ggplot2::ggplot(prior_mu_df, aes(x = percentCoherence, y = prior_mu, group = sample_id)) +
    # 绘制多条先验预测回归线
    ggplot2::geom_line(color = "gray50", alpha = 0.3) +
    
    ggplot2::facet_wrap(~ subject, nrow = 1, scales = "fixed") +
    ggplot2::labs(x = "percentCoherence", y = "RT", title = "Prior regression models"
    ) +
    ggplot2::coord_cartesian(xlim = c(5, 10), ylim = c(-250, 100)) +
    papaja::theme_apa() 
  
  print(p)
  return(invisible(p))
}

# =================================================
#                    补充...部分
# =================================================
options(repr.plot.width = 15, repr.plot.height = 6)
# 调用绘图函数
plot_prior1(
  prior_only_model = ...,
  data = ...
)

##MCMC采样&后验参数估计
subject_coef <- as.data.frame(stats::coef(var_inter_model))

#整理数据
coef_df <- as.data.frame(summary(var_inter_model)$fixed)

plot_hdi <- subject_coef %>%
  select(1:5) %>%
  rename(Intercept = 1, 
         Intercept_error = 2, 
         Intercept_Q2.5 = 3, 
         Intercept_Q97.5 = 4) %>%
  bind_rows(
    data.frame(Intercept = coef_df$Estimate, Intercept_error = coef_df$Est.Error, 
               Intercept_Q2.5 = coef_df$`l-95% CI`, Intercept_Q97.5 = coef_df$`u-95% CI`) %>%
      slice(1)
  )
rownames(plot_hdi)[nrow(plot_hdi)] <- "beta0"

plot_hdi <- plot_hdi %>%
  rownames_to_column("subject") %>%
  mutate(subject = factor(subject, levels = subject))

#可视化
# =================================================
#                    补充...部分
# =================================================

ggplot2::ggplot(plot_hdi, aes(x = ..., y = ...)) +
  # 绘制HDI区间
  ggplot2::geom_errorbarh(
    aes(xmin = plot_hdi$Intercept_Q2.5, xmax = plot_hdi$Intercept_Q97.5),
    height = 0.2, linewidth = 1, color = "steelblue"
  ) +
  # 绘制后验均值点
  ggplot2::geom_point(
    size = 4, color = "white", stroke = 1.2, shape = 21, fill = "steelblue"
  ) +
  papaja::theme_apa()+
  ggplot2::labs(title = "95% HDI")

  #使用brms包中内置的功能
RL_plot0 <- brms::conditional_effects(var_inter_model, 
                                      effects = "percentCoherence",
                                      conditions = data.frame(subject = unique(df_first5$subject)),
                                      re_formula = NULL)
# 提取绘图数据
plot_data <- RL_plot0$percentCoherence

# 进行可视化
pp1 <- ggplot2::ggplot() +
  # 添加原始数据的散点图（按站点着色）
  ggplot2::geom_point(data = df_first5, 
                      aes(x = percentCoherence, y = log_RTs, color = subject),
                      alpha = 0.6, size = 1.5) +
  # 添加置信区间
  ggplot2::geom_ribbon(data = plot_data,
                       aes(x = percentCoherence, ymin = lower__, ymax = upper__, fill = "subject"),
                       alpha = 0.3) +
  # 添加回归线
  ggplot2::geom_line(data = plot_data,
                     aes(x = percentCoherence, y = estimate__, color = "subject"),
                     linewidth = 1) +
  # 创建分面
  ggplot2::facet_wrap(~ subject, nrow = 1) +
  ggplot2::labs(title = "Posterior regression models",
                x = "percentCoherence", 
                y = "RT") +
  papaja::theme_apa() +
  ggplot2::coord_cartesian(xlim = c(5, 10), ylim = c(5, 10)) +
  ggplot2::theme(legend.position = "none",  # 移除图例
                 strip.text = element_text(face = "bold"))
#显示图形
options(repr.plot.width = 20, repr.plot.height = 6)
pp1

##组间方差与组内方差
model_var <- brms::VarCorr(var_inter_model)
print(model_var)
# 提取组间变异
sd_intercept <- model_var$subject$sd[1]  
between_var_intercept <- as.numeric(sd_intercept)^2

# 提取组内变异（从后验样本计算）
sigma_samples <- brms::posterior_samples(var_inter_model, pars = "sigma")
residual_sd <- mean(sigma_samples$sigma)
within_var <- residual_sd^2

# 计算变异解释比例
total_var_intercept <- between_var_intercept + within_var
prop_intercept_between <- (between_var_intercept / total_var_intercept) * 100  # 组间解释百分比
prop_intercept_within <- (within_var / total_var_intercept) * 100            # 组内解释百分比

cat("组间方差：", round(between_var_intercept, 4), "\n")
cat("组内方差：", round(within_var, 4), "\n")
cat("被组间方差所解释的部分：", round(prop_intercept_between, 2), "%\n")
cat("被组内方差所解释的部分：", round(prop_intercept_within, 2), "%\n\n")

# =================================================
#                    补充...部分
# =================================================

var_slope_model <- brms::brm(
  # 模型公式
  formula = ...,
  # 数据
  data = ...,
  # 先验分布设置
  prior = c(
    prior(normal(7, 1), class = Intercept),        # beta_0 
    prior(normal(7.5, 5), class = b, coef = percentCoherence), #beta_1
    prior(exponential(1), class = sd, group = subject, coef = percentCoherence),             #beta_0_sigma
    prior(exponential(1), class = sigma)           #sigma_y
  ),
  chains = 4,
  iter = 2000,
  warmup = 1000,
  seed = 84735,
  control = list(adapt_delta = 0.95)
)

subject_coef2 <- as.data.frame(stats::coef(var_slope_model))

#整理数据
coef_df2 <- as.data.frame(summary(var_slope_model)$fixed)

plot_hdi2 <- subject_coef2 %>%
  select(1:5) %>%
  rename(slope = 1, 
         slope_error = 2, 
         slope_Q2.5 = 3, 
         slope_Q97.5 = 4) %>%
  bind_rows(
    data.frame(slope = coef_df2$Estimate, slope_error = coef_df2$Est.Error, 
               slope_Q2.5 = coef_df2$`l-95% CI`, slope_Q97.5 = coef_df2$`u-95% CI`) %>%
      slice(2)
  )
rownames(plot_hdi2)[nrow(plot_hdi2)] <- "beta1"

plot_hdi2 <- plot_hdi2 %>%
  rownames_to_column("subject") %>%
  mutate(subject = factor(subject, levels = subject))

plot_hdi2

# 可视化
# =================================================
#                    补充...部分
# =================================================

ggplot2::ggplot(plot_hdi2, aes(x = ..., y = ...)) +
  # 绘制HDI区间
  ggplot2::geom_errorbarh(
    aes(xmin = plot_hdi2$slope_Q2.5, xmax = plot_hdi2$slope_Q97.5),
    height = 0.2, linewidth = 1, color = "steelblue"
  ) +
  # 绘制后验均值点
  ggplot2::geom_point(
    size = 4, color = "white", stroke = 1.2, shape = 21, fill = "steelblue"
  ) +
  papaja::theme_apa()+
  ggplot2::labs(title = "95% HDI")

#使用brms包中内置的功能
RL_plot2 <- brms::conditional_effects(var_slope_model, 
                                      effects = "percentCoherence",
                                      conditions = data.frame(subject = unique(df_first5$subject)),
                                      re_formula = NULL)

# 提取绘图数据
plot_data2 <- RL_plot2$percentCoherence

pp2 <- ggplot2::ggplot() +
  # 添加原始数据的散点图（按站点着色）
  ggplot2::geom_point(data = df_first5, 
                      aes(x = percentCoherence, y = log_RTs, color = subject),
                      alpha = 0.6, size = 1.5) +
  # 添加置信区间
  ggplot2::geom_ribbon(data = plot_data2,
                       aes(x = percentCoherence, ymin = lower__, ymax = upper__, fill = "subject"),
                       alpha = 0.3) +
  # 添加回归线
  ggplot2::geom_line(data = plot_data2,
                     aes(x = percentCoherence, y = estimate__, color = "subject"),
                     linewidth = 1) +
  # 创建分面
  ggplot2::facet_wrap(~ subject, nrow = 1) +
  ggplot2::labs(title = "Posterior regression models",
                x = "percentCoherence", 
                y = "RT") +
  papaja::theme_apa() +
  ggplot2::coord_cartesian(xlim = c(5, 10), ylim = c(5, 10)) +
  ggplot2::theme(legend.position = "none",  # 移除图例
                 strip.text = element_text(face = "bold"))
#显示图形
options(repr.plot.width = 20, repr.plot.height = 6)
pp2

##组间方差与组内方差
model2_var <- brms::VarCorr(var_slope_model)
# 提取组间变异
sd_intercept2 <- model2_var$subject$sd[1]  
between_var_intercept2 <- as.numeric(sd_intercept2)^2

# 提取组内变异（从后验样本计算）
sigma_samples2 <- brms::posterior_samples(var_slope_model, pars = "sigma")
residual_sd2 <- mean(sigma_samples2$sigma)
within_var2 <- residual_sd2^2

# 计算变异解释比例
total_var_intercept2 <- between_var_intercept2 + within_var2
prop_intercept_between2 <- (between_var_intercept2 / total_var_intercept2) * 100  # 组间解释百分比
prop_intercept_within2 <- (within_var2 / total_var_intercept2) * 100            # 组内解释百分比

cat("组间方差：", round(between_var_intercept2, 4), "\n")
cat("组内方差：", round(within_var2, 4), "\n")
cat("被组间方差所解释的部分：", round(prop_intercept_between2, 2), "%\n")
cat("被组内方差所解释的部分：", round(prop_intercept_within2, 2), "%\n\n")

# =================================================
#                    补充...部分
# =================================================

# Model3
var_both_model <- brms::brm(
  # 模型公式
  formula = ...,
  # 数据
  data = ...,
  # 先验分布设置
  prior = c(
    prior(normal(7, 1), class = Intercept),        # beta0 
    prior(normal(7.5, 5), class = b, coef = percentCoherence), #beta1
    prior(exponential(1), class = sd, group = subject, coef = Intercept), #beta0_sigma
    prior(exponential(1), class = sd, group = subject, coef = percentCoherence), #beta1_sigma
    prior(exponential(1), class = sigma)           #sigma_y
  ),
  chains = 4,
  iter = 2000,
  warmup = 1000,
  seed = 84735,
  control = list(adapt_delta = 0.95)
)

subject_coef3 <- as.data.frame(stats::coef(var_both_model))

#整理数据
coef_df3 <- as.data.frame(summary(var_both_model)$fixed)
#截距
Intercept_hdi <- subject_coef3 %>%
  select(1:4) %>%
  rename(Intercept = 1, 
         Intercept_error = 2, 
         Intercept_Q2.5 = 3, 
         Intercept_Q97.5 = 4) %>%
  bind_rows(
    data.frame(Intercept = coef_df3$Estimate, Intercept_error = coef_df3$Est.Error, 
               Intercept_Q2.5 = coef_df3$`l-95% CI`, Intercept_Q97.5 = coef_df3$`u-95% CI`) %>%
      slice(1)
  )
rownames(Intercept_hdi)[nrow(Intercept_hdi)] <- "beta0"
Intercept_hdi <- Intercept_hdi %>%
  rownames_to_column("subject") %>%
  mutate(subject = factor(subject, levels = subject))
#斜率
Slope_hdi <- subject_coef3 %>%
  select(5:8) %>%
  rename(slope = 1, 
         slope_error = 2, 
         slope_Q2.5 = 3, 
         slope_Q97.5 = 4) %>%
  bind_rows(
    data.frame(slope = coef_df2$Estimate, slope_error = coef_df2$Est.Error, 
               slope_Q2.5 = coef_df2$`l-95% CI`, slope_Q97.5 = coef_df2$`u-95% CI`) %>%
      slice(2)
  )
rownames(Slope_hdi)[nrow(Slope_hdi)] <- "beta1"
Slope_hdi <- Slope_hdi %>%
  rownames_to_column("subject") %>%
  mutate(subject = factor(subject, levels = subject))

# =================================================
#                    补充...部分
# =================================================

# 可视化
int_plot <- ggplot2::ggplot(Intercept_hdi, aes(x = ..., y = ...)) +
  # 绘制HDI区间
  ggplot2::geom_errorbarh(
    aes(xmin = Intercept_hdi$Intercept_Q2.5, xmax = Intercept_hdi$Intercept_Q97.5),
    height = 0.2, linewidth = 1, color = "steelblue"
  ) +
  # 绘制后验均值点
  ggplot2::geom_point(
    size = 4, color = "white", stroke = 1.2, shape = 21, fill = "steelblue"
  ) +
  papaja::theme_apa()+
  ggplot2::labs(title = "95% HDI")

slop_plot <- ggplot2::ggplot(Slope_hdi, aes(x = ..., y = ...)) +
  # 绘制HDI区间
  ggplot2::geom_errorbarh(
    aes(xmin = Slope_hdi$slope_Q2.5, xmax = Slope_hdi$slope_Q97.5),
    height = 0.2, linewidth = 1, color = "steelblue"
  ) +
  # 绘制后验均值点
  ggplot2::geom_point(
    size = 4, color = "white", stroke = 1.2, shape = 21, fill = "steelblue"
  ) +
  papaja::theme_apa()+
  ggplot2::labs(title = "95% HDI")

int_plot + slop_plot

# 使用brms包中内置的功能
RL_plot3 <- brms::conditional_effects(var_both_model, 
                                      effects = "percentCoherence",
                                      conditions = data.frame(subject = unique(df_first5$subject)),
                                      re_formula = NULL)

# 提取绘图数据
plot_data3 <- RL_plot3$percentCoherence

pp3 <- ggplot2::ggplot() +
  # 添加原始数据的散点图（按站点着色）
  ggplot2::geom_point(data = df_first5, 
                      aes(x = percentCoherence, y = log_RTs, color = subject),
                      alpha = 0.6, size = 1.5) +
  # 添加置信区间
  ggplot2::geom_ribbon(data = plot_data3,
                       aes(x = percentCoherence, ymin = lower__, ymax = upper__, fill = "subject"),
                       alpha = 0.3) +
  # 添加回归线
  ggplot2::geom_line(data = plot_data3,
                     aes(x = percentCoherence, y = estimate__, color = "subject"),
                     linewidth = 1) +
  # 创建分面
  ggplot2::facet_wrap(~ subject, nrow = 1) +
  ggplot2::labs(title = "Posterior regression models",
                x = "percentCoherence", 
                y = "RT") +
  papaja::theme_apa() +
  ggplot2::coord_cartesian(xlim = c(5, 10), ylim = c(5, 10)) +
  ggplot2::theme(legend.position = "none",  # 移除图例
                 strip.text = element_text(face = "bold"))
#显示图形
options(repr.plot.width = 20, repr.plot.height = 6)
pp3

# 提取随机效应的变异信息
model3_var <- brms::VarCorr(var_both_model)

# 提取组间变异
sd_intercept3 <- model3_var$subject$sd[1]  
between_var_intercept3 <- as.numeric(sd_intercept3)^2

sd_slope3 <- model3_var$subject$sd[2]      
between_var_slope3 <- as.numeric(sd_slope3)^2

# 提取组内变异
sigma_samples3 <- brms::posterior_samples(var_both_model, pars = "sigma")
residual_sd3 <- mean(sigma_samples3$sigma)
within_var3 <- residual_sd3^2

# 计算截距的变异解释比例
total_var_intercept3 <- between_var_intercept3 + within_var3
prop_intercept_between3 <- (between_var_intercept3 / total_var_intercept3) * 100  # 组间解释百分比
prop_intercept_within3 <- (within_var3 / total_var_intercept3) * 100            # 组内解释百分比

# 计算斜率的变异解释比例
total_var_slope3 <- between_var_slope3 + within_var3
prop_slope_between3 <- (between_var_slope3 / total_var_slope3) * 100            # 组间解释百分比
prop_slope_within3 <- (within_var3 / total_var_slope3) * 100                    # 组内解释百分比

# 打印结果
cat("=== 截距的变异解释比例 ===\n")
cat("截距的组间方差：", round(between_var_intercept3, 4), "\n")
cat("组内方差：", round(within_var3, 4), "\n")
cat("截距的组间解释比例：", round(prop_intercept_between3, 2), "%\n")
cat("截距的组内解释比例：", round(prop_intercept_within3, 2), "%\n\n")

cat("=== 斜率的变异解释比例 ===\n")
cat("斜率的组间方差：", round(between_var_slope3, 4), "\n")
cat("组内方差：", round(within_var3, 4), "\n")
cat("斜率的组间解释比例：", round(prop_slope_between3, 2), "%\n")
cat("斜率的组内解释比例：", round(prop_slope_within3, 2), "%\n")

# =================================================
#                    补充...部分
# =================================================

# 对仅截距随机模型进行后验预测
var_inter_ppc <- posterior_predict(
  object = ..., # 填入拟合的模型
  seed = 84735
)

# 对仅斜率随机模型进行后验预测
var_slope_ppc <- posterior_predict(
  object = ...,
  seed = 84735
)

# 对截距+斜率随机模型进行后验预测
var_both_ppc <- posterior_predict(
  object = ...,
  seed = 84735
)

# 定义计算MAE的函数
calculate_MAE <- function(model_ppc, original_data, y_var = "log_RTs") {
  # 计算每个观测的后验预测均值
  pre_y_mean <- colMeans(model_ppc)
  
  # 构建数据框：预测均值 + 原始观测值
  MAE_df <- data.frame(
    RT_ppc_mean = pre_y_mean,
    RT_original = original_data[[y_var]]  # 提取指定的响应变量观测值
  )
  
  # 计算绝对预测误差
  MAE_df <- MAE_df %>%
    mutate(pre_error = abs(RT_original - RT_ppc_mean))
  
  # 计算误差的中位数
  MAE_value <- median(MAE_df$pre_error, na.rm = TRUE)
  
  return(MAE_value)
}

var_inter_mae <- calculate_MAE(var_inter_ppc, var_inter_model$data, "log_RTs")
var_slope_mae <- calculate_MAE(var_slope_ppc, var_slope_model$data, "log_RTs")
var_both_mae <- calculate_MAE(var_both_ppc, var_both_model$data, "log_RTs")

# 汇总所有模型的MAE
mae_summary <- data.frame(
  model = c("var_inter", "var_slope", "var_both"),
  mae = c(var_inter_mae, var_slope_mae, var_both_mae)
)
print(mae_summary)

#相对指标ELPD-LOO
# 计算各模型的LOO
loo1 <- loo::loo(...) # 填入拟合的模型
loo2 <- loo::loo(...)
loo3 <- loo::loo(...)

# 比较模型的LOO
loo::loo_compare(loo1, loo2, loo3)