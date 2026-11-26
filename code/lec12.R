# 安装和加载包
options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
if (!require(remotes)) {
  install.packages("remotes")
}
remotes::install_github('njudd/ggrain')
if (!requireNamespace('pacman', quietly = TRUE)) {
  install.packages('pacman')
}

pacman::p_load("tidyverse","ggplot2", "dplyr","gridExtra","papaja", "patchwork","bayesplot",
               "rstan",'logspline', "easystats","ggrain") 
options(warn = -1)  # 抑制警告

# 导入数据
df <- tryCatch({
  read.csv('/home/mw/input/bayes3797/evans2020JExpPsycholLearn_exp1_full_data.csv')
}, error = function(e) {
  read.csv('data/evans2020JExpPsycholLearn_exp1_full_data.csv')
})

# 筛选 subject == 31727 且 percentCoherence 为 10 或 40 的数据

df_clean <- df %>%
  filter(subject == 31727, percentCoherence %in% c(10, 40)) %>%
  select(subject, percentCoherence, correct)

# 显示结果
df_clean

# 计算每个 percentCoherence 组的 correct 列的平均值
df_summary <- df_clean %>%
  group_by(percentCoherence) %>%
  summarise(mean_correct = mean(correct, na.rm = TRUE), .groups = 'drop')

# 显示结果
df_summary

# 计算每个 percentCoherence 组的 correct 均值，并绘制条形图
df_clean %>%
  group_by(percentCoherence) %>%
  summarise(mean_correct = mean(correct, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(percentCoherence), y = mean_correct)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Percent Coherence", y = "Accuracy") +
  papaja::theme_apa()

# 数据准备
Treatment_Coding <- df_clean %>%
  mutate(
    percentCoherence = factor(percentCoherence),
    x = as.numeric(percentCoherence) - 1  # Treatment coding: 10 -> 0, 40 -> 1
  )

# Stan 数据
stan_data <- list(
  N = nrow(Treatment_Coding),
  y = Treatment_Coding$correct,
  x = Treatment_Coding$x,
  K = 1  # 只有一个预测变量（treatment 编码）
)
stan_data

# Stan 模型代码（字符串形式）
stan_model_code <- "
data {
  int<lower=0> N;          // 样本数
  int<lower=0,upper=1> y[N]; // 因变量
  vector[N] x;               // 预测变量（treatment: 0 or 1）
}

parameters {
  real beta_0;             // 截距
  real beta_1;             // 斜率（10% vs 40% 的差异）
}

model {
  // 先验
  beta_0 ~ normal(0, 10);
  beta_1 ~ normal(0, 10);
  
  // 似然
  y ~ bernoulli_logit(beta_0 + beta_1 * x);
}

generated quantities {
  vector[N] log_lik;       // 用于 LOO
  vector[N] y_rep;         // 后验预测
  real<lower=0,upper=1> pi_10;  // 10% 相干性的预测概率
  real<lower=0,upper=1> pi_40;  // 40% 相干性的预测概率
  
  for (n in 1:N) {
    real logit_p = beta_0 + beta_1 * x[n];
    log_lik[n] = bernoulli_logit_lpmf(y[n] | logit_p);
    y_rep[n] = bernoulli_rng(inv_logit(logit_p));
  }
  
  pi_10 = inv_logit(beta_0);           // x = 0
  pi_40 = inv_logit(beta_0 + beta_1);   // x = 1
}
"

# 编译并拟合模型
fit <- stan(
  model_code = stan_model_code,
  data = stan_data,
  chains = 4,
  iter = 2000,
  warmup = 1000,
  seed = 123
)

fit_trace <- bayesplot::mcmc_trace(fit,pars = c('beta_0','beta_1','pi_10','pi_40'))+
  papaja::theme_apa()
fit_trace

trace <- rstan::extract(fit)
trace_beta_1_par <- as.data.frame(trace$beta_1)

#get HDI
bayestestR::hdi(trace_beta_1_par)

print(fit,pars = c('beta_0','beta_1','pi_10','pi_40'))

# 设置模拟样本数
S <- 10000

# 从先验中抽样
beta_0_prior <- rnorm(S, mean = 0, sd = 10)
beta_1_prior <- rnorm(S, mean = 0, sd = 10)

# 计算先验预测概率
pi_10_prior <- plogis(beta_0_prior)          # x = 0
pi_40_prior <- plogis(beta_0_prior + beta_1_prior)  # x = 1

# 转换为数据框用于绘图
prior_df <- data.frame(
  pi_10 = pi_10_prior,
  pi_40 = pi_40_prior
) %>%
  pivot_longer(cols = everything(), names_to = "condition", values_to = "pi")

# 绘图：先验预测概率分布
ggplot(prior_df, aes(x = pi, fill = condition)) +
  geom_histogram(alpha = 0.7, bins = 100, position = "identity") +
  scale_x_continuous(limits = c(0, 1), name = "Predicted probability (π)") +
  scale_fill_manual(values = c("pi_10" = "skyblue", "pi_40" = "salmon")) +
  labs(
    title = "Prior Predictive Distribution of π (10% vs 40% coherence)",
    y = "Density (approx.)"
  ) +
  papaja::theme_apa()

set.seed(123)

# 要绘制的 percentCoherence 范围
percent_coherence <- seq(0, 50, length.out = 200)

# 随机抽取 50 个样本
n_curves <- 50
sample_idx <- sample(S, n_curves)

# 构建每条直线：只基于 (10, pi10) 和 (40, pi40) 两点线性插值
curve_data <- map_dfr(sample_idx, ~ {
  pi10 <- plogis(beta_0_prior[.x])              # x = 10 → logit = beta0
  pi40 <- plogis(beta_0_prior[.x] + beta_1_prior[.x])  # x = 40 → logit = beta0 + beta1
  
  # 线性插值：用两点定义直线 y = a + b * x
  # 已知: (x1=10, y1=pi10), (x2=40, y2=pi40)
  slope <- (pi40 - pi10) / (40 - 10)
  intercept <- pi10 - slope * 10
  
  tibble(
    draw = .x,
    percentCoherence = percent_coherence,
    prob_correct = intercept + slope * percent_coherence
  )
})

# 绘图
ggplot(curve_data, aes(x = percentCoherence, y = prob_correct, group = draw)) +
  geom_line(color = "grey60", alpha = 0.7, size = 0.5) +
  # 可选：添加两个关键点的平均值
  geom_point(
    data = tibble(
      pc = c(10, 40),
      pi_mean = c(
        mean(plogis(beta_0_prior)), 
        mean(plogis(beta_0_prior + beta_1_prior))
      )
    ),
    aes(x = pc, y = pi_mean),
    color = "red",
    size = 2,
    inherit.aes = FALSE
  ) +
  labs(
    x = "percentCoherence",
    y = "probability of correct",
    title = "50 Prior Predictive Linear Models",
    subtitle = "Each line connects (10%, π₁₀) and (40%, π₄₀) with straight line"
  ) +
  scale_x_continuous(breaks = seq(0, 50, by = 10)) +
  ylim(0, 1) +
  papaja::theme_apa() +
  theme(
    plot.title = element_text(size = 14),
    axis.title = element_text(size = 12)
  )

set.seed(123)
# 定义连续的 percentCoherence
percent_coherence <- seq(0, 50, length.out = 200)
x_continuous <- (percent_coherence - 10) / 30  # 10%→0, 40%→1

# 随机抽取 50 条曲线
n_curves <- 50
sample_idx <- sample(S, n_curves)

# 构建曲线数据
curve_data <- map_dfr(sample_idx, ~ {
  tibble(
    draw = .x,
    percentCoherence = percent_coherence,
    prob_correct = plogis(beta_0_prior[.x] + beta_1_prior[.x] * x_continuous)
  )
})

# 绘图
ggplot(curve_data, aes(x = percentCoherence, y = prob_correct, group = draw)) +
  geom_line(color = "grey60", alpha = 0.7, size = 0.5) +
  # 添加两个条件下的平均预测概率（红点）
  geom_point(
    data = tibble(
      pc = c(10, 40),
      pi_mean = c(
        mean(plogis(beta_0_prior)), 
        mean(plogis(beta_0_prior + beta_1_prior))
      )
    ),
    aes(x = pc, y = pi_mean),
    color = "red",
    size = 2,
    inherit.aes = FALSE   
  ) +
  labs(
    x = "percentCoherence",
    y = "probability of correct",
    title = "Relationships between percentCoherence and the probability of correct",
    subtitle = paste0("50 prior predictive logistic curves (", n_curves, " draws from prior)")
  ) +
  scale_x_continuous(breaks = seq(0, 50, by = 10)) +
  ylim(0, 1) +
  papaja::theme_apa() +
  theme(
    plot.title = element_text(size = 14),
    axis.title = element_text(size = 12)
  )

# 1. 提取后验样本（保留链和迭代信息）
post_array <- as.array(fit)  # dimensions: iterations x chains x parameters

# 2. 合并 chains 和 iterations → 得到 S 个后验样本
# 提取 beta_0 和 beta_1
beta_0_samples <- as.vector(post_array[, , "beta_0"])
beta_1_samples <- as.vector(post_array[, , "beta_1"])

# 3. 设定绘制多少条曲线（例如 50 条）
n_curves <- 50
set.seed(123)  # 为了可重复性
sample_idx <- sample(length(beta_0_samples), size = n_curves)

# 4. 构建数据框：每条线有2个点 (x=0 和 x=1)
coherence_levels <- c(10, 40)  # x = 0 → 10%, x = 1 → 40%

plot_data <- tibble(
  curve_id = rep(1:n_curves, each = 2),
  coherence = rep(coherence_levels, times = n_curves),
  prob = c(
    plogis(beta_0_samples[sample_idx]),                    # pi_10 at x=0 (10%)
    plogis(beta_0_samples[sample_idx] + beta_1_samples[sample_idx])  # pi_40 at x=1 (40%)
  )
)

# 5. 绘图
ggplot(plot_data, aes(x = coherence, y = prob, group = curve_id)) +
  geom_line(color = "grey60", alpha = 0.5) +
  scale_x_continuous(breaks = coherence_levels, name = "Percent Coherence") +
  scale_y_continuous(limits = c(0, 1), name = "Probability of Correct") +
  labs(
    title = paste(n_curves, "Posterior Plausible Models")
  ) +
  papaja::theme_apa() +
  theme(plot.title = element_text(hjust = 0.5))

# 1. 提取后验样本（beta_0 和 beta_1）
post_array <- as.array(fit)
beta_0_samp <- as.vector(post_array[, , "beta_0"])
beta_1_samp <- as.vector(post_array[, , "beta_1"])

# 2. 计算 x = 1 时的预测概率（pi_40）
pi_40_samp <- plogis(beta_0_samp + beta_1_samp)

# 3. 生成后验预测样本：对每个后验样本，模拟一个新观测 y_new ~ Bernoulli(pi_40)
set.seed(123)  # 为了可重复性
y_pred <- rbinom(length(pi_40_samp), size = 1, prob = pi_40_samp)

# 4. 计算 0 和 1 的比例
prop_table <- prop.table(table(y_pred))
prop_df <- data.frame(
  correct = as.integer(names(prop_table)),
  proportion = as.numeric(prop_table)
)

# 5. 绘制柱状图
ggplot(prop_df, aes(x = factor(correct), y = proportion, fill = factor(correct))) +
  geom_col(width = 0.6, show.legend = FALSE) +
  scale_fill_manual(values = c("#70AD47", "#4472C4")) +  # 绿色和蓝色，可自定义
  geom_text(aes(label = scales::percent(proportion, accuracy = 0.1)), 
            vjust = -0.4, size = 4) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(
    title = "Out-of-sample Prediction (x = 1)",
    x = "Correct",
    y = "Proportion"
  ) +
  papaja::theme_apa() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(size = 12),
    axis.title = element_text(size = 12)
  ) +
  scale_x_discrete(labels = c("0" = "Incorrect", "1" = "Correct"))  

# 不确定性区间（95%预测区间）
quantile(pi_40_samp, c(0.025, 0.975))

df <- Treatment_Coding
# 1. 计算每个观测的后验平均预测概率 pi
# 注意：这里我们使用后验均值，而不是每个后验样本
pi_10_mean <- mean(plogis(beta_0_samp))          # x = 0
pi_40_mean <- mean(plogis(beta_0_samp + beta_1_samp))  # x = 1

df$pi <- ifelse(df$x == 0, pi_10_mean, pi_40_mean)

# 2. 为每个观测生成一个预测结果（基于其 pi）
set.seed(123)
df$prediction <- rbinom(n = nrow(df), size = 1, prob = df$pi)

# 3. 查看前几行
head(df[, c("subject", "percentCoherence", "correct", "pi", "prediction")])

# 定义计算混淆矩阵函数（TP, TN, FP, FN）
calculate_confusion_table <- function(df, y_col = "correct", yhat_col = "prediction") {
  # 计算各种情况的数量
  TN <- sum((df[[y_col]] == 0) & (df[[yhat_col]] == 0))  # 真阴性
  FP <- sum((df[[y_col]] == 0) & (df[[yhat_col]] == 1))  # 假阳性
  FN <- sum((df[[y_col]] == 1) & (df[[yhat_col]] == 0))  # 假阴性
  TP <- sum((df[[y_col]] == 1) & (df[[yhat_col]] == 1))  # 真阳性
  
  # 创建 DataFrame 来表示列联表
  contingency_df <- data.frame(
    `$\\hat{Y} = 0$` = c(TN, FN),
    `$\\hat{Y} = 1$` = c(FP, TP),
    row.names = c('$Y=0$', '$Y=1$')
  )
  
  # 返回结果
  return(list(
    TN = TN,
    FP = FP,
    FN = FN,
    TP = TP,
    contingency_df = contingency_df
  ))
}

# 计算混淆矩阵
cm <- calculate_confusion_table(df, y_col = "correct", yhat_col = "prediction")

cm$contingency_df

# 计算指标 Accuracy, Sensitivity, Specificity
accuracy <- (cm$TP + cm$TN) / nrow(df)
sensitivity <- ifelse(cm$TP + cm$FN > 0, cm$TP / (cm$TP + cm$FN), 0)
specificity <- ifelse(cm$TN + cm$FP > 0, cm$TN / (cm$TN + cm$FP), 0)


cat("\n")
cat("True Positive:", cm$TP, "\n")
cat("False Positive:", cm$FP, "\n")
cat("True Negative:", cm$TN, "\n")
cat("False Negative:", cm$FN, "\n")
cat("准确性:", accuracy, "\n")
cat("敏感性:", sensitivity, "\n")
cat("特异性:", specificity, "\n")

library(brms)

# 1. 准备数据：确保 percentCoherence 是因子
df_clean <- df_clean %>%
  mutate(percentCoherence = as.factor(percentCoherence))

# 2. 建模（无需 Stan）
model <- brm(
  formula = correct ~ percentCoherence,   # 自动处理为 C(percentCoherence)
  data = df_clean,
  family = bernoulli(link = "logit"),
  chains = 4,
  iter = 2000,
  warmup = 1000,
  seed = 84735,
  cores = 4
)

# 3. 参数摘要
summary(model)

# 4. 预测概率
newdata = data.frame(percentCoherence = factor(c("10", "40")))
predictions = fitted(model, newdata = newdata)
cat("p(coherence=10) =", round(predictions[1], 3), "\n")
cat("p(coherence=40) =", round(predictions[2], 3), "\n")

# 5. 后验预测检查（PPC）
ppc_dens_overlay(y = df_clean$correct, yrep = posterior_predict(model, ndraws = 100)) +
  labs(title = "Posterior Predictive Check")

# === 1. 数据加载 ===
df_raw <- tryCatch({
  read.csv("/home/mw/input/bayes3797/Data_Sum_HPP_Multi_Site_Share.csv")
}, error = function(e) {
  read.csv("data/Data_Sum_HPP_Multi_Site_Share.csv")
})

# === 2. 数据筛选与清洗 ===
df <- df_raw %>%
  filter(Site == "Tsinghua") %>%
  select(romantic, avoidance_r, sex) %>%
  mutate(
    romantic = ifelse(romantic == 2, 0, 1),  # 2 → "no" → 0; 1 → "yes" → 1
    romantic = factor(romantic, levels = c(0, 1), labels = c("no", "yes")),  # 转换为因子
    index = 1:n()  # 设置索引（1 到 n）
  )

# === 3. 绘制raincloud图 ===
p_raincloud <- ggplot(df, aes(x = romantic, y = avoidance_r, fill = romantic)) +
  geom_rain(alpha = 0.6, rain.side = "l", point.args = list(alpha = 0.4)) +
  scale_fill_manual(values = c("no" = "red", "yes" = "blue")) +
  labs(
    x = "Romantic Relationship",
    y = "Avoidance (Reversed)",
    fill = "Romantic"
  ) +
  theme_apa() +
  theme(legend.position = "none")

p_raincloud

# === 4. 准备 Stan 数据 ===
stan_data <- list(
  N = nrow(df),
  y = df$romantic,
  x = df$avoidance_r
)

# === 5. 定义 Stan 模型（logistic 回归）===
stan_model_code <- "
data {
  int<lower=0> N;
  int<lower=0,upper=1> y[N];
  vector[N] x;
}
parameters {
  real beta_0;
  real beta_1;
}
model {
  beta_0 ~ normal(0, 0.5);
  beta_1 ~ normal(0, 0.5);
  y ~ bernoulli_logit(beta_0 + beta_1 * x);
}
generated quantities {
  vector[N] pi;
  vector[N] y_rep;
  for (n in 1:N) {
    pi[n] = inv_logit(beta_0 + beta_1 * x[n]);
    y_rep[n] = bernoulli_rng(pi[n]);
  }
}
"

# === 6. 拟合模型（MCMC 采样）===
fit <- stan(
  model_code = stan_model_code,
  data = stan_data,
  chains = 4,
  iter = 5000,
  warmup = 1000,
  seed = 84735,
  cores = 4
)

trace <- bayesplot::mcmc_trace(fit,pars = c('beta_0','beta_1'))+
  papaja::theme_apa()
trace

# density plot
post_array <- as.array(fit)

p_density <- mcmc_dens_overlay(post_array, pars = c("beta_0", "beta_1")) +
  theme_minimal()

p_density

# === 8. 绘制后验预测回归线 + 真实数据点 ===
# 提取后验样本
beta_0_samp <- as.vector(post_array[, , "beta_0"])
beta_1_samp <- as.vector(post_array[, , "beta_1"])

# 创建网格
x_grid <- seq(min(df$avoidance_r), max(df$avoidance_r), length.out = 100)

# 选择 50 条后验曲线
set.seed(123)
idx <- sample(length(beta_0_samp), 50)

# 构建曲线数据
curve_df <- map_dfr(idx, ~ {
  tibble(
    draw = .x,
    avoidance_r = x_grid,
    pi = plogis(beta_0_samp[.x] + beta_1_samp[.x] * x_grid)
  )
})

# 绘图
p_posterior_line <- ggplot(curve_df, aes(x = avoidance_r, y = pi, group = draw)) +
  geom_line(color = "grey60", alpha = 0.5) +
  geom_point(data = df, aes(x = avoidance_r, y = romantic, color = factor(romantic)),
             size = 2, alpha = 0.7,
             inherit.aes = FALSE) +
  scale_color_manual(values = c("red", "blue"), labels = c("no", "yes")) +
  labs(
    title = "Posterior Predictive Regression Lines",
    x = "avoidance_r",
    y = "P(romantic = yes)"
  ) +
  # === 1. 数据加载 ===
  df_raw <- tryCatch({
    read.csv("/home/mw/input/bayes3797/Data_Sum_HPP_Multi_Site_Share.csv")
  }, error = function(e) {
    read.csv("data/Data_Sum_HPP_Multi_Site_Share.csv")
  })

# === 2. 数据筛选与清洗 ===
df <- df_raw %>%
  filter(Site == "Tsinghua") %>%
  select(romantic, avoidance_r, sex) %>%
  mutate(
    romantic = ifelse(romantic == 2, 0, 1),  # 2 → "no" → 0; 1 → "yes" → 1
    romantic = factor(romantic, levels = c(0, 1), labels = c("no", "yes")),  # 转换为因子
    index = 1:n()  # 设置索引（1 到 n）
  )

# === 3. 绘制raincloud图 ===
p_raincloud <- ggplot(df, aes(x = romantic, y = avoidance_r, fill = romantic)) +
  geom_rain(alpha = 0.6, rain.side = "l", point.args = list(alpha = 0.4)) +
  scale_fill_manual(values = c("no" = "red", "yes" = "blue")) +
  labs(
    x = "Romantic Relationship",
    y = "Avoidance (Reversed)",
    fill = "Romantic"
  ) +
  theme_apa() +
  theme(legend.position = "none")

p_raincloud 



# === 9. 对新数据 X=1 进行预测（柱状图）===
# 假设新样本的 avoidance_r = 1（标准化后）
new_x <- 1
pi_new <- plogis(beta_0_samp + beta_1_samp * new_x)
y_pred_new <- rbinom(length(pi_new), 1, pi_new)

# 汇总比例
prop_df <- tibble(
  class = c("no", "yes"),
  proportion = as.numeric(prop.table(table(factor(y_pred_new, levels = 0:1))))
)

p_prediction_bar <- ggplot(prop_df, aes(x = class, y = proportion, fill = class)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  scale_fill_manual(values = c("no" = "#70AD47", "yes" = "#4472C4")) +
  geom_text(aes(label = scales::percent(proportion, accuracy = 0.1)), 
            vjust = -0.4, size = 4) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(
    title = "Out-of-sample Prediction (X = 1)",
    x = "Romantic",
    y = "Proportion"
  ) +
  papaja::theme_apa() +
  theme(plot.title = element_text(hjust = 0.5))

p_prediction_bar

# === 10. 后验预测评估：混淆矩阵 & 指标 ===
# 为原始数据生成预测（使用后验均值）
df$pi_mean <- plogis(mean(beta_0_samp) + mean(beta_1_samp) * df$avoidance_r)
df$prediction <- rbinom(nrow(df), 1, df$pi_mean)

# 计算指标
tp <- sum(df$romantic == 1 & df$prediction == 1)
fp <- sum(df$romantic == 0 & df$prediction == 1)
tn <- sum(df$romantic == 0 & df$prediction == 0)
fn <- sum(df$romantic == 1 & df$prediction == 0)

accuracy <- (tp + tn) / nrow(df)
sensitivity <- ifelse(tp + fn > 0, tp / (tp + fn), 0)
specificity <- ifelse(tn + fp > 0, tn / (tn + fp), 0)

# 打印结果
cat("\n")
cat("True Positive:", tp, "\n")
cat("False Positive:", fp, "\n")
cat("True Negative:", tn, "\n")
cat("False Negative:", fn, "\n")
cat("准确性:", accuracy, "\n")
cat("敏感性:", sensitivity, "\n")
cat("特异性:", specificity, "\n")