# 第7讲：后验推断与估计
# 以 lec7.qmd 为准；图表数据、参数、采样次数与区间概率保持一致。

options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
pacman::p_load(ggplot2, patchwork, rstan, bayesplot, papaja, bayestestR)
theme_set(papaja::theme_apa(base_size = 16))
set.seed(84735)


# ---- 回顾：未归一化后验 ----
# 对应 qmd：posterior-kernel

N <- 10
Y <- 9
theta <- seq(0, 1, length.out = 10000)
prior_density <- dbeta(theta, shape1 = 2, shape2 = 2)
likelihood <- dbinom(Y, size = N, prob = theta)
posterior_unstandardized <- likelihood * prior_density

# ---- Beta-Binomial：4 条链，每条 1000 个热身后样本 ----
# 对应 qmd：beta-binomial-fit

stan_data <- list(N = 10,Y = 9)

# 定义模型
stan_model_code <- "
data {
  int<lower=0> N;
  int<lower=0, upper=N> Y;
}
parameters {
  real<lower=0, upper=1> theta;
}
model {
  theta ~ beta(2, 2);
  Y ~ binomial(N, theta);
}
"

# 进行采样
fit <- rstan::stan(
  model_code = stan_model_code,      # 定义的模型或模型文件路径
  data = stan_data,                  # 输入数据
  chains = 4,                        # 马尔可夫链数量
  iter = 2000,                       # 总迭代次数（每个链）
  warmup = 1000,                        # 热身迭代次数（不保存）
  seed = 84735,
  refresh = 0
)

draws <- rstan::extract(fit, pars = "theta")
sample_theta <- draws$theta

# ---- 轨迹图 ----
# 对应 qmd：trace

trace <- bayesplot::mcmc_trace(fit, pars = "theta") +
        papaja::theme_apa()
trace

# ---- 自相关图 ----
# 对应 qmd：autocorrelation

auto_r <- bayesplot::mcmc_acf(fit, pars = "theta") +
          papaja::theme_apa()

auto_r

# ---- R-hat、有效样本量比率与发散 ----
# 对应 qmd：diagnostics

bayesplot::rhat(fit, pars = "theta")
bayesplot::neff_ratio(fit, pars = "theta")
sampler_params <- rstan::get_sampler_params(fit, inc_warmup = FALSE)
sum(vapply(sampler_params, function(x) sum(x[, "divergent__"]), numeric(1)))

# ---- 重新采样并抽稀：4 条链，每条保留 200 个样本 ----
# 对应 qmd：thinned-fit

thinned_fit <- rstan::stan(
  fit = fit, data = stan_data,
  chains = 4, iter = 4000, warmup = 2000,
  thin = 10, seed = 84735, refresh = 0
)

# ---- 抽稀后的轨迹图 ----
# 对应 qmd：thinned-trace

thinned_trace <- bayesplot::mcmc_trace(thinned_fit, pars = "theta") +
        papaja::theme_apa()

thinned_trace

# ---- 比较抽稀前后的自相关 ----
# 对应 qmd：acf-comparison

p1 <- bayesplot::mcmc_acf_bar(fit, pars = "theta", lags = 10) +
      papaja::theme_apa() +
      ggplot2::ggtitle('Full Trace')

p2 <- bayesplot::mcmc_acf_bar(thinned_fit, pars = "theta", lags = 10) +
      papaja::theme_apa() +
      ggplot2::ggtitle('Thinned Trace')

p1 + p2

# ---- 先验、缩放似然与解析后验 ----
# 对应 qmd：posterior-curves

x <- seq(0, 1, length.out = 10001)
prior <- dbeta(x, 2, 2)
likelihood <- dbinom(9, 10, x)
scaled_likelihood <- likelihood / max(likelihood) * max(prior)
posterior <- dbeta(x, 11, 3)
curve_data <- data.frame(x, prior, scaled_likelihood, posterior)

# ---- 绘制先验、似然与后验 ----
# 对应 qmd：posterior-density

ggplot(curve_data, aes(x)) +
  geom_line(aes(y = prior, color = "Prior"), linewidth = 1) +
  geom_line(aes(y = scaled_likelihood, color = "Scaled likelihood"), linewidth = 1) +
  geom_line(aes(y = posterior, color = "Posterior"), linewidth = 1) +
  labs(x = expression(theta), y = "Density / scaled likelihood", color = NULL)

# ---- 比较 Beta(11,3) 与 Beta(101,21) 的 95% ETI ----
# 对应 qmd：credible-intervals-comparison

# 创建 Beta 分布的数据
x_values <- seq(0, 1, length.out = 100)
y_values1 <- dbeta(x_values, shape1 = 11, shape2 = 3)
y_values2 <- dbeta(x_values, shape1 = 101, shape2 = 21)

# 计算 95% 可信区间（CrI）
  lower_bound1_95 <- qbeta(0.025, shape1 = 11, shape2 = 3)
  upper_bound1_95 <- qbeta(0.975, shape1 = 11, shape2 = 3)
  lower_bound2_95 <- qbeta(0.025, shape1 = 101, shape2 = 21)
  upper_bound2_95 <- qbeta(0.975, shape1 = 101, shape2 = 21)

# 绘制 Beta 分布的 PDF
p1 <- ggplot2::ggplot(data.frame(x = x_values, y = y_values1),aes(x = x, y = y)) +
  geom_line(col = 'black', linewidth = 1) +
  geom_area(data = subset(data.frame(x = x_values, y = y_values1),
                           x >= lower_bound1_95 & x <= upper_bound1_95),
            aes(x = x, y = y), fill = "lightblue", alpha = 0.5) +
  labs( x = expression(theta), y = NULL, title = "Posterior\n(Beta(alpha = 11, beta = 3))")  +
  scale_y_continuous(expand = c(0,0),limit = c(0,12)) +
  papaja::theme_apa()

p2 <- ggplot2::ggplot(data.frame(x = x_values, y = y_values2),aes(x = x, y = y)) +
  geom_line(col = 'black', linewidth = 0.7) +
  geom_area(data = subset(data.frame(x = x_values, y = y_values2),
                           x >= lower_bound2_95 & x <= upper_bound2_95),
            aes(x = x, y = y), fill = "lightblue", alpha = 0.5) +
  labs( x = expression(theta), y = NULL, title = "Posterior\n(Beta(alpha = 101, beta = 21))")  +
  scale_y_continuous(expand = c(0,0),limit = c(0,12)) +
  papaja::theme_apa()

#并排显示
p1 + p2

# ---- Beta(101,21) 的 50%、95%、99% ETI ----
# 对应 qmd：credible-intervals-masses

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
  geom_line(col = 'black', linewidth = 1) +
  geom_area(data = subset(data.frame(x = x_values, y = y_values),
                           x >= lower_bound_50 & x <= upper_bound_50),
            aes(x = x, y = y), fill = "#bdd7e7", alpha = 0.5) +
  labs( x = expression(theta), y = NULL, title = "50% CrI for Beta(101, 21)")  +
  scale_y_continuous(expand = c(0,0),limit = c(0,12)) +
  xlim(0.5,1) +
  papaja::theme_apa()


p2 <- ggplot2::ggplot(data.frame(x = x_values, y = y_values),aes(x = x, y = y)) +
  geom_line(col = 'black', linewidth = 0.7) +
  geom_area(data = subset(data.frame(x = x_values, y = y_values),
                           x >= lower_bound_95 & x <= upper_bound_95),
            aes(x = x, y = y), fill = "#6baed6", alpha = 0.5) +
  labs( x = expression(theta), y = NULL, title = "95% CrI for Beta(101, 21)")  +
  scale_y_continuous(expand = c(0,0),limit = c(0,12)) +
  xlim(0.5,1) +
  papaja::theme_apa()

p3 <- ggplot2::ggplot(data.frame(x = x_values, y = y_values),aes(x = x, y = y)) +
  geom_line(col = 'black', linewidth = 0.7) +
  geom_area(data = subset(data.frame(x = x_values, y = y_values),
                           x >= lower_bound_99 & x <= upper_bound_99),
            aes(x = x, y = y), fill = "#3182bd", alpha = 0.5) +
  labs( x = expression(theta), y = NULL, title = "99% CrI for Beta(101, 21)")  +
  scale_y_continuous(expand = c(0,0)) +
  xlim(0.5,1) +
  papaja::theme_apa()

#并排显示
p1 + p2 + p3

# ---- Beta(101,21) 的 95% HDI ----
# 对应 qmd：hdi

set.seed(84735)
hdi_result <- bayestestR::hdi(rbeta(100000, 101, 21), ci = 0.95)
hdi_lower <- hdi_result$CI_low
hdi_upper <- hdi_result$CI_high
beta_df <- data.frame(x = seq(0.4, 1, length.out = 10001))
beta_df$y <- dbeta(beta_df$x, 101, 21)
p <- ggplot(beta_df, aes(x, y)) +
  geom_area(data = subset(beta_df, x >= hdi_lower & x <= hdi_upper),
            fill = "lightblue", alpha = 0.5) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "red") +
  labs(x = expression(theta), y = "Density", title = "95% HDI: Beta(101, 21)")
p

# ---- 95% HDI 与 ROPE [0.4,0.6] ----
# 对应 qmd：hdi-rope

pp <- p + annotate("segment", x = 0.4, xend = 0.6, y = 0, yend = 0,
             color = "#009e73", linewidth = 3)
pp

# ---- 区间假设：H0 为区间内，H_1 为区间外 ----
# 对应 qmd：interval-bayes-factor

# 数据来自 data/beta-binmial.csv（118 次判断、正确 99 次），故后验为 Beta(2 + 99, 2 + 118 - 99) = Beta(101, 21)
prior_h0 <- pbeta(0.6, 2, 2) - pbeta(0.4, 2, 2)
post_h0 <- pbeta(0.6, 101, 21) - pbeta(0.4, 101, 21)
bf_10 <- ((1 - post_h0) / post_h0) / ((1 - prior_h0) / prior_h0)

# ---- 低于 80% 的后验概率 ----
# 对应 qmd：directional-probability

post_left <- pbeta(0.8, 101, 21)

# ---- 方向假设：H_1 为 theta < 0.8，H0 为 theta >= 0.8 ----
# 对应 qmd：directional-prior

prior_left <- pbeta(0.8, 2, 2)
prior_odds <- prior_left / (1 - prior_left)
posterior_odds <- post_left / (1 - post_left)
bf_left_right <- posterior_odds / prior_odds

# ---- 与课件内联结果对应的数值汇总 ----

data.frame(
  posterior = c("Beta(11,3)", "Beta(101,21)"),
  ETI_95_low = c(lower_bound1_95, lower_bound2_95),
  ETI_95_high = c(upper_bound1_95, upper_bound2_95)
)

data.frame(HDI_95_low = hdi_lower, HDI_95_high = hdi_upper)

data.frame(
  prior_h0 = prior_h0,
  post_h0 = post_h0,
  BF_10_interval = bf_10,
  post_left = post_left,
  prior_left = prior_left,
  prior_odds = prior_odds,
  posterior_odds = posterior_odds,
  BF_10_direction = bf_left_right,
  BF_01_direction = 1 / bf_left_right
)
