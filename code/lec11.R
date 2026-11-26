# 第 11 讲：贝叶斯回归模型的评估与比较
# 在项目根目录运行 Rscript code/lec11.R。加 --exercise 可运行模型 4–6 的参考解答。
# 模型结构、先验和逐行留出的单位与 lec11.qmd 一致。

pacman::p_load(dplyr, knitr, rstan, loo)

# 汇总自我匹配实验的反应时并建立三个设计矩阵。

rt_trials <- utils::read.csv(here::here("data", "Kolvoort_2020_HBM_Exp1_Clean.csv"))
rt <- rt_trials %>%
  dplyr::group_by(Subject, Label, Matching) %>%
  dplyr::summarise(RT_sec = mean(RT_sec), .groups = "drop") %>%
  dplyr::mutate(
    Label = factor(Label, levels = 1:3, labels = c("Self", "Friend", "Stranger")),
    Matching = factor(Matching, levels = c("Matching", "Nonmatching"))
  )
utils::head(rt, 3)

rt_formulas <- list(
  model1 = RT_sec ~ Label,
  model2 = RT_sec ~ Label + Matching,
  model3 = RT_sec ~ Label * Matching
)
contrast_codes <- list(Label = "contr.treatment", Matching = "contr.treatment")
rt_designs <- lapply(rt_formulas, function(formula) {
  factors <- intersect(names(contrast_codes), all.vars(formula))
  stats::model.matrix(formula, data = rt, contrasts.arg = contrast_codes[factors])
})
utils::head(rt_designs$model3, 3)

utils::head(rt_trials[c("Subject", "Label", "Matching", "RT_ms", "RT_sec")])
stats::contr.treatment(levels(rt$Label))
stats::contr.treatment(levels(rt$Matching))

# 用同一高斯回归程序拟合三个模型。

gaussian_stan <- "
data {
  int<lower=1> N;
  int<lower=1> K;
  matrix[N, K] X;
  vector[N] y;
  vector[K] prior_mean;
  vector<lower=0>[K] prior_sd;
  real<lower=0> sigma_rate;
}
parameters {
  vector[K] beta;
  real<lower=0> sigma;
}
model {
  for (k in 1:K) {
    beta[k] ~ normal(prior_mean[k], prior_sd[k]);
  }
  sigma ~ exponential(sigma_rate);
  for (i in 1:N) {
    y[i] ~ normal(X[i] * beta, sigma);
  }
}
generated quantities {
  vector[N] y_rep;
  vector[N] log_lik;
  for (i in 1:N) {
    real mu_i = X[i] * beta;
    y_rep[i] = normal_rng(mu_i, sigma);
    log_lik[i] = normal_lpdf(y[i] | mu_i, sigma);
  }
}
"

rt_stan_data <- lapply(rt_designs, function(X) {
  list(N = nrow(X), K = ncol(X), X = X, y = rt$RT_sec,
       prior_mean = c(5, rep(0, ncol(X) - 1)),
       prior_sd = c(2, rep(1, ncol(X) - 1)), sigma_rate = 0.3)
})

fit_models <- function(model_data, model_code, section, stan_model = NULL) {
  sampling <- list(chains = 4L, iter = 3000L, warmup = 1000L, seed = 84735L)
  key <- digest::digest(list(
    model_code, model_data, sampling, R.version.string,
    as.character(utils::packageVersion("rstan"))
  ))
  cache_dir <- here::here("tmpdata", "lec11-r")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_file <- file.path(cache_dir, paste0(section, "-", key, ".rds"))
  if (file.exists(cache_file)) return(readRDS(cache_file))

  if (is.null(stan_model)) {
    stan_model <- rstan::stan_model(model_code = model_code)
  }
  fits <- lapply(model_data, function(data) {
    do.call(rstan::sampling, c(list(object = stan_model, data = data, refresh = 0L), sampling))
  })
  cache_temp <- tempfile(tmpdir = cache_dir, fileext = ".rds")
  on.exit(unlink(cache_temp), add = TRUE)
  saveRDS(fits, cache_temp)
  if (!file.rename(cache_temp, cache_file) && !file.exists(cache_file)) {
    stop("无法保存拟合结果：", cache_file)
  }
  fits
}

rt_fits <- fit_models(rt_stan_data, gaussian_stan, section = "reaction-time")

# 贝叶斯模型与 OLS 使用同一列编码和行顺序，区间分别按各自的定义计算。

rt_ols <- stats::lm(rt_formulas$model3, data = rt, contrasts = contrast_codes)
stopifnot(identical(stats::model.matrix(rt_ols), rt_designs$model3))
ols_coefficients <- stats::coef(summary(rt_ols))
ols_ci <- stats::confint(rt_ols, level = 0.95)

X <- rt_designs$model3
ols_matrix <- stats::lm(rt$RT_sec ~ X - 1)
stopifnot(isTRUE(all.equal(unname(stats::coef(ols_matrix)),
                           unname(stats::coef(rt_ols)))))

bayes <- rstan::summary(rt_fits$model3, pars = "beta",
                        probs = c(0.025, 0.975))$summary
interval <- function(lower, upper) sprintf("[%.3f, %.3f]", lower, upper)
coefficient_table <- data.frame(
  parameter = c("截距", "朋友", "陌生人", "不匹配", "朋友 × 不匹配", "陌生人 × 不匹配"),
  posterior_mean = sprintf("%.3f", bayes[, "mean"]),
  posterior_sd = sprintf("%.3f", bayes[, "sd"]),
  credible_interval = interval(bayes[, "2.5%"], bayes[, "97.5%"]),
  ols_estimate = sprintf("%.3f", ols_coefficients[, "Estimate"]),
  ols_se = sprintf("%.3f", ols_coefficients[, "Std. Error"]),
  confidence_interval = interval(ols_ci[, 1], ols_ci[, 2])
)
print(knitr::kable(
  coefficient_table, format = "pipe",
  col.names = c("参数", "后验均值", "后验 SD", "可信区间", "OLS 估计", "OLS SE", "置信区间")
))

sigma_summary <- rstan::summary(rt_fits$model3, pars = "sigma",
                                probs = c(0.025, 0.975))$summary
ols_summary <- summary(rt_ols)
ols_f <- ols_summary$fstatistic

print(knitr::kable(
  data.frame(
    残差SD后验均值 = sigma_summary[1, "mean"],
    残差SD后验标准差 = sigma_summary[1, "sd"],
    残差SD可信区间 = interval(sigma_summary[1, "2.5%"], sigma_summary[1, "97.5%"]),
    R2 = ols_summary$r.squared,
    调整后R2 = ols_summary$adj.r.squared,
    F = unname(ols_f[1]),
    p = format.pval(stats::pf(ols_f[1], ols_f[2], ols_f[3], lower.tail = FALSE), digits = 3),
    对数似然 = as.numeric(stats::logLik(rt_ols)),
    AIC = stats::AIC(rt_ols),
    BIC = stats::BIC(rt_ols)
  ),
  digits = 3
))

# 预测评估：样本内 MedAE、逐行 PSIS-LOO 和高斯模型 DIC。

predictive_draws <- rstan::extract(rt_fits$model1, pars = "y_rep")$y_rep
predicted_rt <- colMeans(predictive_draws)
utils::head(data.frame(observed = rt$RT_sec, predicted = predicted_rt), 4)

calculate_medae <- function(fit, observed) {
  draws <- rstan::extract(fit, pars = "y_rep")$y_rep
  stopifnot(ncol(draws) == length(observed))
  stats::median(abs(observed - colMeans(draws)))
}

medae <- vapply(rt_fits, calculate_medae, numeric(1), observed = rt$RT_sec)
print(knitr::kable(data.frame(模型 = names(medae), MedAE秒 = unname(medae)),
             format = "pipe", digits = 4))

calculate_loo <- function(fit) {
  log_lik <- loo::extract_log_lik(fit, merge_chains = FALSE)
  r_eff <- loo::relative_eff(exp(log_lik))
  loo::loo(log_lik, r_eff = r_eff)
}

rt_loo <- lapply(rt_fits, calculate_loo)
rt_loo$model3$estimates
loo::pareto_k_table(rt_loo$model3)

rt_loo_comparison <- as.data.frame(loo::loo_compare(rt_loo))
if (!"model" %in% names(rt_loo_comparison)) {
  rt_loo_comparison$model <- rownames(rt_loo_comparison)
}
print(knitr::kable(rt_loo_comparison[c("model", "elpd_diff", "se_diff")],
             format = "pipe", digits = 2, row.names = FALSE))

calculate_dic <- function(fit, model_data) {
  log_lik <- loo::extract_log_lik(fit)
  mean_deviance <- mean(-2 * rowSums(log_lik))

  draws <- rstan::extract(fit, pars = c("beta", "sigma"))
  mean_mu <- drop(model_data$X %*% colMeans(draws$beta))
  plug_in_deviance <- -2 * sum(stats::dnorm(
    model_data$y, mean = mean_mu, sd = mean(draws$sigma), log = TRUE
  ))

  mean_deviance + (mean_deviance - plug_in_deviance)
}

dic <- mapply(calculate_dic, rt_fits, rt_stan_data)
model_order <- match(names(rt_fits), rt_loo_comparison$model)
elpd_diff <- rt_loo_comparison$elpd_diff[model_order]
stopifnot(all(is.finite(elpd_diff)))
print(knitr::kable(
  data.frame(模型 = names(rt_fits), MedAE秒 = unname(medae),
             ELPD差 = elpd_diff, DIC = unname(dic)),
  format = "pipe", digits = 4
))

# 练习数据与课件使用同一筛选规则。

exercise_raw <- utils::read.csv(here::here("data", "Data_Sum_HPP_Multi_Site_Share.csv"))
exercise <- exercise_raw %>%
  dplyr::filter(Site == "Tsinghua", smoke %in% c(1L, 2L),
                is.finite(stress), is.finite(scontrol)) %>%
  dplyr::transmute(stress, scontrol, smoke = as.integer(smoke == 1L))
utils::head(exercise, 4)

# Part 5–6：用 brms 拟合三个候选模型并提取后验结果。
if ("--exercise" %in% commandArgs(trailingOnly = TRUE)) {
  # 让截距先验对应原始压力为 0 时的预期分数，与课件一致。
  exercise_formulas <- list(
    model4 = scontrol ~ 0 + Intercept + stress,
    model5 = scontrol ~ 0 + Intercept + stress + smoke,
    model6 = scontrol ~ 0 + Intercept + stress * smoke
  )
  exercise_priors <- c(
    brms::set_prior("normal(0, 10)", class = "b"),
    brms::set_prior("normal(50, 10)", class = "b", coef = "Intercept"),
    brms::set_prior("exponential(0.6)", class = "sigma")
  )
  exercise_cache <- here::here("tmpdata", "lec11-brms")
  dir.create(exercise_cache, recursive = TRUE, showWarnings = FALSE)
  exercise_fits <- lapply(names(exercise_formulas), function(name) {
    brms::brm(
      formula = exercise_formulas[[name]],
      data = exercise,
      family = stats::gaussian(link = "identity"),
      prior = exercise_priors,
      backend = "rstan",
      chains = 4, iter = 3000, warmup = 1000,
      seed = 84735, refresh = 0,
      file = file.path(exercise_cache, name),
      file_refit = "on_change"
    )
  })
  names(exercise_fits) <- names(exercise_formulas)

  # Part 6：按模型查看后验摘要，保留逐人预测和逐点对数似然。
  for (name in names(exercise_fits)) {
    cat("\n", name, "\n", sep = "")
    print(summary(exercise_fits[[name]]))
  }
  set.seed(84735)
  exercise_y_rep <- lapply(exercise_fits, brms::posterior_predict)
  exercise_log_lik <- lapply(exercise_fits, brms::log_lik)
  print(list(y_rep4 = dim(exercise_y_rep$model4),
             log_lik4 = dim(exercise_log_lik$model4), n = nrow(exercise)))
  print(utils::head(colMeans(exercise_y_rep$model5), 4))

  # Part 5：比较样本内 MedAE、逐人 PSIS-LOO 与 DIC。
  exercise_medae <- vapply(exercise_y_rep, function(draws) {
    stats::median(abs(exercise$scontrol - colMeans(draws)))
  }, numeric(1))
  exercise_loo <- lapply(exercise_fits, loo::loo)
  exercise_dic <- vapply(names(exercise_fits), function(name) {
    fit <- exercise_fits[[name]]
    mean_deviance <- mean(-2 * rowSums(exercise_log_lik[[name]]))
    mean_mu <- colMeans(brms::posterior_epred(fit))
    sigma_draws <- posterior::as_draws_df(fit, variable = "sigma")
    plug_in_deviance <- -2 * sum(stats::dnorm(
      exercise$scontrol, mean = mean_mu, sd = mean(sigma_draws$sigma), log = TRUE
    ))
    2 * mean_deviance - plug_in_deviance
  }, numeric(1))
  print(knitr::kable(data.frame(
    模型 = names(exercise_fits), MedAE = unname(exercise_medae), DIC = unname(exercise_dic)
  ), digits = 4))
  print(loo::loo_compare(exercise_loo))
}
