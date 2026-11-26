#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: setup
#| include: false

pacman::p_load(dplyr, knitr, rstan, loo)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: rt-data

rt_trials <- utils::read.csv("data/Kolvoort_2020_HBM_Exp1_Clean.csv")
rt <- rt_trials |>
  dplyr::group_by(Subject, Label, Matching) |>
  dplyr::summarise(RT_sec = mean(RT_sec), .groups = "drop") |>
  dplyr::mutate(
    Label = factor(Label, levels = 1:3, labels = c("Self", "Friend", "Stranger")),
    Matching = factor(Matching, levels = c("Matching", "Nonmatching"))
  )
utils::head(rt, 3)
#
#
#
#
#
#
#
#
#
#
#
#| label: rt-design

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
#
#
#
#
#
#
#
#
#
#| label: inspect-rt-inputs

utils::head(rt_trials[c("Subject", "Label", "Matching", "RT_ms", "RT_sec")])
stats::contr.treatment(levels(rt$Label))
stats::contr.treatment(levels(rt$Matching))
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: gaussian-stan

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
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: rt-stan-data

rt_stan_data <- lapply(rt_designs, function(X) {
  list(N = nrow(X), K = ncol(X), X = X, y = rt$RT_sec,
       prior_mean = c(5, rep(0, ncol(X) - 1)),
       prior_sd = c(2, rep(1, ncol(X) - 1)), sigma_rate = 0.3)
})
#
#
#
#
#
#
#
#
#
#| results: hide

rt_stan_model <- rstan::stan_model(model_code = gaussian_stan)
rt_fits <- lapply(rt_stan_data, function(model_data) {
  rstan::sampling(rt_stan_model, data = model_data, chains = 4,
                  iter = 3000, warmup = 1000, seed = 84735, refresh = 0)
})
#
#
#
#
#
#
#
#
#
#| label: rt-ols

rt_ols <- stats::lm(rt_formulas$model3, data = rt, contrasts = contrast_codes)
stopifnot(identical(stats::model.matrix(rt_ols), rt_designs$model3))
ols_coefficients <- stats::coef(summary(rt_ols))
ols_ci <- stats::confint(rt_ols, level = 0.95)
#
#
#
#
#
#
#
#| label: ols-explicit-design

X <- rt_designs$model3
ols_matrix <- stats::lm(rt$RT_sec ~ X - 1)
stopifnot(isTRUE(all.equal(unname(stats::coef(ols_matrix)),
                           unname(stats::coef(rt_ols)))))
#
#
#
#
#
#
#
#| label: coefficient-comparison
#| echo: false

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
knitr::kable(
  coefficient_table, format = "pipe",
  col.names = c("参数", "后验均值", "后验 SD", "可信区间", "OLS 估计", "OLS SE", "置信区间")
)
#
#
#
#
#
#
#
#| label: regression-diagnostics
#| include: false

sigma_summary <- rstan::summary(rt_fits$model3, pars = "sigma",
                                probs = c(0.025, 0.975))$summary
ols_summary <- summary(rt_ols)
ols_f <- ols_summary$fstatistic
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: predictive-means

predictive_draws <- rstan::extract(rt_fits$model1, pars = "y_rep")$y_rep
predicted_rt <- colMeans(predictive_draws)
utils::head(data.frame(observed = rt$RT_sec, predicted = predicted_rt), 4)
#
#
#
#
#
#
#
#
#
#| label: predictive-means-tidybayes
#| eval: false

posterior_predictive <- tidybayes::spread_draws(rt_fits$model1, y_rep[i])
posterior_mean <- posterior_predictive |>
  dplyr::group_by(i) |>
  dplyr::summarise(predicted = mean(y_rep), .groups = "drop") |>
  dplyr::arrange(i)
utils::head(posterior_mean, 5)
stats::median(abs(rt$RT_sec[posterior_mean$i] - posterior_mean$predicted))
#
#
#
#
#
#
#
#| label: calculate-medae

calculate_medae <- function(fit, observed) {
  draws <- rstan::extract(fit, pars = "y_rep")$y_rep
  stopifnot(ncol(draws) == length(observed))
  stats::median(abs(observed - colMeans(draws)))
}

medae <- vapply(rt_fits, calculate_medae, numeric(1), observed = rt$RT_sec)
knitr::kable(data.frame(模型 = names(medae), MedAE秒 = unname(medae)),
             format = "pipe", digits = 4)
#
#
#
#
#
#
#
#
#
#
#
#| label: calculate-loo

calculate_loo <- function(fit) {
  log_lik <- loo::extract_log_lik(fit, merge_chains = FALSE)
  r_eff <- loo::relative_eff(exp(log_lik))
  loo::loo(log_lik, r_eff = r_eff)
}

rt_loo <- lapply(rt_fits, calculate_loo)
rt_loo$model3$estimates
loo::pareto_k_table(rt_loo$model3)
#
#
#
#
#
#| label: compare-loo

rt_loo_comparison <- as.data.frame(loo::loo_compare(rt_loo))
if (!"model" %in% names(rt_loo_comparison)) {
  rt_loo_comparison$model <- rownames(rt_loo_comparison)
}
knitr::kable(rt_loo_comparison[c("model", "elpd_diff", "se_diff")],
             format = "pipe", digits = 2, row.names = FALSE)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: calculate-dic

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
#
#
#
#
#
#| label: compare-criteria

dic <- mapply(calculate_dic, rt_fits, rt_stan_data)
model_order <- match(names(rt_fits), rt_loo_comparison$model)
elpd_diff <- rt_loo_comparison$elpd_diff[model_order]
stopifnot(all(is.finite(elpd_diff)))
knitr::kable(
  data.frame(模型 = names(rt_fits), MedAE秒 = unname(medae),
             ELPD差 = elpd_diff, DIC = unname(dic)),
  format = "pipe", digits = 4
)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: exercise-data

exercise_raw <- utils::read.csv("data/Data_Sum_HPP_Multi_Site_Share.csv")
exercise <- exercise_raw |>
  dplyr::filter(Site == "Tsinghua", smoke %in% c(1L, 2L),
                is.finite(stress), is.finite(scontrol)) |>
  dplyr::transmute(stress, scontrol, smoke = as.integer(smoke == 1L))
utils::head(exercise, 4)
#
#
#
#
#
#
#
#| label: exercise-formulas
#| eval: false

exercise_formulas <- list(
  model4 = scontrol ~ stress,
  model5 = scontrol ~ ...,    # 补全主效应
  model6 = scontrol ~ ...     # 补全主效应与交互项
)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: exercise-fit
#| eval: false

exercise_stan_data <- lapply(exercise_formulas, function(formula) {
  X <- stats::model.matrix(formula, data = exercise)
  list(N = nrow(X), K = ncol(X), X = X, y = exercise$scontrol,
       prior_mean = c(50, rep(0, ncol(X) - 1)),
       prior_sd = rep(10, ncol(X)), sigma_rate = 0.6)
})
exercise_fits <- lapply(exercise_stan_data, function(model_data) {
  rstan::sampling(rt_stan_model, data = model_data, chains = 4,
                  iter = 3000, warmup = 1000, seed = 84735, refresh = 0)
})
#
#
#
#
#
#
#
#
#
#| label: exercise-predictive-comparison
#| eval: false

vapply(exercise_fits, calculate_medae, numeric(1), observed = ...)
exercise_loo <- lapply(exercise_fits, calculate_loo)
loo::loo_compare(exercise_loo)
#
#
#
#
#
#
#
#
#
#| label: exercise-dic
#| eval: false

mapply(calculate_dic, exercise_fits, ...)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
