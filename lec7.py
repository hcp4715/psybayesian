"""第7讲：与 lec7.qmd 对应的 PyMC、SciPy 和 ArviZ 实现。

依赖：numpy、scipy、matplotlib、pymc 5.x、arviz 0.x。
R 的 iter 包含 warmup；PyMC 的 draws 仅指热身后样本数。
"""

import arviz as az
import matplotlib.pyplot as plt
from matplotlib.axes import Axes
import numpy as np
import pymc as pm
from scipy import stats


def bayesian_analysis_plot(
    alpha: float,
    beta: float,
    y: int,
    n: int,
    ax: Axes | None = None,
    plot_prior: bool = True,
    plot_likelihood: bool = True,
    plot_posterior: bool = True,
    xlabel: str = r"$\theta$",
    show_legend: bool = True,
    legend_loc: str = "upper left",
) -> Axes:
    """固定观测成功次数 y，绘制先验、按峰高缩放的似然及解析后验。"""
    if (
        not np.isfinite([alpha, beta]).all()
        or alpha <= 0
        or beta <= 0
        or n <= 0
        or not 0 <= y <= n
    ):
        raise ValueError("先验形状参数与 n 必须为正，且成功次数须满足 0 <= y <= n。")
    if ax is None:
        ax = plt.gca()

    x = np.linspace(0, 1, 10001)
    prior = stats.beta.pdf(x, alpha, beta)
    likelihood = stats.binom.pmf(y, n, x)
    prior_peak = prior[np.isfinite(prior)].max()
    scaled_likelihood = likelihood / likelihood.max() * prior_peak
    posterior = stats.beta.pdf(x, alpha + y, beta + n - y)
    if plot_prior:
        ax.plot(x, prior, label="Prior", linewidth=1)
    if plot_likelihood:
        ax.plot(x, scaled_likelihood, label="Scaled likelihood", linewidth=1)
    if plot_posterior:
        ax.plot(x, posterior, label="Posterior", linewidth=1)
    ax.set_xlabel(xlabel)
    ax.set_ylabel("Density / scaled likelihood")
    if show_legend:
        ax.legend(loc=legend_loc)
    ax.spines[["top", "right"]].set_visible(False)
    return ax


# ---- 全局设置与未归一化后验（qmd：setup、posterior-kernel） ----
SEED = 84735
np.random.seed(SEED)
plt.rcParams.update({"font.size": 12, "axes.spines.top": False, "axes.spines.right": False})

N = 10
Y = 9
theta_grid = np.linspace(0, 1, 10000)
prior_density = stats.beta.pdf(theta_grid, 2, 2)
likelihood = stats.binom.pmf(Y, N, theta_grid)
posterior_unstandardized = likelihood * prior_density


# ---- Beta-Binomial 采样（qmd：beta-binomial-fit） ----
with pm.Model() as bb_model:
    theta = pm.Beta("theta", alpha=2, beta=2)
    observed_y = pm.Binomial("Y", n=N, p=theta, observed=Y)
    trace = pm.sample(
        draws=1000, tune=1000, chains=4, cores=1,
        random_seed=SEED, return_inferencedata=True,
    )

sample_theta = trace.posterior["theta"].values.reshape(-1)

az.plot_trace(trace, var_names=["theta"], legend=True)
plt.show()
az.plot_autocorr(trace, var_names=["theta"], max_lag=20, combined=False)
plt.show()

# 沿用 qmd 的传统 split-Rhat 和基本 ESS 口径；两种实现不要求数值逐位相同。
rhat = az.rhat(trace, var_names=["theta"], method="split")
neff_ratio = az.ess(trace, var_names=["theta"], method="identity", relative=True)
divergences = int(trace.sample_stats["diverging"].sum().item())
diagnostics = {
    "rhat": rhat["theta"].item(),
    "neff_ratio": neff_ratio["theta"].item(),
    "divergences": divergences,
}
diagnostics


# ---- 重新采样并抽稀（qmd：thinned-fit、thinned-trace、acf-comparison） ----
# 对应 R 的 iter=4000、warmup=2000、thin=10；每条链最终保留 200 个样本。
with bb_model:
    thinned_raw = pm.sample(
        draws=2000, tune=2000, chains=4, cores=1,
        random_seed=SEED, return_inferencedata=True,
    )
thinned_trace = thinned_raw.isel(draw=slice(None, None, 10))

az.plot_trace(thinned_trace, var_names=["theta"], legend=True)
plt.show()
fig, axes = plt.subplots(1, 2, figsize=(12, 4.5))
az.plot_autocorr(trace, var_names=["theta"], max_lag=10, combined=True, ax=axes[0])
axes[0].set_title("Full Trace")
az.plot_autocorr(thinned_trace, var_names=["theta"], max_lag=10, combined=True, ax=axes[1])
axes[1].set_title("Thinned Trace")
plt.tight_layout()
plt.show()


# ---- 先验、缩放似然与解析后验（qmd：posterior-curves、posterior-density） ----
x = np.linspace(0, 1, 10001)
prior = stats.beta.pdf(x, 2, 2)
likelihood = stats.binom.pmf(9, 10, x)
scaled_likelihood = likelihood / likelihood.max() * prior.max()
posterior = stats.beta.pdf(x, 11, 3)

fig, ax = plt.subplots(figsize=(12, 4.5))
bayesian_analysis_plot(alpha=2, beta=2, y=9, n=10, ax=ax)
plt.tight_layout()
plt.show()


# ---- 两个后验的 95% 等尾区间（qmd：credible-intervals-comparison） ----
# 绘图网格与现有 qmd 保持一致；精确区间边界由分位数函数计算。
params = [(11, 3), (101, 21)]
ci_level = 0.95
x_values = np.linspace(0, 1, 100)
eti_comparison = {}
fig, axes = plt.subplots(1, 2, figsize=(12, 4.5), sharey=True)
for ax, (a, b) in zip(axes, params):
    density = stats.beta.pdf(x_values, a, b)
    lower, upper = stats.beta.ppf([0.025, 0.975], a, b)
    eti_comparison[(a, b)] = (lower, upper)
    selected = (x_values >= lower) & (x_values <= upper)
    ax.plot(x_values, density, color="black", linewidth=1)
    ax.fill_between(x_values[selected], density[selected], color="lightblue", alpha=0.5)
    ax.set_title(f"Posterior\n(Beta(alpha = {a}, beta = {b}))")
    ax.set_xlabel(r"$\theta$")
    ax.set_ylim(0, 12)
plt.tight_layout()
plt.show()


# ---- 50%、95%、99% 等尾区间（qmd：credible-intervals-masses） ----
ci_levels = {
    "50% CrI": (0.25, 0.75),
    "95% CrI": (0.025, 0.975),
    "99% CrI": (0.005, 0.995),
}
ci_colors = ["#bdd7e7", "#6baed6", "#3182bd"]
eti_intervals = {}
density = stats.beta.pdf(x_values, 101, 21)
fig, axes = plt.subplots(1, 3, figsize=(12, 4.5))
for i, (name, probabilities) in enumerate(ci_levels.items()):
    lower, upper = stats.beta.ppf(probabilities, 101, 21)
    eti_intervals[name] = (lower, upper)
    selected = (x_values >= lower) & (x_values <= upper)
    axes[i].plot(x_values, density, color="black", linewidth=1 if i == 0 else 0.7)
    axes[i].fill_between(
        x_values[selected], density[selected], color=ci_colors[i], alpha=0.5,
    )
    axes[i].set_title(f"{name} for Beta(101, 21)")
    axes[i].set_xlabel(r"$\theta$")
    axes[i].set_xlim(0.5, 1)
    axes[i].set_ylim(0, 12 if i < 2 else density.max())
plt.tight_layout()
plt.show()


# ---- 95% HDI 与 ROPE（qmd：hdi、hdi-rope） ----
np.random.seed(SEED)
samples = np.random.beta(101, 21, 100000)
hdi_lower, hdi_upper = az.hdi(samples, hdi_prob=0.95)
hdi_result = {"CI_low": hdi_lower, "CI_high": hdi_upper}
x_hdi = np.linspace(0.4, 1, 10001)
density_hdi = stats.beta.pdf(x_hdi, 101, 21)
selected_hdi = (x_hdi >= hdi_lower) & (x_hdi <= hdi_upper)

fig, ax = plt.subplots(figsize=(12, 4.5))
ax.fill_between(x_hdi[selected_hdi], density_hdi[selected_hdi], color="lightblue", alpha=0.5)
ax.plot(x_hdi, density_hdi, color="black", linewidth=1)
ax.axvline(0.5, color="red", linestyle="--")
ax.set(xlabel=r"$\theta$", ylabel="Density", title="95% HDI: Beta(101, 21)")
plt.tight_layout()
plt.show()

# 两张图使用同一份后验样本及同一个 HDI，避免比较时引入重新抽样的差别。
fig, ax = plt.subplots(figsize=(12, 4.5))
ax.fill_between(x_hdi[selected_hdi], density_hdi[selected_hdi], color="lightblue", alpha=0.5)
ax.plot(x_hdi, density_hdi, color="black", linewidth=1)
ax.axvline(0.5, color="red", linestyle="--")
ax.plot([0.4, 0.6], [0, 0], color="#009e73", linewidth=3)
ax.set(xlabel=r"$\theta$", ylabel="Density", title="95% HDI: Beta(101, 21)")
plt.tight_layout()
plt.show()


# ---- 区间假设与方向假设（qmd：interval-bayes-factor、directional-*） ----
# 区间假设：H0 为 theta 在 [0.4,0.6] 内，H_1 为区间外。
# 数据来自 data/beta-binmial.csv（118 次判断、正确 99 次），故后验为 Beta(101, 21)
prior_h0 = stats.beta.cdf(0.6, 2, 2) - stats.beta.cdf(0.4, 2, 2)
post_h0 = stats.beta.cdf(0.6, 101, 21) - stats.beta.cdf(0.4, 101, 21)
bf_10 = ((1 - post_h0) / post_h0) / ((1 - prior_h0) / prior_h0)

# 方向假设：H_1 为 theta < 0.8，H0 为 theta >= 0.8。
post_left = stats.beta.cdf(0.8, 101, 21)
prior_left = stats.beta.cdf(0.8, 2, 2)
prior_odds = prior_left / (1 - prior_left)
posterior_odds = post_left / (1 - post_left)
bf_left_right = posterior_odds / prior_odds
results = {
    "prior_h0": prior_h0,
    "post_h0": post_h0,
    "BF_10_interval": bf_10,
    "post_left": post_left,
    "prior_left": prior_left,
    "prior_odds": prior_odds,
    "posterior_odds": posterior_odds,
    "BF_10_direction": bf_left_right,
    "BF_01_direction": 1 / bf_left_right,
    "HDI_95_low": hdi_lower,
    "HDI_95_high": hdi_upper,
}
results


# ---- Python 代码核验说明 ----
# 1. 数据、Beta(2,2) 先验、theta 参数、种子 84735、ETI 及 ROPE 与 qmd 一致。
# 2. 首次采样每链 draws=1000、tune=1000；抽稀演示重新采样每链 2000+2000 次，
#    再每隔 10 个保留一个，对应 R 的 4000 次总迭代、2000 次热身及 thin=10。
# 3. 原程序将二项数据分布当作似然曲线；现固定 Y=9，在 theta 网格上计算
#    binom.pmf(9,10,theta)，并按 qmd 的先验峰高进行缩放。绘图使用 SciPy，免去 preliz。
# 4. ETI 为 95% 以及 50%/95%/99%；HDI 为 95%，使用固定种子的 100000 个 Beta 样本。
# 5. R/Stan 与 PyMC/ArviZ 的采样器、随机数生成器和诊断实现不同；抽样结果、
#    HDI 端点和诊断数值允许波动，解析分位数、后验概率与 BF 应在浮点精度内一致。
# 6. 当前 qmd 的 100 点区间着色网格及抽稀演示设置保留；本次未改写课件中的实现。
# 7. 已在 PyMC 5.28.5 / ArviZ 0.23.4 下完整运行：两次采样和 9 幅图均通过核验。
#    首次保留 4000 个样本，抽稀后保留 800 个；分位数、后验概率和 BF 已与 R 交叉核验。
