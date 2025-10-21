import pymc as pm
import arviz as az
import matplotlib.pyplot as plt

# 设置数据
N = 10; Y = 9
# 创建PyMC模型
bb_model = pm.Model()

with bb_model:
    pi = pm.Beta('pi', alpha=2, beta=2)
    likelihood = pm.Binomial('likelihood', n=N, p=pi, observed=Y)

#采样过程仍在该容器中进行
with bb_model:
    trace = pm.sample(draws=5000,                   # 设置MCMC采样样本的数量
                      tune=0,                       # 设置调试样本的数量
                      chains=4,                     # 设置4条MCMC链
                      random_seed=84735)


ax = az.plot_trace(trace, legend = True)

ax = az.plot_autocorr(trace, max_lag=20, combined=True)

az.summary(trace, kind = "diagnostics")

thinned_trace = trace.sel(draw=slice(None, None, 10))

# 注意，thinned_trace 只有 500个采样
thinned_trace.posterior

fig, axes = plt.subplots(1, 2, figsize=(7, 3))

az.plot_autocorr(trace, max_lag=10, combined=True, ax=axes[0])
axes[0].set_title('Full Trace')
az.plot_autocorr(thinned_trace, max_lag=10, combined=True, ax=axes[1])
axes[1].set_title('Thinned trace')

plt.tight_layout()
plt.show()

# 导入数字和向量处理包：numpy
import numpy as np
# 导入基本绘图工具：matplotlib
import matplotlib.pyplot as plt
# 导入高级绘图工具 seaborn 为 sns
import seaborn as sns
# 导入概率分布计算和可视化包：preliz
import preliz as pz


def bayesian_analysis_plot(
        alpha, beta, y, n,
        ax=None,
        plot_prior=True,
        plot_likelihood=True,
        plot_posterior=True,
        xlabel=r"$\pi$",
        show_legend=True,
        legend_loc="upper left"):
    """
    该函数绘制先验分布、似然分布和后验分布的 PDF 图示在指定的子图上。

    参数:
    - alpha: Beta 分布的 alpha 参数（先验）
    - beta: Beta 分布的 beta 参数（先验）
    - y: 观测数据中的支持次数
    - n: 总样本数
    - ax: 子图对象，在指定子图上绘制图形
    """

    if ax is None:
        ax = plt.gca()

    if plot_prior:
        # 先验分布
        prior = pz.Beta(alpha, beta)
        prior.plot_pdf(color="black", ax=ax, legend="None")
        x_prior = np.linspace(prior.ppf(0.0001), prior.ppf(0.9999), 100)
        ax.fill_between(x_prior, prior.pdf(x_prior), color="#f0e442", alpha=0.5, label="prior")

    if plot_likelihood:
        # 似然分布 (两种写法等价)
        # likelihood = pz.Beta(y,n-y)
        # likelihood.plot_pdf(color="black", ax=ax, legend="None")
        x = np.linspace(0, 1, 1000)
        likelihood = pz.Binomial(n=n, p=y / n).pdf(x=x * n)
        likelihood = likelihood * n
        ax.plot(x, likelihood, color="black", label=r"$\mathbf{Binomial}$" + rf"(n={n},p={round(y / n, 2)})")
        ax.fill_between(x, likelihood, color="#0071b2", alpha=0.5, label="likelihood")

    if plot_posterior:
        # 后验分布
        posterior = pz.Beta(alpha + y, beta + n - y)
        posterior.plot_pdf(color="black", ax=ax, legend="None")
        x_posterior = np.linspace(posterior.ppf(0.0001), posterior.ppf(0.9999), 100)
        ax.fill_between(x_posterior, posterior.pdf(x_posterior), color="#009e74", alpha=0.5, label="posterior")

    if show_legend:
        ax.legend(loc=legend_loc)
    else:
        ax.legend().set_visible(False)

    # 设置图形
    ax.set_xlabel(xlabel)
    sns.despine()

# 创建一个单独的图和轴
fig, ax = plt.subplots(figsize=(7, 5))

# 先验参数 alpha=2, beta=2, 观测数据 y=9, n=10
bayesian_analysis_plot(alpha=2, beta=2, y=9, n=10, ax=ax)

# 显示图像
plt.tight_layout()
plt.show()

import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import beta

# 定义 Beta 分布的参数 Beta(101, 21)
params = [(11, 3), (101, 21)]
titles = [r"Beta(11, 3)", r"Beta(101, 21)"]
ci_level = 0.95

# 创建图形和子图
fig, axes = plt.subplots(1, 2, figsize=(8, 3), sharey=True)

# 绘制 Beta 分布的 PDF 和 95% CI 区间
for i, (a, b) in enumerate(params):
    x = np.linspace(0, 1, 1000)
    y = beta.pdf(x, a, b)
    ci_lower, ci_upper = beta.ppf([(1 - ci_level) / 2, (1 + ci_level) / 2], a, b)
    x2 = np.linspace(ci_lower, ci_upper, 1000)
    y2 = beta.pdf(x2, a, b)

    # 绘图
    axes[i].plot(x, y, color='black', label=f'{int(ci_level * 100)}% CI')
    axes[i].fill_between(x2, y2, color='#a6bddb', alpha=0.6)
    axes[i].set_title(titles[i])
    axes[i].set_xlabel(r"$\pi$")
    axes[i].set_ylabel("density")
    axes[i].set_ylim(0, 12)

    # 隐藏上部和右部的边框
    axes[i].spines['top'].set_visible(False)
    axes[i].spines['right'].set_visible(False)

    # 添加图例
    axes[i].legend()

# 调整子图布局
plt.tight_layout()
plt.show()

import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import beta

# 定义 Beta 分布的参数 Beta(101, 21)
a, b = 101, 21
x = np.linspace(0, 1, 1000)
y = beta.pdf(x, a, b)

# 设置三个不同的可信区间
ci_levels = {
    "50% CI": (0.25, 0.75),
    "95% CI": (0.025, 0.975),
    "99% CI": (0.005, 0.995)
}
ci_colors = ['#bdd7e7', '#6baed6', '#3182bd']  # Colors for 50%, 95%, and 99% CI

# 为每个可信区间创建子图
fig, axes = plt.subplots(1, 3, figsize=(12, 3), sharey=True)

# 绘制每个可信区间
for i, (ci_name, (lower_percentile, upper_percentile)) in enumerate(ci_levels.items()):
    # 计算可信区间的上下界
    ci_lower, ci_upper = beta.ppf([lower_percentile, upper_percentile], a, b)

    # 绘制分布曲线并填充可信区间
    axes[i].plot(x, y, color='black')
    axes[i].fill_between(x, y, where=(x >= ci_lower) & (x <= ci_upper), color=ci_colors[i], alpha=0.6)
    axes[i].set_title(f"{ci_name} for Beta(101, 21)")
    axes[i].set_xlabel(r"$\pi$")
    axes[i].set_ylim(0, 12)

    # 绘制可信区间的垂直线
    y_ci_lower = beta.pdf(ci_lower, a, b)
    y_ci_upper = beta.pdf(ci_upper, a, b)
    axes[i].axvline(ci_lower, color='black', linestyle='-', ymax=y_ci_lower / max(y))
    axes[i].axvline(ci_upper, color='black', linestyle='-', ymax=y_ci_upper / max(y))

    # 移除顶部和右侧的边框
    axes[i].spines['top'].set_visible(False)
    axes[i].spines['right'].set_visible(False)

    axes[i].set_xlim(0.5, 1)

# 设置 y 轴标签
axes[0].set_ylabel("density")

# 调整布局
plt.tight_layout()
plt.show()

import numpy as np
import matplotlib.pyplot as plt
import arviz as az
import scipy.stats as stats

# 生成 Beta 分布的样本
alpha = 101
beta = 21
samples = np.random.beta(alpha, beta, 10000)

# 使用 arviz 绘制后验分布图
az.plot_posterior(samples, hdi_prob=0.95)

# 添加 x=0.5的竖线
plt.axvline(x=0.5, color='red', linestyle='--')

# 显示图形
plt.show()

import numpy as np
import matplotlib.pyplot as plt
import arviz as az

# 生成 Beta 分布的样本
alpha = 101
beta = 21
samples = np.random.beta(alpha, beta, 10000)

# 使用 arviz 绘制后验分布图，并添加 ROPE 区间
az.plot_posterior(samples, hdi_prob=0.95, rope=[0.4, 0.6])

# 显示图形
plt.show()