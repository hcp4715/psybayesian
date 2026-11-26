import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

# ===========================================================================
#    Normal-Normal 例子：解析解 vs PyMC 的 MCMC
#    模型：先验 mu ~ N(0, 1)，似然 Y ~ N(mu, 0.75^2)，观测到 Y = 6.25
# ===========================================================================

# --------------- 方法1：解析解（Normal-Normal 共轭） ---------------
# 后验均值 = (mu0*sigma^2 + ybar*n*tau^2) / (n*tau^2 + sigma^2)
# 后验方差 = tau^2*sigma^2 / (n*tau^2 + sigma^2)
post_mean = (0 * 0.75 ** 2 + 6.25 * 1 ** 2) / (1 ** 2 + 0.75 ** 2)   # 4
post_var = (1 ** 2 * 0.75 ** 2) / (1 ** 2 + 0.75 ** 2)                # 0.36
post_sd = np.sqrt(post_var)                                            # 0.6
print(f"解析解：后验 ~ N({post_mean}, {post_var})")

# --------------- 方法2：PyMC 的 MCMC ---------------
import pymc as pm
import arviz as az

with pm.Model() as nn_model_simple:
    mu = pm.Normal("mu", mu=0, sigma=1)                    # 先验
    y = pm.Normal("y", mu=mu, sigma=0.75, observed=6.25)   # 似然
    idata_nn = pm.sample(draws=1000,   # 每条链保留的样本数（对应 iter - warmup）
                         tune=1000,    # 对应 R/Stan 的 warmup
                         chains=4,
                         random_seed=202409)

print(az.summary(idata_nn, var_names=["mu"]))

# --------------- Step 4b：用 trace 图做视觉检查 ---------------
# ✅ 像"毛毛虫"：每条链在同一条水平带内快速抖动、互相重叠、没有趋势
az.plot_trace(idata_nn, var_names=["mu"])

x_range = np.linspace(2, 7, 1000)  # 设置 x 的范围为 [2,7]
y_norm = norm.pdf(x_range, 4, 0.6)  # 后验分布 N(4, 0.6^2) 的概率密度

# 从后验分布 N(4, 0.6^2) 中采样，得到1000个样本值
samples = np.random.normal(4, 0.6, 1000)

# 显示前 10 个样本值
first_10_samples = samples[:10]
print("前 10 个样本值：", first_10_samples)

#  绘制图像
plt.figure(figsize=(10, 6))
plt.plot(x_range, norm.pdf(x_range, 4, 0.6), linewidth=1.5, color='r', label='Posterior N(4, 0.6^2)')
count, bins, ignored = plt.hist(samples, bins=45, density=True, alpha=1, color='lightgrey', edgecolor='black',
                                label='Sampled Histogram')

plt.ylim(0, 0.85)  # 与 R 版保持一致
plt.legend()
plt.show()

# ===========================================================================
#                  心情示例：状态转移（Markov Chain）
# ===========================================================================

import numpy as np
import matplotlib.pyplot as plt

# 中文标签字体（缺失时自动回退到可用字体）
plt.rcParams["font.sans-serif"] = ["Arial Unicode MS", "Heiti TC", "Songti SC",
                                   "Microsoft YaHei", "SimHei",
                                   "Noto Sans CJK SC", "DejaVu Sans"]
plt.rcParams["axes.unicode_minus"] = False

# 状态转移矩阵 P：行 = 前一天(n-1)，列 = 当天(n)
emo = ["开心", "冷静", "悲伤"]
P = np.array([[0.50, 0.25, 0.25],
              [0.50, 0.00, 0.50],
              [0.25, 0.25, 0.50]])

n_days = 2000
rng = np.random.default_rng(202409)


def sim_mood(start, n):
    """从 start 出发，按照 P 转移 n 次（每次 = 一天），返回状态序列。"""
    states = np.empty(n + 1, dtype=int)
    states[0] = emo.index(start)
    for i in range(n):
        states[i + 1] = rng.choice(3, p=P[states[i]])
    return states


# 分别以三种情绪为起点，各模拟一条链
chains = {st: sim_mood(st, n_days) for st in emo}

# 出现频率
freq = {st: np.bincount(chains[st], minlength=3) / (n_days + 1) for st in emo}

# 累计频率
cum = {st: np.array([np.bincount(chains[st][:t + 1], minlength=3) / (t + 1)
                     for t in range(n_days + 1)]) for st in emo}

# 理论平稳分布
pi = np.array([0.40, 0.20, 0.40])

fig, axs = plt.subplots(2, 3, figsize=(16, 7))
for j, st in enumerate(emo):
    # 上排：出现频率
    ax = axs[0, j]
    ax.bar(emo, freq[st], color="#6497b1")
    ax.plot(emo, pi, linestyle="--", color="grey", marker="d")
    for k, v in enumerate(freq[st]):
        ax.text(k, v + 0.01, f"{v:.2f}", ha="center", fontsize=9)
    ax.set_ylim(0, 0.5)
    ax.set_title(f"起点 = {st}")
    ax.set_ylabel("频率")

    # 下排：累计频率
    ax = axs[1, j]
    for k, e in enumerate(emo):
        ax.plot(range(1, n_days + 2), cum[st][:, k], label=e)
        ax.axhline(pi[k], linestyle="--", color=f"C{k}", alpha=0.6)
    ax.set_ylim(0, 1)
    ax.set_xlabel("天数")
    ax.set_ylabel("累计频率")
    if j == 2:
        ax.legend(fontsize=8)

plt.tight_layout()
plt.show()

import numpy as np
import scipy.stats as st
import pandas as pd
import seaborn as sns

np.random.seed(2024)

current = 3  # 假设theta(n)为3

proposal = st.norm(current, 1).rvs()  # 从当前正态分布中抽出一个样本

print("从建议分布中新采样 θ(n+1)为：", proposal)

# 设置先验
prior = st.norm(loc=0, scale=1)


def likelihood(theta):
    Y = 6.25  # 假设数据 Y = 6.25
    return st.norm(loc=theta, scale=0.75).pdf(Y)  # 注意：这里为方便演示固定 scale = 0.75


# 计算建议位置(n+1)的未归一化的后验概率值（先验*似然）
proposal_posterior = prior.pdf(proposal) * likelihood(proposal)

# 计算当前位置(n)的未归一化的后验概率值（先验*似然）
current_posterior = prior.pdf(current) * likelihood(current)

# 计算接受概率α，为两者概率值之比
alpha = min(1, proposal_posterior / current_posterior)

# 打印出接受概率α
print("后验比为：", proposal_posterior / current_posterior, ", alpha为:", alpha)

# 根据接受概率α进行抽样，抽样内容为建议位置和当前位置
next_stop = np.random.choice([proposal, current], 1, p=[alpha, 1 - alpha])

# 打印出下一个位置的值
next_stop[0]


# 从第一段代码我们可以看到此时的接受概率α=1，因此接受了建议值作为我们的下一个值

def one_mh_iteration(current, sigma=1):
    """
    def后面为函数值，current为输入值，作为建议分布(正态分布)的均值

    接下来的代码和之前一样

    return 则是该函数返回的值，我们将建议值，接受概率，和下一个位置这三个值组成了一个数据框进行返回
    """
    proposal = st.norm(current, sigma).rvs()

    prior = st.norm(loc=0, scale=1)

    def likelihood(theta):
        # 假设数据 Y = 6.25
        Y = 6.25
        return st.norm(loc=theta, scale=0.75).pdf(Y)

    proposal_posterior = prior.pdf(proposal) * likelihood(proposal)
    current_posterior = prior.pdf(current) * likelihood(current)
    alpha = min(1, proposal_posterior / current_posterior)
    next_stop = np.random.choice([proposal, current], 1, p=[alpha, 1 - alpha])
    return pd.DataFrame({"proposal": [proposal],
                         "alpha": [alpha],
                         "next_stop": [next_stop[0]]})


np.random.seed(2024)
one_mh_iteration(current=3)

# 变换不同的随机数种子，其实也是生成不同的建议值
np.random.seed(83)
one_mh_iteration(current=3)


def mh_tour(N, sigma=1):
    """
    N为迭代次数，sigma为正态建议分布的标准差

    我们在单次采样函数的基础上叠加了一个循环
    将每次的采样结果存在mu[i]中，
    在每次采样结束后，将采样结果替换为当前位置

    返回值为迭代次数，和每次采样得到的结果
    """
    current = 3
    mu = np.zeros(N)

    for i in range(N):
        sim = one_mh_iteration(current, sigma)
        mu[i] = sim["next_stop"][0]
        current = sim["next_stop"][0]

    return pd.DataFrame({"iteration": range(1, N + 1),
                         'mu': mu})


# 调用定义好的函数，将采样次数设为5000
np.random.seed(84735)
mh_simulation = mh_tour(N=5000)
mh_simulation.tail()

import matplotlib.pyplot as plt

# 生成一行两列的画布
fig, axs = plt.subplots(1, 2, figsize=(20, 5))

# density plot：在第一列绘制出采样结果的分布
axs[0].hist(mh_simulation["mu"],
            edgecolor="white",
            color="darkgrey",
            alpha=0.7,
            bins=30,
            density=True)
axs[0].set_xlabel("mu", fontsize=16)
axs[0].set_ylabel("density", fontsize=16)

# 绘制分布外围线条
x_norm = np.linspace(2, 6.5, 10000)
y_norm = st.norm.pdf(x_norm, loc=mh_simulation["mu"].mean(), scale=mh_simulation["mu"].std())

axs[0].plot(x_norm, y_norm, color='blue')

# trace plot：在第二列绘制出每一次的采样结果
axs[1].plot(mh_simulation["iteration"], mh_simulation["mu"],
            color="grey", )
axs[1].set_xlabel("iteration", fontsize=16)
axs[1].set_ylabel("mu", fontsize=16)

import arviz as az
import matplotlib.pyplot as plt

# arviz 1.x：plot_trace 需要 DataTree/InferenceData 对象（不再接受 dict）
az.plot_trace(az.from_dict({"posterior": {"mu": mh_simulation["mu"].values[None, :]}}),
              var_names=["mu"])

import numpy as np
import matplotlib.pyplot as plt
import arviz as az
import scipy.stats as st
import pandas as pd


def one_mh_iteration(current, sigma=1):
    """
    def后面为函数值，current为输入值，作为建议分布(正态分布)的均值

    接下来的代码和之前一样

    return 则是该函数返回的值，我们将建议值，接受概率，和下一个位置这三个值组成了一个数据框进行返回
    """
    proposal = st.norm(current, sigma).rvs()

    prior = st.norm(loc=0, scale=1)

    def likelihood(theta):
        # 假设数据 Y = 6.25
        Y = 6.25
        return st.norm(loc=theta, scale=0.75).pdf(Y)

    proposal_posterior = prior.pdf(proposal) * likelihood(proposal)
    current_posterior = prior.pdf(current) * likelihood(current)
    alpha = min(1, proposal_posterior / current_posterior)
    next_stop = np.random.choice([proposal, current], 1, p=[alpha, 1 - alpha])
    return pd.DataFrame({"proposal": [proposal],
                         "alpha": [alpha],
                         "next_stop": [next_stop[0]]})


def mh_tour(N, sigma=1):
    """
    N为迭代次数，sigma为正态建议分布的标准差

    我们在单次采样函数的基础上叠加了一个循环
    将每次的采样结果存在mu[i]中，
    在每次采样结束后，将采样结果替换为当前位置

    返回值为迭代次数，和每次采样得到的结果
    """
    current = 3
    mu = np.zeros(N)

    for i in range(N):
        sim = one_mh_iteration(current, sigma)
        mu[i] = sim["next_stop"][0]
        current = sim["next_stop"][0]

    return pd.DataFrame({"iteration": range(1, N + 1),
                         'mu': mu})


# ===========================================================================
#                            请尝试修改下面的数值，观察后验如何变化。
# ===========================================================================
np.random.seed(84735)

mh_simulation = mh_tour(N=5000, sigma=1)
az.plot_trace(az.from_dict({"posterior": {"mu": mh_simulation["mu"].values[None, :]}}),
              var_names=["mu"])

# ===========================================================================
#                            可以自行复制代码多试几次
# ===========================================================================
# 练习：把下面的 ... 换成 0.01 / 1 / 100，看看轨迹图与密度图有什么不同
# mh_simulation = mh_tour(N=5000, sigma=...)
# az.plot_trace(az.from_dict({"posterior": {"mu": mh_simulation["mu"].values[None, :]}},
#                            var_names=["mu"]))

# ===========================================================================
#        比较三种做法：解析解、PyMC 的 MCMC 和手搓的 MCMC
# ===========================================================================
mh_mu = mh_simulation["mu"].values
pymc_mu = idata_nn.posterior["mu"].values.reshape(-1)

fig, ax = plt.subplots(figsize=(10, 5))
ax.hist(pymc_mu, bins=50, density=True, color="#6497b1", alpha=0.7,
        edgecolor="white", label="PyMC MCMC")
sns.kdeplot(mh_mu, color="#D55E00", linewidth=2, label="Hand-coded MCMC", ax=ax)
xg = np.linspace(2, 6.5, 1000)
ax.plot(xg, st.norm.pdf(xg, post_mean, post_sd), color="black", linewidth=2,
        label="Analytic solution")
ax.axvline(post_mean, linestyle="--", color="grey")
ax.set_xlim(2, 6.5)
ax.set_xlabel("mu")
ax.set_ylabel("density")
ax.legend()
sns.despine()
plt.show()

##PyMC实战
import pymc as pm
import arviz as az

# 定义观测值Y=9
Y = 9

# 创建一个 PyMC 模型名为 bb_model
bb_model = pm.Model()

# 使用with语句将模型与变量联系起来
with bb_model:
    # 定义一个符合beta先验分布的变量pi
    pi = pm.Beta('pi', alpha=2, beta=2)
    # 定义符合binomial分布的变量likelihood，其中总次数为10，成功概率为pi，观测值为Y
    likelihood = pm.Binomial('likelihood', n=10, p=pi, observed=Y)

# 模型结构图（需要额外安装 graphviz：pip install graphviz + 系统 graphviz）
try:
    pm.model_to_graphviz(bb_model)
except ImportError:
    print("未安装 graphviz，跳过模型结构图")

# 采样过程仍在该容器中进行
with bb_model:
    trace = pm.sample(draws=1000,  # 每条链保留的样本数，对应 iter - warmup
                      tune=1000,  # 对应 R/Stan 的 warmup
                      chains=2,  # 设置MCMC的链数
                      random_seed=202409,
                      discard_tuned_samples=False)  # 保留 warmup 样本，便于观察热身阶段

# 可视化采样结果
az.plot_trace(trace)

# 含 warmup 的样本：取第一条 Markov 链的前 20 次与前 200 次迭代（热身阶段）
# 提示：warmup 分组名随 arviz 版本不同（新版为 warmup_posterior，旧版为 warmup）
warmup_group = trace["warmup_posterior"] if "warmup_posterior" in trace.children else trace["warmup"]
samples_20 = warmup_group["pi"].sel(chain=0).values[:20]
samples_200 = warmup_group["pi"].sel(chain=0).values[:200]

# 绘图
# az.plot_trace表示绘制前20个采样结果的轨迹图，其中pi为变量名，samples_20为采样结果
az.plot_trace(az.from_dict({"posterior": {"pi": np.asarray(samples_20)[None, :]}}),
              var_names=["pi"])
az.plot_trace(az.from_dict({"posterior": {"pi": np.asarray(samples_200)[None, :]}}),
              var_names=["pi"])

import pandas as pd

post_pi = pd.DataFrame({"pi": trace.posterior["pi"].values.reshape(-1)})
post_pi

import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
import scipy.stats as st

fig, (axs1, axs2) = plt.subplots(1, 2, figsize=(15, 4))

# 绘制采样结果直方图
sns.histplot(data=post_pi,
             x="pi",
             bins=30,
             ax=axs1,
             edgecolor='#20699d',
             color="#6497b1",
             alpha=1)

# 绘制采样结果密度分布图
sns.kdeplot(data=post_pi,
            x="pi",
            color='#6497b1',
            fill=True,
            alpha=1,
            ax=axs2)

# 绘制真实后验分布图
x = np.linspace(0.2, 1, 10000)
# 真实的后验分布 beta（alpha + y, beta + n - y）
y = st.beta.pdf(x, 11, 3)
axs2.plot(x, y, color='black')

sns.despine()

##练习
import pymc as pm
import arviz as az

# ===========================================================================
#                            请尝试修改下面的数值，观察后验如何变化。
# ===========================================================================

observed_data = [320, 310, 280, 340, 300]

# 1. 设立容器

with pm.Model() as nn_model:
    # 2. 定义先验
    mu_prior = pm.Normal('mu_prior', mu=300, sigma=50)
    # 3. 定义似然
    Y_obs = pm.Normal('Y', mu=mu_prior, sigma=20, observed=observed_data)
# ===========================================================================
#                            请尝试修改下面的数值，观察后验如何变化。
# ===========================================================================
with nn_model:
    trace = pm.sample(draws=1000,  # 每条链保留的样本数，对应 iter - warmup
                      tune=1000,  # 对应 R/Stan 的 warmup
                      chains=4,  # 设置MCMC的链数
                      random_seed=202409)
trace

az.plot_trace(trace)

# 从 MCMC 采样结果中提取参数 mu 的后验分布样本
post_mu = pd.DataFrame({"mu": trace.posterior["mu_prior"].values.reshape(-1)})

# ===========================================================================
#                            为了和真实的后验分布进行比较，计算真实的后验分布。
# ===========================================================================


# 先验分布的均值和标准差
mu_prior = 300  # 先验均值
sigma_prior = 50  # 先验标准差

# 观测数据的标准差 (已知)
sigma_obs = 20

# 计算观测数据的数量和均值
n = len(observed_data)
y_mean = np.mean(observed_data)

# 计算后验分布的均值和方差
posterior_mean = (sigma_obs ** 2 * mu_prior + n * sigma_prior ** 2 * y_mean) / (n * sigma_prior ** 2 + sigma_obs ** 2)
posterior_variance = (sigma_prior ** 2 * sigma_obs ** 2) / (n * sigma_prior ** 2 + sigma_obs ** 2)
posterior_std = np.sqrt(posterior_variance)

print(f"后验分布的均值: {posterior_mean}")
print(f"后验分布的标准差: {posterior_std}")

fig, (axs1, axs2) = plt.subplots(1, 2, figsize=(15, 4))

sns.histplot(data=post_mu,
             x="mu",
             bins=50,
             ax=axs1,
             edgecolor='#20699d',
             color="#6497b1",
             alpha=1)

sns.kdeplot(data=post_mu,
            x="mu",
            color='#6497b1',
            fill=True,
            alpha=1,
            ax=axs2)

# 计算真实的后验分布
x = np.linspace(200, 400, 5000)
y = st.norm.pdf(x, posterior_mean, posterior_std)
axs2.plot(x, y, color='red')

sns.despine()

