import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

x_range = np.linspace(2, 10, 1000)  # 设置 x 的范围为 [2,10]
y_norm = norm.pdf(x_range, 6.25, 0.75)

# 对y_norm进行采样，得到1000个样本值
samples = np.random.normal(6.25, 0.75, 1000)

# 显示前 10 个样本值
first_10_samples = samples[:10]
print("前 10 个样本值：", first_10_samples)

#  绘制图像
plt.figure(figsize=(10, 6))
plt.plot(x_range, norm.pdf(x_range, 6.25, 0.75), linewidth=2, color='r', label='Normal Distribution PDF')
count, bins, ignored = plt.hist(samples, bins=30, density=True, alpha=0.6, color='grey', edgecolor='black',
                                label='Sampled Histogram')

plt.legend()
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
prior = st.norm(loc=3, scale=1)


def likelihood(theta):
    Y = 6  # 假设数据 Y 为 6
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

    prior = st.norm(loc=3, scale=1)

    def likelihood(theta):
        # 假设数据 Y 为 6
        Y = 6
        return st.norm(loc=theta, scale=0.75).pdf(Y)

    proposal_posterior = prior.pdf(proposal) * likelihood(proposal)
    current_posterior = prior.pdf(current) * likelihood(current)
    alpha = min(1, proposal_posterior / current_posterior)
    next_stop = np.random.choice([proposal, current], 1, p=[alpha, 1 - alpha])
    return pd.DataFrame({"proposal": [proposal],
                         "alpha": [alpha],
                         "next_stop": [next_stop[0]]})


np.random.seed(8)
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
            color="grey",
            alpha=0.7,
            bins=20,
            density=True)
axs[0].set_xlabel("mu", fontsize=16)
axs[0].set_ylabel("density", fontsize=16)

# 绘制分布外围线条
x_norm = np.linspace(2, 10, 10000)
y_norm = st.norm.pdf(x_norm, loc=mh_simulation["mu"].mean(), scale=mh_simulation["mu"].std())

axs[0].plot(x_norm, y_norm, color='blue')

# trace plot：在第二列绘制出每一次的采样结果
axs[1].plot(mh_simulation["iteration"], mh_simulation["mu"],
            color="grey", )
axs[1].set_xlabel("iteration", fontsize=16)
axs[1].set_ylabel("mu", fontsize=16)

import arviz as az
import matplotlib.pyplot as plt

ax = az.plot_trace({"mu": mh_simulation["mu"]})

ax[0, 0].set_xlim(2, 10)

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

    prior = st.norm(loc=3, scale=1)

    def likelihood(theta):
        # 假设数据 Y 为 6
        Y = 6
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
#                            请修改 ... 中的值。
# ===========================================================================
np.random.seed(84735)

mh_simulation = mh_tour(N=5000, sigma=...)
az.plot_trace({"mu": mh_simulation["mu"]})

# ===========================================================================
#                            可以自行复制代码多试几次
# ===========================================================================
mh_simulation = mh_tour(N=5000, sigma=...)
az.plot_trace({"mu": mh_simulation["mu"]})

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

pm.model_to_graphviz(bb_model)

# 采样过程仍在该容器中进行
with bb_model:
    trace = pm.sample(draws=5000,  # 使用mcmc方法进行采样，draws为采样次数
                      chains=1,  # 设置MCMC的链数
                      random_seed=84735)  # 设置随机状态，以获得与notebook相同的结果

# 可视化采样结果
az.plot_trace(trace)

# 选取第一条Makov链的前20个采样结果和前200个结果
# 从trace中提取pi的值，使用sel(chain=0)表示为选择第一条markov链。使用.values[:20]表示选取前20个采样结果
samples_20 = trace.posterior["pi"].sel(chain=0).values[:20]
samples_200 = trace.posterior["pi"].sel(chain=0).values[:200]

# 绘图
# az.plot_trace表示绘制前20个采样结果的轨迹图，其中pi为变量名，samples_20为采样结果
az.plot_trace({"pi": samples_20})
az.plot_trace({"pi": samples_200})

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
#                            请修改 ... 中的值。
# ===========================================================================

observed_data = [...]

# 1. 设立容器

with pm.Model() as nn_model:
    # 2. 定义先验
    mu_prior = pm.Normal('...', mu=..., sigma=...)
    # 3. 定义似然
    Y_obs = pm.Normal('...', mu=..., sigma=..., observed=...)
# ===========================================================================
#                            请修改 ... 中的值。
# ===========================================================================
with nn_model:
    trace = pm.sample(draws=...,  # 例如采样次数设为500-5000次
                      chains=1,  # 链为1条
                      random_seed=84735)
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
             bins=30,
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
x = np.linspace(200, 400, 10000)
y = st.norm.pdf(x, posterior_mean, posterior_std)
axs2.plot(x, y, color='black')

sns.despine()

