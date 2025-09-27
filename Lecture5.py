import numpy as np
import scipy.stats as st
import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd

pi_grid = np.linspace(0.5, 1, 11)
print("从0.5~1内的连续变量π中取出11个值:", pi_grid)

prior = st.beta.pdf(pi_grid, 70, 30)
likelihood = st.binom.pmf(90, 100, pi_grid)
posterior = prior * likelihood / np.sum(prior * likelihood)

# 使用 plt.stem() 绘制垂直柱状图，表示在 pi_grid 上的 posterior 分布
plt.stem(
    pi_grid,
    posterior,  
    linefmt="black",
    bottom=-1,
)
# 设置 y 轴范围在 0 到 1 之间
plt.ylim(0, 1)  
plt.xlabel("pi_grid")  
plt.ylabel("Posterior")  
# 去除图形的上方和右侧的边框
sns.despine()  

np.random.seed(84735)

#  从 posterior 分布中抽取 10000 个样本
posterior_sample = np.random.choice(
    pi_grid, 
    size=10000,
    p=posterior,  
    replace=True,
)

# 将抽取的样本存储在 DataFrame 中，列名为 "pi_sample"
posterior_sample = pd.DataFrame(
    {"pi_sample": posterior_sample}
)  

# 对 posterior_sample 中的样本进行计数，并使用 normalize=True 将计数转换为相对频率
posterior_sample.value_counts(
    normalize=True
).reset_index()  

x_beta = np.linspace(0, 1, 10000)  # 生成10000 个点，范围在 [0, 1] 之间

y_beta = st.beta.pdf(x_beta, 160, 40)  # 生成 Beta(160,40)

# 绘制共轭方法计算得到的后验 beta(160,40)
plt.plot(x_beta, y_beta, label="posterior of conjucated distribution") 

# 绘制网格方法抽样后得到的结果
plt.hist(
    posterior_sample["pi_sample"],  
    density=True,
    label = "posterior of grid search"
)
plt.legend()

sns.despine()

# 生成一个 101 个点，范围在 [0, 1] 之间
pi_grid = np.linspace(0, 1, 101)  
# 生成 Beta(70,30)
prior = st.beta.pdf(pi_grid, 70, 30)  

# 生成二项分布, 参数为 n=100（总试验次数），k=90（正确次数），以及 pi_grid 中的每个概率值
likelihood = st.binom.pmf(
    90, 100, pi_grid
)  

# 计算后验概率，即先验概率和似然函数的乘积，然后除以归一化常数（分母和）
posterior = (
    prior * likelihood / np.sum(prior * likelihood)
)  

# 画图
plt.stem(pi_grid, posterior, bottom=-1)
plt.ylim(0, 0.3)
sns.despine()

np.random.seed(84735)

# 从 posterior 分布中抽取 10000 个样本
posterior_sample = np.random.choice(
    pi_grid,  
    size=10000,  
    p=posterior,  
    replace=True,
)  

# 将抽取的样本存储在 DataFrame 中，列名为 "pi_sample"
posterior_sample = pd.DataFrame(
    {"pi_sample": posterior_sample}
)  

# 对 posterior_sample 中的样本进行计数，并使用 normalize=True 将计数转换为相对频率
posterior_sample.value_counts(
    normalize=True
).reset_index() 

# 生成一个 10000 个点，范围在 [0, 1] 之间
x_beta = np.linspace(0, 1, 10000)
# 生成Beta(160,40)  
y_beta = st.beta.pdf(x_beta, 160, 40)  

# 绘制共轭方法计算得到的后验 beta(160,40)
plt.plot(x_beta, y_beta, label="posterior of conjucated distribution")  

# 绘制网格方法抽样后得到的结果
plt.hist(
    posterior_sample["pi_sample"],  
    density=True,
    label = "posterior of grid search"
)
plt.legend()

sns.despine()

# 练习部分
import numpy as np
import scipy.stats as st
import matplotlib.pyplot as plt

# 生成模拟数据
np.random.seed(0)
data = np.random.normal(loc=550, scale=80, size=10)

# 展示数据
data.round(0)

##---------------------------------------------------------------------------
#                            设置网格范围和步长
#                            1. 假设被试反应的反应时范围为 200 到 800 ms
#                            2. 设定网格步长为10 (后续可以修改为20,50,100等)
# ---------------------------------------------------------------------------
# theta_grid = np.linspace(..., ..., 20) 

##---------------------------------------------------------------------------
#                            计算先验概率
#                            1. 设定先验概率服从正态分布
#                            2. 先验均值为500, 标准差为100
# ---------------------------------------------------------------------------
# prior_mean = ... 
# prior_std = ... 

##---------------------------------------------------------------------------
#                            计算似然函数
# ---------------------------------------------------------------------------
# likelihood = ...

##---------------------------------------------------------------------------
#                            计算后验
# ---------------------------------------------------------------------------
# posterior_prob = ...

# 归一化后验概率

##---------------------------------------------------------------------------
#                            计算找到后验概率的最大值对应的参数
# ---------------------------------------------------------------------------
# max_posterior = ...
print("最大后验概率对应的参数值：", theta_grid[max_posterior])

# 绘制结果
plt.plot(theta_grid, prior_prob / prior_prob.sum(), color="orange", label="prior")
plt.plot(theta_grid, posterior_prob, label="posterior of grid method")
plt.vlines(data.mean(), 0, posterior_prob.max(), color="red", label="true data")
plt.legend()
plt.title("Grid search posterior distribution")
plt.xlabel("$\mu$")
plt.ylabel("Density")
plt.show()

##---------------------------------------------------------------------------
#                            通过共轭方法计算后验概率 (具体算法见补充材料)
# ---------------------------------------------------------------------------
x = np.linspace(200,800,10000)
prior_mean = 500
prior_variance = 200**2
sigma2 = 80**2
n = len(data)  # 观测数据的数量
posterior_mean = (prior_mean / prior_variance + data.sum() / sigma2) / (1 / prior_variance + n / sigma2)
posterior_std = np.sqrt((1 / prior_variance + n / sigma2)**-1)
posterior_conjucate = st.norm.pdf(x, loc=posterior_mean, scale=posterior_std)

# 绘制结果
plt.plot(x, posterior_conjucate, label="posterior of conjucated method")
plt.vlines(data.mean(), 0, posterior_conjucate.max(), color="red", label="true data")
plt.legend()
plt.title("Conjucated posterior distribution")
plt.xlabel("$\mu$")
plt.ylabel("Density")
plt.show()

##---------------------------------------------------------------------------
#                            设置网格范围和步长
#                            1. 假设被试反应的反应时范围为 200 到 800 ms
#                            2. 假设被试反应时的方差范围为 20 到 200
# ---------------------------------------------------------------------------
n_step = 20
# mean_grid = ...  请补充...
# std_grid = ...   请补充...

mean_grid = np.linspace(200, 800, n_step)
std_grid = np.linspace(20, 200, n_step)
mean_mesh, std_mesh = np.meshgrid(mean_grid, std_grid)

##---------------------------------------------------------------------------
#                            计算先验概率
#                            1. 设定先验概率服从正态分布
#                            2. 先验均值为500，标准差为100
# ---------------------------------------------------------------------------

# prior_mean = ...        
# prior_std = ...         

# 显示 prior_grid 的形状
prior_grid.shape

##---------------------------------------------------------------------------
#                            计算似然函数
#                            1. 先计算一种参数条件下的似然值
#                            2. 通过for循环计算所有参数条件下的似然值，并储存在likelihood_grid中
# ---------------------------------------------------------------------------
# likelihood_single = ...
# likelihood_grid = np.zeros((n_step, n_step))

likelihood_grid.shape

##---------------------------------------------------------------------------
#                            计算grid的后验概率
# ---------------------------------------------------------------------------
# posterior_grid = ...

posterior_grid /= posterior_grid.sum()  # 归一化

# 显示 posterior_grid 的形状
posterior_grid.shape

##---------------------------------------------------------------------------
#                            计算找到后验概率的最大值对应的参数
# ---------------------------------------------------------------------------
# max_idx = ...
# estimated_mean = ...
# estimated_std = ...

print(f"Estimated Mean: {estimated_mean}")
print(f"Estimated Standard Deviation: {estimated_std}")

# 绘制后验概率分布图
plt.figure(figsize=(8, 6))

plt.scatter(prior_mean_mean, prior_std_mean, color="orange", label="prior")
plt.scatter(data.mean(), data.std(), color="black", label="data")
plt.scatter(estimated_mean, estimated_std, color="red", label="max_posterior")

plt.xlabel("Mean")
plt.ylabel("Standard Deviation")
plt.title("Posterior Distribution")

plt.xlim(400, 800)
plt.ylim(20, 200)
plt.legend()
plt.show()

# 如何使用 PyMC，通过简单的代码实现之前对于 Beta-Binomial 模型的后验推断
import pymc as pm
import numpy as np
import arviz as az
import matplotlib.pyplot as plt
from scipy.stats import beta

# 生成模拟数据
n_trials = 100
n_successes = 90

# 定义贝叶斯模型
with pm.Model() as bb_model:
    # 设置先验
    p = pm.Beta('p', alpha=70, beta=30)
    
    # 设置似然
    likelihood = pm.Binomial('likelihood', n=n_trials, p=p, observed=n_successes)
    
    # 采样
    trace = pm.sample(10000, return_inferencedata=True)

# 显示采样结果
az.summary(trace)

# 绘制p的后验分布
az.plot_posterior(trace)
plt.show()

import seaborn as sns

fig, ax = plt.subplots(figsize=(10, 5))

alpha_prior = 70
beta_prior = 30

# 共轭先验的 Beta 分布
x = np.linspace(0, 1, 100)
alpha_posterior = alpha_prior + n_successes
beta_posterior = beta_prior + (n_trials - n_successes)

# 绘制 beta-binomial 共轭分布的对比
ax.plot(
    x, beta.pdf(x, alpha_prior, beta_prior), 
    label=f'Prior Beta({alpha_prior},{beta_prior})',
    color = "green"
    )
ax.plot(
    x, beta.pdf(x, alpha_posterior, beta_posterior), 
    label=f'Posterior Beta({alpha_posterior},{beta_posterior}) from conjucated prior',
    color = "red", linestyle='--',
    )
ax.legend()

az.plot_posterior(trace, var_names=['p'], ax=ax)  

ax.set_xlim(0.5, 1)
ax.set_title('Posterior results')

sns.despine()
plt.tight_layout()
plt.show()

import numpy as np
import scipy.stats as st
import matplotlib.pyplot as plt
import seaborn as sns

# 定义正确率范围
x = np.linspace(0, 1, 10000)  # 正确率在0到1之间

# 定义先验分布 (基于文献，正确率均值为70%)
prior_mean = 0.70
prior_std = 0.05
prior_y = st.norm.pdf(x, loc=prior_mean, scale=prior_std) / np.sum(
    st.norm.pdf(x, prior_mean, prior_std)
)

# 生成似然分布 (基于新实验数据，正确率均值为75%)
likelihood_mean = 0.75
likelihood_std = 0.05
likelihood_values = st.norm.pdf(x, loc=likelihood_mean, scale=likelihood_std) / np.sum(
    st.norm.pdf(x, likelihood_mean, likelihood_std)
)

# 计算后验分布
posterior_mean = (prior_mean * likelihood_std**2 + likelihood_mean * prior_std**2) / (
    prior_std**2 + likelihood_std**2
)
posterior_std = np.sqrt(
    (prior_std**2 * likelihood_std**2) / (prior_std**2 + likelihood_std**2)
)
posterior = st.norm.pdf(x, loc=posterior_mean, scale=posterior_std) / np.sum(
    st.norm.pdf(x, posterior_mean, posterior_std)
)

# 绘制先验、似然和后验分布
plt.plot(x, prior_y, color="#f0e442", label="prior")
plt.fill_between(x, prior_y, color="#f0e442", alpha=0.5)
plt.plot(x, likelihood_values, color="#0071b2", label="likelihood")
plt.fill_between(x, likelihood_values, color="#0071b2", alpha=0.5)
plt.plot(x, posterior, color="#009e74", label="posterior")
plt.fill_between(x, posterior, color="#009e74", alpha=0.5)

# 设置 x 和 y 轴标签
plt.xlabel("$\mu$ for accuracy (correct response rate)")
plt.ylabel("density")
plt.legend()

# 移除图的上、右边框线
sns.despine()

# 展示图像
plt.show()