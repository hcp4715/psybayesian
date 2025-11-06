# 导入 pymc 模型包，和 arviz 等分析工具 
import pymc as pm
import arviz as az
import seaborn as sns
import scipy.stats as st
import numpy as np
import matplotlib.pyplot as plt
import xarray as xr
import pandas as pd

# 忽略不必要的警告
import warnings
warnings.filterwarnings("ignore")

# 通过 pd.read_csv 加载数据 Kolvoort_2020_HBM_Exp1_Clean.csv
try:
  df_raw = pd.read_csv('/home/mw/input/bayes3797/Kolvoort_2020_HBM_Exp1_Clean.csv')
except:
  df_raw = pd.read_csv('2024/data/Kolvoort_2020_HBM_Exp1_Clean.csv')

df_raw.head()

# 筛选出被试"201"，匹配类型为"Matching"的数据
df_raw["Subject"] = df_raw["Subject"].astype(str)
df = df_raw[(df_raw["Subject"] == "201") & (df_raw["Matching"] == "Matching")]

# 选择需要的两列
df = df[["Label", "RT_sec"]]

# 重新编码标签（Label）
df["Label"] = df["Label"].map({1: 0, 2: 1, 3: 1})

# #设置索引
df["index"] = range(len(df))
df = df.set_index("index")

# 显示部分数据
df.head()

import seaborn as sns
import matplotlib.pyplot as plt

# 计算每个Label条件下的均值
mean_values = df.groupby('Label')['RT_sec'].mean()

# 使用 seaborn 绘制箱线图，展示 Matching 条件下反应时间的分布情况
plt.figure(figsize=(5, 3.2))
sns.boxplot(x="Label", y="RT_sec", data=df)

plt.plot(mean_values.index, mean_values.values, marker='o', color='r', linestyle='-', linewidth=2)

# 移除上面和右边的边框
sns.despine()

# 添加标签
plt.xlabel("Label Condition (0=self, 1=other)")
plt.ylabel("Reaction Time (sec)")
plt.show()

# 导入库
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import scipy.stats as stats
import seaborn as sns

# 定义新的先验分布的参数
mu_beta0 = 5         
sigma_beta0 = 2     
mu_beta1 = 0       
sigma_beta1 = 1   
lambda_sigma = 0.3      

# 生成 beta_0 的先验分布值
x_beta0 = np.linspace(-5, 15, 1000)  
y_beta0 = stats.norm.pdf(x_beta0, mu_beta0, sigma_beta0)

# 生成 beta_1 的先验分布值
x_beta1 = np.linspace(-5, 5, 1000) 
y_beta1 = stats.norm.pdf(x_beta1, mu_beta1, sigma_beta1)

# 生成 sigma 的先验分布值
x_sigma = np.linspace(0, 10, 1000)  
y_sigma = stats.expon.pdf(x_sigma, scale=1/lambda_sigma)

# 绘制先验分布图
fig, axes = plt.subplots(1, 3, figsize=(15, 5))

# 绘制 beta_0 的先验分布
axes[0].plot(x_beta0, y_beta0, 'k-')
axes[0].set_title(r"$N(5, 2^2)$")
axes[0].set_xlabel(r"$\beta_0$")
axes[0].set_ylabel("pdf")

# 绘制 beta_1 的先验分布
axes[1].plot(x_beta1, y_beta1, 'k-')
axes[1].set_title(r"$N(0, 1^2)$")
axes[1].set_xlabel(r"$\beta_1$")
axes[1].set_ylabel("pdf")

# 绘制 sigma 的先验分布
axes[2].plot(x_sigma, y_sigma, 'k-')
axes[2].set_title(r"Exp(0.3)")
axes[2].set_xlabel(r"$\sigma$")
axes[2].set_ylabel("pdf")

# 调整布局并显示图表
sns.despine()
plt.tight_layout()
plt.show()

# 设置随机种子确保结果可以重复
np.random.seed(84735)

# 根据设定的先验分布，在其中各抽取200个beta_0,200个beta_1, 200个sigma
beta0_200 = np.random.normal(loc = 5, scale = 2, size = 200)
beta1_200 = np.random.normal(loc = 0, scale = 1 , size = 200)
sigma_200 = np.random.exponential(scale=1/0.3, size=200)

#将结果存在一个数据框内
prior_pred_sample = pd.DataFrame({"beta0":beta0_200,
                                  "beta1":beta1_200,
                                  "sigma":sigma_200})
#查看抽样结果                                
prior_pred_sample

# 通过np.arange设置Label，0 代表 Self， 1 代表other。
x_sim = np.array([0, 1])

# 查看自变量值
x_sim

# 设置随机种子确保结果可以重复
np.random.seed(84735)
# 根据设定的先验分布，在其中各抽取200个beta_0,200个beta_1, 200个sigma
beta0_200 = np.random.normal(loc = 5, scale = 2, size = 200)
beta1_200 = np.random.normal(loc = 0, scale = 1 , size = 200)
sigma_200 = np.random.exponential(scale=1/0.3, size=200)
#将结果存在一个数据框内
prior_pred_sample = pd.DataFrame({"beta0":beta0_200,
                                  "beta1":beta1_200,
                                  "sigma":sigma_200})
#查看抽样结果                                
prior_pred_sample
# 保存为数据框
prior_pred_sample = pd.DataFrame({"beta0": beta0_200, "beta1": beta1_200, "sigma": sigma_200})


# 获取第一组采样参数
beta_0 = prior_pred_sample["beta0"][0]
beta_1 = prior_pred_sample["beta1"][0]

print(f"获取的第一组采样参数值，beta_0:{beta_0:.2f}, beta_1:{beta_1:.2f}")

#===========================
#     根据回归公式 $\mu = \beta_0 + \beta_1 X$ 预测$\mu$ 的值。
#     已知：自变量（标签），self = 1, other = 2
#===========================
x_sim = np.array([1, 2])
mu = beta_0 + beta_1 * x_sim
print("预测值 μ:", mu)

#===========================
#     绘制回归线，请设置x轴和y轴的变量
#===========================
x_axis = ...
y_axis = ...

plt.plot(x_axis,y_axis)
plt.xlabel("Label Condition")
plt.ylabel("RT (sec)")  
sns.despine()

# 通过 np.array 设置实验条件的取值范围，self=0，other=1
x_sim = np.array([0, 1])

# 设置一个空列表，用来储存每一个的预测结果
mu_outcome = []

# 循环生成 200 次先验预测回归线
for i in range(len(prior_pred_sample)):
    # 根据回归公式计算预测值
    mu = prior_pred_sample["beta0"][i] + prior_pred_sample["beta1"][i] * x_sim
    mu_outcome.append(mu)

# 画出每一次的先验预测结果
for i in range(len(mu_outcome)):
    plt.plot(x_sim, mu_outcome[i])

plt.title("prior predictive check")
plt.xlabel("Label Condition")
plt.ylabel("RT (sec)")
sns.despine()

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

def prior_predictive_plot(beta0_mean=0.5, beta0_sd=0.3, beta1_mean=-0.1, beta1_sd=0.04, sigma_rate=0.2, samples=200, seed=84735):
    """
    生成先验预测图。
    
    参数：
    - beta0_mean: float，beta0的均值
    - beta0_sd: float，beta0的标准差
    - beta1_mean: float，beta1的均值
    - beta1_sd: float，beta1的标准差
    - sigma_rate: float，控制sigma的指数分布率参数（lambda = 1/scale）
    - samples: int，生成的样本数量
    - seed: int，随机种子，默认为84735，确保结果可重复
    
    输出：
    - 一个先验预测图
    """
    
    # 设置随机种子
    if seed is not None:
        np.random.seed(seed)
    
    # 根据设定的先验分布抽样
    beta0_samples = np.random.normal(loc=beta0_mean, scale=beta0_sd, size=samples)
    beta1_samples = np.random.normal(loc=beta1_mean, scale=beta1_sd, size=samples)
    sigma_samples = np.random.exponential(scale=1/sigma_rate, size=samples)
    
    # 创建数据框存储样本
    prior_pred_sample = pd.DataFrame({
        "beta0": beta0_samples,
        "beta1": beta1_samples,
        "sigma": sigma_samples
    })

    # 定义实验条件（self=0，other=1）
    x_sim = np.array([0, 1])
    
    # 创建一个空列表存储每次模拟的结果
    mu_outcome = []
    
    # 生成先验预测回归线
    for i in range(samples):
        mu = prior_pred_sample["beta0"][i] + prior_pred_sample["beta1"][i] * x_sim
        mu_outcome.append(mu)
    
    # 绘图
    plt.figure(figsize=(10, 6))
    for i in range(len(mu_outcome)):
        plt.plot(x_sim, mu_outcome[i])
    
    plt.title("Prior Predictive Check")
    plt.xlabel("Label Condition")
    plt.ylabel("RT (sec)")
    sns.despine()
    plt.show()

# 使用示例
prior_predictive_plot()

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

#=====================================================
#     请完善代码中...的部分，设置3个参数的值，使先验分布更符合实际情况
#=====================================================

prior_predictive_plot(beta0_mean=...,  # beta 0 的均值
                      beta0_sd=...,    # beta 0 的标准差
                      beta1_mean=...,  # beta 1 的标准差
                      beta1_sd=...,    # beta 1 的标准差
                      sigma_rate=...,  # 控制sigma的指数分布率参数（lambda = 1/scale）
                      samples=200,
                      seed=84735)

import pymc as pm

with pm.Model() as linear_model:

    # 定义先验分布参数
    beta_0 = pm.Normal("beta_0", mu=5, sigma=2)        
    beta_1 = pm.Normal("beta_1", mu=0, sigma=1)      
    sigma = pm.Exponential("sigma", 3)                    

    # 定义自变量 x
    x = pm.MutableData("x", df['Label'])         

    # 定义 mu，将自变量与先验结合
    mu = beta_0 + beta_1 * x

    # 定义似然：预测值y符合N(mu, sigma)分布
    likelihood = pm.Normal("y_est", mu=mu, sigma=sigma, observed=df['RT_sec'])  # 实际观测数据 y 是 RT

#===========================
#     注意！！！以下代码可能需要运行1-2分钟左右
#===========================
with linear_model:
    trace = pm.sample(draws=5000,                   # 使用mcmc方法进行采样，draws为采样次数
                      tune=1000,                    # tune为调整采样策略的次数，可以决定这些结果是否要被保留
                      chains=4,                     # 链数
                      discard_tuned_samples=True,  # tune的结果将在采样结束后被丢弃
                      random_seed=84735)

trace

trace.posterior

trace.posterior['beta_0']

trace.posterior['beta_0'][0, 10]

import arviz as az

ax = az.plot_trace(trace, figsize=(7, 7),compact=False,legend=True)
plt.tight_layout()
plt.show()

az.summary(trace, kind="diagnostics")

# 通过np.arange设置 x 轴代表 Label，其中 0 代表 Self， 1代表other。
x_sim = np.array([0, 1])

# 选取一组参数来作为预测
beta_0 = trace.posterior["beta_0"].stack(sample=("chain", "draw"))[:2]
beta_1 = trace.posterior["beta_1"].stack(sample=("chain", "draw"))[:2]

# 生成20000条回归线
y_sim_re = beta_0 + beta_1 * x_sim

# 绘制真实数据的散点图
plt.scatter(trace.constant_data.x, trace.observed_data.y_est,c="r", label="observed data")

# 绘制回归线条
plt.plot(x_sim, y_sim_re, c="grey", label = "Predicted Mean")
plt.scatter(x_sim, y_sim_re, color="black", s=50)


# 设置标题等
plt.xlim(-0.5, 1.5) 
plt.xticks([0, 1]) 
plt.title("posterior predictive check")
plt.xlabel("Label")
plt.ylabel("RT (sec)")  
plt.legend()
sns.despine()

import xarray as xr

# 导入真实的自变量
x_value = xr.DataArray(df.Label)

# 基于后验参数生成y_model
trace.posterior["y_model"] = trace.posterior["beta_0"] + trace.posterior["beta_1"] * x_value
df['Mean RT'] = df.groupby('Label')['RT_sec'].transform('mean')

# 绘制后验预测线性模型
az.plot_lm(
           y= df['Mean RT'],
           x= df.Label,
           y_model = trace.posterior["y_model"],
           y_model_mean_kwargs={"color":"black", "linewidth":2},
           figsize=(6,4),
           textsize=16,
           grid=False)

# 设置坐标轴标题、字体大小
plt.xlim(-0.5, 1.5) 
plt.xticks([0, 1]) 
plt.xlabel('Label')  
plt.ylabel('RT (sec)')  
plt.legend(['observed mean', 'Uncertainty in mean', 'Mean']) 

sns.despine()

# 采样得到的参数后验分布都储存在 trace.posterior中，我们进行一些提取操作
pos_sample = trace.posterior.stack(sample=("chain", "draw"))

# 将每个参数的20000次采样结果存储在数据框中
df_pos_sample = pd.DataFrame({"beta_0": pos_sample["beta_0"].values,
                              "beta_1": pos_sample["beta_1"].values,
                              "sigma": pos_sample["sigma"].values})

# 查看参数
df_pos_sample

# 抽取第一组参数组合，生成正态分布的均值
row_i = 0  
X_i = 1   
mu_i = df_pos_sample.beta_0[row_i] + df_pos_sample.beta_1[row_i] * X_i           
sigma_i = df_pos_sample.sigma[row_i]

# 从正态分布中随机抽取一个值，作为预测值
prediction_i = np.random.normal(
                                loc = mu_i,                                            
                                scale= sigma_i, 
                                size=1)

# 你可以运行该代码块多次，比较在相同参数下，预测值的变化(感受采样变异)。
print(f"mu_i: {mu_i:.2f}, 预测值：{prediction_i[0]:.2f}")

# 生成两个空列，用来储存每一次生成的均值mu，和每一次抽取的预测值y_new
df_pos_sample['mu'] = np.nan
df_pos_sample['y_new'] = np.nan
X_i = 1
np.random.seed(84735)

# 将之前的操作重复20000次
for row_i in range(len(df_pos_sample)):
    mu_i = df_pos_sample.beta_0[row_i] + df_pos_sample.beta_1[row_i] * X_i
    df_pos_sample["mu"][row_i] = mu_i
    df_pos_sample["y_new"][row_i] = np.random.normal(loc = mu_i,
                                            scale= df_pos_sample.sigma[row_i],
                                            size=1)

df_pos_sample

#查看真实数据中的取值，与后验预测分布作对比
df2 = df.drop(["Mean RT"],axis=1).copy()
print("x=1时y的取值有:", np.array(df2[df2["Label"]==1]))

#新建画布
fig, axs = plt.subplots(1, 2, figsize=(15, 5), sharey=True, sharex=True)        

#在第一个画布中绘制出生成的mu的分布
sns.kdeplot(data=df_pos_sample,                                                
            x="mu", 
            color="black",
            ax=axs[0])

#在第二个画布中绘制出生成的y_new的分布
sns.kdeplot(data=df_pos_sample,                                                 
            x="y_new", 
            color="black",
            ax=axs[1])

fig.suptitle('Posterior predictive distribution(x=1)', fontsize=15)
sns.despine()

with linear_model:
    ppc_data = pm.sample_posterior_predictive(trace)

ppc_data

# num_pp_samples 参数代表从总的采样(20000)选取多少采样(这里是1000)进行后验预测计算
az.plot_ppc(ppc_data, num_pp_samples=1000) 

# 筛选编号为“205”的被试的数据
df_new = df_raw[(df_raw["Subject"] == "205") & (df_raw["Matching"] == "Matching")]

# 选择需要的两列
df_new  = df[["Label", "RT_sec"]]

#设置索引
df_new["index"] = range(len(df_new))
df_new = df_new.set_index("index")

# 显示部分数据
df_new.head()

import xarray as xr
import arviz as az
import matplotlib.pyplot as plt
import seaborn as sns

def plot_posterior_predictive(df, trace, ax=None, title = "Posterior Predictive"):
    """
    绘制后验预测线性模型，展示不同 Label 条件下的平均反应时间 (RT) 及其不确定性。
    
    参数:
    - df : pandas.DataFrame
        包含实验数据的 DataFrame，其中需要包括 'Label' 和 'RT_sec' 两列。
    - trace : arviz.InferenceData
        包含后验参数的 ArviZ InferenceData 对象，需要包括 `beta_0` 和 `beta_1`。
    - ax : matplotlib.axes.Axes, optional
        用于绘制图像的 matplotlib 轴对象。如果未提供，将自动创建一个新的轴对象。
        
    Returns:
    - ax : matplotlib.axes.Axes
        返回绘制了图形的 matplotlib 轴对象。
    
    说明:
    该函数首先将 `Label` 列转换为 xarray 数据格式，以用于生成后验预测模型。接着，
    基于后验参数 `beta_0` 和 `beta_1` 计算模型预测的 `y_model`，并对每个 `Label`
    组内的反应时间 (`RT_sec`) 计算均值。在此基础上，使用 ArviZ 的 `plot_lm` 绘制
    后验预测线性模型，并设置图例、坐标轴范围、标签和其他样式。
    """
    # 如果没有提供 ax，则创建新的图形和轴对象
    if ax is None:
        fig, ax = plt.subplots(figsize=(6, 4))

    # 导入真实的自变量
    x_value = xr.DataArray(df.Label)

    # 基于后验参数生成 y_model
    trace.posterior["y_model"] = trace.posterior["beta_0"] + trace.posterior["beta_1"] * x_value
    df['Mean RT'] = df.groupby('Label')['RT_sec'].transform('mean')

    # 绘制后验预测线性模型
    az.plot_lm(
        y=df['Mean RT'],
        x=df.Label,
        y_model=trace.posterior["y_model"],
        y_model_mean_kwargs={"color":"black", "linewidth":2},
        textsize=16,
        grid=False,
        axes=ax  # 使用传入的轴对象
    )

    # 设置坐标轴标题、范围和字体大小
    ax.set_xlim(-0.5, 1.5)
    ax.set_xticks([0, 1])
    ax.set_ylim(0.65, 0.95)
    ax.set_xlabel('Label')
    ax.set_ylabel('RT (sec)')
    ax.legend(['observed mean', 'Uncertainty in mean', 'Mean'])
    ax.set_title(title)

    # 去除顶部和右侧边框
    sns.despine(ax=ax)

    # 返回轴对象
    return ax

az.summary(trace)

import arviz as az

# 定义 ROPE 区间，根据研究的需要指定实际等效范围
rope_interval = [-0.05, 0.05]

# 绘制后验分布，显示 HDI 和 ROPE
az.plot_posterior(
    trace,
    var_names="beta_1",
    hdi_prob=0.95,
    rope=rope_interval,
    figsize=(8, 5),
    textsize=12
)

plt.show()

import bambi as bmb

#定义先验并传入模型中
beta_0 = bmb.Prior("Normal", mu=5, sigma=2)  
beta_1 = bmb.Prior("Normal", mu=0, sigma=1)        
sigma = bmb.Prior("Exponential", lam = 0.3)         

# 将三个参数的先验定义在字典prior中
priors = {"beta_0": beta_0, 
          "beta_1": beta_1,
          "sigma": sigma}

#定义关系式，传入数据
model = bmb.Model('RT_sec ~ Label', 
                  data=df,
                  priors=priors,
                  dropna=True)
#总结对模型的设置
model

#===========================
#      MCMC采样过程
#      注意！！！以下代码可能需要运行几分钟
#===========================
trace = model.fit(draws=5000,                   # 使用mcmc方法进行采样，draws为采样次数
                  tune=1000,                    # tune为调整采样策略的次数，可以决定这些结果是否要被保留
                  chains=4,
                  random_seed=84735)

ax = az.plot_trace(trace, figsize=(7,7), compact=False)
plt.tight_layout()
plt.show()

az.summary(trace)


