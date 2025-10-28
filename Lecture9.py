# 导入 pymc 模型包，和 arviz 等分析工具 
import pymc as pm
import arviz as az
import scipy.stats as st
import numpy as np
import matplotlib.pyplot as plt
import xarray as xr
import pandas as pd
import seaborn as sns
from math import inf

# 忽略不必要的警告
import warnings
warnings.filterwarnings("ignore")

# 通过 pd.read_csv 加载数据 Kolvoort_2020_HBM_Exp1_Clean.csv
try:
  df_raw = pd.read_csv('data/Kolvoort_2020_HBM_Exp1_Clean.csv')
except:
  df_raw = pd.read_csv('data/Kolvoort_2020_HBM_Exp1_Clean.csv')

# 筛选出被试"201"，匹配类型为"Matching"的数据
df_raw["Subject"] = df_raw["Subject"].astype(str)
df = df_raw[(df_raw["Subject"] == "201") & (df_raw["Matching"] == "Matching")]

# 选择需要的两列
df = df[["Label", "RT_sec"]]

# 重新编码标签（Label）
df["Label"] = df["Label"].map({1: 0, 2: 1, 3: 1})

# 设置索引
df["index"] = range(len(df))
df = df.set_index("index")

# #保存数据
# df.to_csv('data/Kolvoort_2020_HBM_Exp1_Clean_201.csv', index=False)

# 显示部分数据
df.head()

#模型定义
# 设置随机种子以确保结果可复现
np.random.seed(123)

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
    likelihood = pm.Normal("y_est", mu=mu, sigma=sigma, observed=df['RT_sec']) 
    
#后验采样
with linear_model:
    trace = pm.sample(draws=5000,                   # 使用mcmc方法进行采样，draws为采样次数
                      tune=1000,                    # tune为调整采样策略的次数，可以决定这些结果是否要被保留
                      chains=4,                     # 链数
                      discard_tuned_samples=True,  # tune的结果将在采样结束后被丢弃
                      random_seed=84735)
                      
#MCMC诊断
#axes = az.plot_trace(trace)
#plt.tight_layout()
#plt.show()

#Arviz实现贝叶斯因子计算
# 进行贝叶斯因子计算，需要采样先验分布
with linear_model:
    trace.extend(pm.sample_prior_predictive(5000, random_seed=84735) )

# 绘制贝叶斯因子图
#az.plot_bf(trace, var_name="beta_1", ref_val=0)

# 设置 x 轴的范围
#plt.xlim(-0.5, 0.5) 

# 去除上框线和右框线
#sns.despine()


# 定义零假设模型（仅包含截距的模型）
with pm.Model() as model_H0:

    beta_0 = pm.Normal("beta_0", mu=5, sigma=2)        
    sigma = pm.Exponential("sigma", 3)                    
    mu = beta_0
    likelihood = pm.Normal("y_est", mu=mu, sigma=sigma, observed=df['RT_sec'])  
    trace_H0 = pm.sample(draws=5000, tune=1000, chains=4,discard_tuned_samples=True, random_seed=84735)
    pm.compute_log_likelihood(trace_H0)

# 定义备择假设模型（包含截距和斜率的模型）
with pm.Model() as model_H1:
    
    beta_0 = pm.Normal("beta_0", mu=5, sigma=2)        
    beta_1 = pm.Normal("beta_1", mu=0, sigma=1)      
    sigma = pm.Exponential("sigma", 3)                    
    x = pm.MutableData("x", df['Label'])         
    mu = beta_0 + beta_1 * x
    likelihood = pm.Normal("y_est", mu=mu, sigma=sigma, observed=df['RT_sec'])  
    trace_H1 = pm.sample(draws=5000, tune=1000, chains=4,discard_tuned_samples=True, random_seed=84735)
    pm.compute_log_likelihood(trace_H1)
    

# 计算贝叶斯因子
model_compare = az.compare({"Label": trace_H0, "Null model": trace_H1}, method='BB-pseudo-BMA')
weight_H0 = model_compare.loc["Label", "weight"]
weight_H1 = model_compare.loc["Null model", "weight"]
BF_10 = weight_H1 / weight_H0
#print(f"贝叶斯因子 (BF_10): {BF_10}")

#计算先验概率比
def calculate_odds(tace_samples, region = [-0.05, 0.05]):
    
    # 计算区间 [-0.05, 0.05] 内的样本
    in_range = tace_samples[(tace_samples >= region[0]) & (tace_samples <= region[1])]

    # 计算区间外的样本
    out_of_range = tace_samples[(tace_samples < region[0]) | (tace_samples > region[1])]

    # 计算区间内外的比例
    P_in_range = len(in_range) / len(tace_samples)
    P_out_of_range = len(out_of_range) / len(tace_samples)

    # 计算比率
    ratio = P_out_of_range / P_in_range
    return ratio
  

prior_samples = trace.prior['beta_1'].values.reshape(-1)

prior_odds = calculate_odds(prior_samples, region=[-0.05, 0.05])

#print(f"先验概率比（prior odds）: {prior_odds:.4f}")




# 请直接运行，无需修改
def calculate_odds(tace_samples, region = [-0.05, 0.05]):
    
    # 计算区间 [-0.05, 0.05] 内的样本
    in_range = tace_samples[(tace_samples >= region[0]) & (tace_samples <= region[1])]

    # 计算区间外的样本
    out_of_range = tace_samples[(tace_samples < region[0]) | (tace_samples > region[1])]

    # 计算区间内外的比例
    P_in_range = len(in_range) / len(tace_samples)
    P_out_of_range = len(out_of_range) / len(tace_samples)

    # 计算比率
    ratio = P_out_of_range / P_in_range
    return ratio

def plot_region(trace_samples, region=[-0.05,0.05], dist_type="Prior", ax=None):
    
    if ax is None:
        ax = plt.gca()
    
    kde = sns.kdeplot(trace_samples, color="black", linewidth=2, ax=ax)

    # 获取核密度估计的曲线数据
    x, y = kde.get_lines()[0].get_data()

    # 高亮 [-0.05, 0.05] 区间的密度
    ax.fill_between(
        x, y,
        where=(x >= region[0]) & (x <= region[1]),
        color="blue",
        alpha=0.3,
        label="Null"
    )

    # 高亮区间之外（替代区域）
    ax.fill_between(
        x, y,
        where=(x < region[0]) | (x > region[1]),
        color="yellow",
        alpha=0.3,
        label="Alternative"
    )

    # 图例和标题
    ax.set_title(f"{dist_type} Distribution with Null Region")
    ax.set_ylabel("Density")
    ax.legend(title=f"{dist_type} regions")
    

#=====================================
#      基于方向的贝叶斯因子计算
#      自行练习
#=====================================

# 获取 beta_1的先验分布的采样
# beta_1_prior = ...


# 获取 beta_1的后验分布的采样
# beta_1_posterior = ...



# region = ...

# 计算先验比

# prior_odds = ...

# 计算后验比
# prior_odds = ...

# 计算贝叶斯因子
# prior_odds = ...

#print("bayes_factor:", bayes_factor)

本项目来源于和鲸社区，使用转载需要标注来源
作者: 和鲸社区
来源: 和鲸社区
# 最后检查结果，前面部分的代码运行无误后，请直接运行该代码块

# 绘制密度图
plt.figure(figsize=(8, 5))

# 绘图
az.plot_kde(beta_1_prior, label="prior", plot_kwargs={"color": "cyan", "alpha": 0.5}, fill_kwargs={"alpha": 0.3})

az.plot_kde(beta_1_posterior, label="posterior", plot_kwargs={"color": "coral", "alpha": 0.6}, fill_kwargs={"alpha": 0.3})

# 添加垂直虚线
plt.axvline(0, color="grey", linestyle="--", linewidth=1)

# 标注零假设区域
plt.fill_betweenx([0, 8], -1, 0, color="blue", alpha=0.2) 

plt.xlim(-1,1)

# 设置图例和标签
plt.legend(title="Distribution", loc="upper right")
plt.xlabel("beta_1")
plt.ylabel("Density")
plt.title("Prior and Posterior Distribution")

sns.despine()

plt.show()
