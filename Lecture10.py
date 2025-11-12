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

try:
    df_raw = pd.read_csv('/home/mw/input/bayes3797/Kolvoort_2020_HBM_Exp1_Clean.csv')
except:
    df_raw = pd.read_csv('/data/Kolvoort_2020_HBM_Exp1_Clean.csv')

df = df_raw.groupby(['Subject', 'Label', 'Matching'], as_index=False)['RT_sec'].mean()

# 将 Label 列的数字编码转为文字标签
df['Label'] = df['Label'].replace({1: 'Self', 2: 'Friend', 3: 'Stranger'})

df['Matching'] = df['Matching'].replace({'Matching': 'matching', 'Nonmatching': 'nonmatching'})

# 设置索引
df["index"] = range(len(df))
df = df.set_index("index")

df

print(f"Label列共有 {df['Label'].unique()} 类")

# 将 Label 列转换为有序的分类变量
df['Label'] = pd.Categorical(df['Label'], categories=['Self', 'Friend', 'Stranger'], ordered=True)

df['Label']

# 将分类变量转换为哑变量
X1 = (df['Label'] == 'Friend').astype(int)
X2 = (df['Label'] == 'Stranger').astype(int)

import pymc as pm

# 建立模型
with pm.Model() as model1:
    # 定义先验分布参数
    beta_0 = pm.Normal('beta_0', mu=5, sigma=2)
    beta_1 = pm.Normal('beta_1', mu=0, sigma=1)
    beta_2 = pm.Normal('beta_2', mu=0, sigma=1)
    sigma = pm.Exponential('sigma', lam=0.3)

    # 线性模型表达式
    mu = beta_0 + beta_1 * X1 + beta_2 * X2

    # 观测数据的似然函数
    likelihood = pm.Normal('Y_obs', mu=mu, sigma=sigma, observed=df['RT_sec'])

with model1:
    model1_trace = pm.sample(draws=5000,  # 使用mcmc方法进行采样，draws为采样次数
                             tune=1000,  # tune为调整采样策略的次数，可以决定这些结果是否要被保留
                             chains=4,  # 链数
                             discard_tuned_samples=True,  # tune的结果将在采样结束后被丢弃
                             random_seed=84735)  # 后验采样

az.summary(model1_trace)

# 定义 ROPE 区间，根据研究的需要指定实际等效范围
rope_interval = [-0.05, 0.05]

# 绘制后验分布，显示 HDI 和 ROPE
az.plot_posterior(
    model1_trace,
    var_names=["beta_1", "beta_2"],
    hdi_prob=0.95,
    rope=rope_interval,
    figsize=(9, 3),
    textsize=12
)

plt.show()

# 进行贝叶斯因子计算，需要采样先验分布
with model1:
    model1_trace.extend(pm.sample_prior_predictive(5000, random_seed=84735))

fig, axes = plt.subplots(1, 2, figsize=(10, 3.5))

# 绘制贝叶斯因子图
ax = axes[0]
az.plot_bf(model1_trace, var_name="beta_1", ref_val=0, ax=ax)
# 设置 x 轴的范围
ax.set_xlim(-0.5, 0.5)
ax = axes[1]
az.plot_bf(model1_trace, var_name="beta_2", ref_val=0, ax=ax)
# 设置 x 轴的范围
ax.set_xlim(-0.5, 0.5)

# 去除上框线和右框线
sns.despine()
plt.show()

with model1:
    model1_ppc = pm.sample_posterior_predictive(model1_trace, random_seed=84735)

az.plot_ppc(model1_ppc, num_pp_samples=500)

import xarray as xr

# 导入真实的自变量
X1 = xr.DataArray((df['Label'] == 'Friend').astype(int))
X2 = xr.DataArray((df['Label'] == 'Stranger').astype(int))

# 基于后验参数生成y_model
model1_trace.posterior["y_model"] = model1_trace.posterior["beta_0"] + model1_trace.posterior["beta_1"] * X1 + \
                                    model1_trace.posterior["beta_2"] * X2
df['Mean RT'] = df.groupby('Label')['RT_sec'].transform('mean')

# 绘制后验预测线性模型
az.plot_lm(
    y=df['Mean RT'],
    x=df.Label,
    y_model=model1_trace.posterior["y_model"],
    y_model_mean_kwargs={"color": "black", "linewidth": 2},
    figsize=(6, 4),
    textsize=16,
    grid=False)

# 设置坐标轴标题、字体大小
plt.xlim(-0.5, 2.5)
plt.xticks([0, 1, 2])
plt.xlabel('Label')
plt.ylabel('RT (sec)')
plt.legend(['observed mean', 'Uncertainty in mean', 'Mean'])

sns.despine()


def plot_prediction(df, predicted_y="prediction", ax=None):
    if ax is None:
        fig, ax = plt.subplots(figsize=(5, 4))

    sns.boxplot(x='Label', y='RT_sec', hue='Matching', data=df, palette='Set2', ax=ax)

    prediction = df.groupby(["Label", "Matching"])[predicted_y].mean().reset_index()
    # 创建映射字典：Label到x位置
    label_to_x = {'Self': 0, 'Friend': 1, 'Stranger': 2}
    # 将 Label 映射到相应的 x 值
    prediction['x_position'] = prediction['Label'].map(label_to_x)
    # 根据 Matching 设置偏移量
    prediction['x_offset'] = np.where(prediction['Matching'] == 'matching', -0.2, 0.2)
    # 计算最终的 x 位置
    prediction['final_x'] = prediction['x_position'].to_numpy() + prediction['x_offset'].to_numpy()

    ax.plot(prediction['final_x'], prediction[predicted_y], marker='o', linestyle='', color='red', label="prediction")
    ax.legend()


import xarray as xr

# 导入真实的自变量
X1 = xr.DataArray((df['Label'] == 'Friend').astype(int))
X2 = xr.DataArray((df['Label'] == 'Stranger').astype(int))

model1_trace.posterior["y_model"] = model1_trace.posterior["beta_0"] + model1_trace.posterior["beta_1"] * X1 + \
                                    model1_trace.posterior["beta_2"] * X2

df["model1_prediction"] = model1_trace.posterior.y_model.mean(dim=["chain", "draw"]).values

plot_prediction(df, "model1_prediction")

# 显示图形
sns.despine()
plt.tight_layout()
plt.show()

# 转换分类变量为哑变量
X1 = (df['Label'] == 'Friend').astype(int)
X2 = (df['Label'] == 'Stranger').astype(int)

# Matching 条件的哑变量
Matching = (df['Matching'] == 'matching').astype(int)

import pymc as pm

with pm.Model() as model2:
    # 先验分布
    beta_0 = pm.Normal('beta_0', mu=5, sigma=2)  # 截距
    beta_1 = pm.Normal('beta_1', mu=0, sigma=1)  # Friend 的主效应
    beta_2 = pm.Normal('beta_2', mu=0, sigma=1)  # Stranger 的主效应
    beta_3 = pm.Normal('beta_3', mu=0, sigma=1)  # Matching 的主效应
    sigma = pm.Exponential('sigma', lam=0.3)  # 误差项的标准差

    # 线性模型
    mu = beta_0 + beta_1 * X1 + beta_2 * X2 + beta_3 * Matching

    # 观测数据的似然函数
    likelihood = pm.Normal('Y_obs', mu=mu, sigma=sigma, observed=df['RT_sec'])

with model2:
    model2_trace = pm.sample(draws=5000,  # 使用mcmc方法进行采样，draws为采样次数
                             tune=1000,  # tune为调整采样策略的次数，可以决定这些结果是否要被保留
                             chains=4,  # 链数
                             discard_tuned_samples=True,  # tune的结果将在采样结束后被丢弃
                             random_seed=84735)  # 后验采样

az.summary(model2_trace)

# 定义 ROPE 区间，根据研究的需要指定实际等效范围
rope_interval = [-0.05, 0.05]

# 绘制后验分布，显示 HDI 和 ROPE
az.plot_posterior(
    model2_trace,
    var_names=["beta_1", "beta_2", "beta_3"],
    hdi_prob=0.95,
    rope=rope_interval,
    figsize=(12, 3),
    textsize=12
)

plt.show()

# 进行贝叶斯因子计算，需要采样先验分布
with model2:
    model2_trace.extend(pm.sample_prior_predictive(5000, random_seed=84735))

fig, axes = plt.subplots(1, 3, figsize=(12, 3.5))

# 绘制贝叶斯因子图
# beta1
ax = axes[0]
az.plot_bf(model2_trace, var_name="beta_1", ref_val=0, ax=ax)
ax.set_xlim(-0.5, 0.5)
# beta2
ax = axes[1]
az.plot_bf(model2_trace, var_name="beta_2", ref_val=0, ax=ax)
ax.set_xlim(-0.5, 0.5)
# beta3
ax = axes[2]
az.plot_bf(model2_trace, var_name="beta_3", ref_val=0, ax=ax)
ax.set_xlim(-0.5, 0.5)

# 去除上框线和右框线
sns.despine()
plt.show()

with model2:
    model2_ppc = pm.sample_posterior_predictive(model2_trace, random_seed=84735)

az.plot_ppc(model2_ppc, num_pp_samples=500)

import xarray as xr

# 导入真实的自变量
X1 = xr.DataArray((df['Label'] == 'Friend').astype(int))
X2 = xr.DataArray((df['Label'] == 'Stranger').astype(int))
Matching = xr.DataArray((df['Matching'] == 'matching').astype(int))

model2_trace.posterior["y_model"] = model2_trace.posterior["beta_0"] + model2_trace.posterior["beta_1"] * X1 + \
                                    model2_trace.posterior["beta_2"] * X2 + model2_trace.posterior["beta_3"] * Matching

df["model2_prediction"] = model2_trace.posterior.y_model.mean(dim=["chain", "draw"]).values

fig, axes = plt.subplots(1, 2, figsize=(8, 4))

# 在第一个子图上绘制 Model 1 的预测结果
plot_prediction(df, predicted_y="model1_prediction", ax=axes[0])
axes[0].set_title("Model 1")

# 在第二个子图上绘制 Model 2 的预测结果
plot_prediction(df, predicted_y="model2_prediction", ax=axes[1])
axes[1].set_title("Model 2")

# 显示图形
sns.despine()
plt.tight_layout()
plt.show()

# 转换分类变量为哑变量
X1 = (df['Label'] == 'Friend').astype(int)
X2 = (df['Label'] == 'Stranger').astype(int)

# Matching 条件的哑变量
Matching = (df['Matching'] == 'matching').astype(int)

# Friend 和 Matching 的交互
Interaction_1 = X1 * Matching
# Stranger 和 Matching 的交互
Interaction_2 = X2 * Matching

import pymc as pm

with pm.Model() as model3:
    # 定义先验分布
    beta_0 = pm.Normal('beta_0', mu=5, sigma=2)
    beta_1 = pm.Normal('beta_1', mu=0, sigma=1)
    beta_2 = pm.Normal('beta_2', mu=0, sigma=1)
    beta_3 = pm.Normal('beta_3', mu=0, sigma=1)
    beta_4 = pm.Normal('beta_4', mu=0, sigma=1)
    beta_5 = pm.Normal('beta_5', mu=0, sigma=1)
    sigma = pm.Exponential('sigma', lam=0.3)

    # 线性模型
    mu = (beta_0 + beta_1 * X1 + beta_2 * X2 + beta_3 * Matching +
          beta_4 * Interaction_1 + beta_5 * Interaction_2)

    # 观测数据的似然函数
    likelihood = pm.Normal('Y_obs', mu=mu, sigma=sigma, observed=df['RT_sec'])

with model3:
    model3_trace = pm.sample(draws=5000,  # 使用mcmc方法进行采样，draws为采样次数
                             tune=1000,  # tune为调整采样策略的次数，可以决定这些结果是否要被保留
                             chains=4,  # 链数
                             discard_tuned_samples=True,  # tune的结果将在采样结束后被丢弃
                             random_seed=84735)  # 后验采样

az.summary(model3_trace)

# 定义 ROPE 区间，根据研究的需要指定实际等效范围
rope_interval = [-0.05, 0.05]

# 绘制后验分布，显示 HDI 和 ROPE
az.plot_posterior(
    model3_trace,
    var_names=["beta_4", "beta_5"],
    hdi_prob=0.95,
    rope=rope_interval,
    figsize=(12, 3),
    textsize=12
)

plt.show()

# 进行贝叶斯因子计算，需要采样先验分布
with model3:
    model3_trace.extend(pm.sample_prior_predictive(5000, random_seed=84735))

fig, axes = plt.subplots(1, 2, figsize=(9, 3.5))

# 绘制贝叶斯因子图
# beta4
ax = axes[0]
az.plot_bf(model3_trace, var_name="beta_4", ref_val=0, ax=ax)
ax.set_xlim(-0.5, 0.5)
# beta5
ax = axes[1]
az.plot_bf(model3_trace, var_name="beta_5", ref_val=0, ax=ax)
ax.set_xlim(-0.5, 0.5)

# 去除上框线和右框线
sns.despine()
plt.show()

with model3:
    model3_ppc = pm.sample_posterior_predictive(model3_trace, random_seed=84735)

az.plot_ppc(model3_ppc, num_pp_samples=500)

import xarray as xr

# 导入真实的自变量
X1 = xr.DataArray((df['Label'] == 'Friend').astype(int))
X2 = xr.DataArray((df['Label'] == 'Stranger').astype(int))
Matching = xr.DataArray((df['Matching'] == 'matching').astype(int))
Interaction_1 = X1 * Matching
Interaction_2 = X2 * Matching

model3_trace.posterior["y_model"] = model3_trace.posterior["beta_0"] + \
                                    (model3_trace.posterior["beta_1"] * X1) + \
                                    (model3_trace.posterior["beta_2"] * X2) + \
                                    (model3_trace.posterior["beta_3"] * Matching) + \
                                    (model3_trace.posterior["beta_4"] * Interaction_1) + \
                                    (model3_trace.posterior["beta_5"] * Interaction_2)

df["model3_prediction"] = model3_trace.posterior.y_model.mean(dim=["chain", "draw"]).values

df['Label'] = df['Label'].astype(str)

fig, axes = plt.subplots(1, 2, figsize=(8, 4))

# 绘制model3预测结果
plot_prediction(df, "model3_prediction", ax=axes[0])
axes[0].set_title("Model 3")

# 绘制model2预测结果
plot_prediction(df, "model2_prediction", ax=axes[1])
axes[1].set_title("Model 2")

# 显示图像
sns.despine()
plt.tight_layout()
plt.show()

print(axes)