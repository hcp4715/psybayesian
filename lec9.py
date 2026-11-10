# 导入 pymc 模型包，和 arviz 等分析工具 
import pymc as pm
import arviz as az
import seaborn as sns
import scipy.stats as st
import numpy as np
import matplotlib.pyplot as plt
import xarray as xr
import pandas as pd
from pathlib import Path

# 忽略不必要的警告
import warnings
warnings.filterwarnings("ignore")

# 与课件读取同一份项目数据。
df_raw = pd.read_csv(Path("data") / "Kolvoort_2020_HBM_Exp1_Clean.csv")

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

# Match the prior predictive demonstration in lec9_new.qmd.
np.random.seed(84735)
beta0_200 = np.random.normal(5, 2, 200)
beta1_200 = np.random.normal(0, 1, 200)
sigma_200 = np.random.exponential(1 / 0.3, 200)
prior_pred_sample = pd.DataFrame({"beta0": beta0_200, "beta1": beta1_200,
                                  "sigma": sigma_200})
print(prior_pred_sample.head())
x_sim = np.array([0, 1])  # Self, Other

# The first chart pairs all 200 parameter draws with one sampled mean line.
fig, axes = plt.subplots(1, 2, figsize=(12, 5),
                         gridspec_kw={"width_ratios": [1.1, 1]})
for name, color in [("beta0", "#1f77b4"), ("beta1", "#ff7f0e"),
                    ("sigma", "#2ca02c")]:
    axes[0].plot(np.arange(1, 201), prior_pred_sample[name],
                 color=color, linewidth=0.65, label=name)
axes[0].set(xlabel="Sample Index", ylabel="Value",
            title="Sampled Parameters: beta_0, beta_1, sigma")
axes[0].legend()
first_mu = prior_pred_sample.beta0.iloc[0] + prior_pred_sample.beta1.iloc[0] * x_sim
equation_label = (f"mu = {prior_pred_sample.beta0.iloc[0]:.2f} "
                  f"{prior_pred_sample.beta1.iloc[0]:+.2f} * X")
axes[1].plot(x_sim, first_mu, color="blue", label=equation_label)
axes[1].scatter(x_sim, first_mu, color="red")
axes[1].set(xlabel="X (Condition)", ylabel="Predicted Mean RT (sec)",
            title="Sampled Regression Line", xticks=x_sim,
            xticklabels=["self (0)", "other (1)"])
axes[1].legend()
sns.despine(fig=fig)
plt.tight_layout()
plt.show()

# Draw a fresh set, then simulate one observation per condition.
beta0_200 = np.random.normal(5, 2, 200)
beta1_200 = np.random.normal(0, 1, 200)
sigma_200 = np.random.exponential(1 / 0.3, 200)
prior_pred_sample = pd.DataFrame({"beta0": beta0_200, "beta1": beta1_200,
                                  "sigma": sigma_200})
beta_0 = prior_pred_sample.beta0.iloc[0]
beta_1 = prior_pred_sample.beta1.iloc[0]
sigma_1 = prior_pred_sample.sigma.iloc[0]
mu = beta_0 + beta_1 * x_sim
y_rep = np.random.normal(mu, sigma_1)
prior_pred_one = pd.DataFrame({"Condition": ["Self", "Other"],
                               "mu": mu, "y_rep": y_rep})
print(prior_pred_one)

# This exercise remains incomplete in the qmd too.
if False:
    x_axis = ...
    y_axis = ...
    plt.plot(x_axis, y_axis)

mu_outcome = []
y_rep_outcome = []
for row in prior_pred_sample.itertuples(index=False):
    mean = row.beta0 + row.beta1 * x_sim
    mu_outcome.append(mean)
    y_rep_outcome.append(np.random.normal(mean, row.sigma))

fig, axes = plt.subplots(1, 2, figsize=(12, 5))
for mean in mu_outcome:
    axes[0].plot(x_sim, mean, alpha=0.4, linewidth=0.5)
axes[0].set(xlabel="Label Condition", ylabel="Mean RT (sec)",
            title="Prior Predictive Means", xticks=x_sim,
            xticklabels=["Self", "Other"])
y_rep_plot_data = pd.DataFrame({"Condition": np.tile(["Self", "Other"], 200),
                                "y_rep": np.asarray(y_rep_outcome).ravel()})
sns.violinplot(data=y_rep_plot_data, x="Condition", y="y_rep",
               color="lightblue", ax=axes[1])
sns.stripplot(data=y_rep_plot_data, x="Condition", y="y_rep",
              alpha=0.4, size=1.5, ax=axes[1])
axes[1].axhline(0, linestyle="--", color="gray")
axes[1].set(xlabel="Label Condition", ylabel="Predicted RT (sec)",
            title="Prior Predictive Distribution")
sns.despine(fig=fig)
plt.tight_layout()
plt.show()


def prior_predictive_plot(beta0_mean=0.5, beta0_sd=0.3,
                          beta1_mean=-0.1, beta1_sd=0.04,
                          sigma_rate=0.2, samples=200, seed=84735):
    """Simulate prior mean lines and observed RTs for Self and Other."""
    rng = np.random.default_rng(seed)
    draws = pd.DataFrame({
        "beta0": rng.normal(beta0_mean, beta0_sd, samples),
        "beta1": rng.normal(beta1_mean, beta1_sd, samples),
        "sigma": rng.exponential(1 / sigma_rate, samples),
    })
    conditions = np.array([0, 1])
    means = np.asarray([row.beta0 + row.beta1 * conditions
                        for row in draws.itertuples(index=False)])
    predictions = rng.normal(means, draws.sigma.to_numpy()[:, None])
    fig, axes = plt.subplots(1, 2, figsize=(12, 5))
    for mean in means:
        axes[0].plot(conditions, mean, alpha=0.4, linewidth=0.5)
    axes[0].set(xlabel="Label Condition", ylabel="Mean RT (sec)",
                title="Prior Predictive Means", xticks=conditions,
                xticklabels=["Self", "Other"])
    plot_data = pd.DataFrame({"Condition": np.tile(["Self", "Other"], samples),
                              "y_rep": predictions.ravel()})
    sns.violinplot(data=plot_data, x="Condition", y="y_rep",
                   color="lightblue", ax=axes[1])
    sns.stripplot(data=plot_data, x="Condition", y="y_rep",
                  alpha=0.4, size=1.5, ax=axes[1])
    axes[1].axhline(0, linestyle="--", color="gray")
    axes[1].set(xlabel="Label Condition", ylabel="Predicted RT (sec)",
                title="Prior Predictive Distribution")
    sns.despine(fig=fig)
    plt.tight_layout()
    plt.show()
    return {"parameters": draws, "means": means,
            "predictions": predictions, "plot": fig}

# Fill in the hyperparameters before running the exercise.
if False:
    prior_predictive_plot(beta0_mean=..., beta0_sd=...,
                          beta1_mean=..., beta1_sd=...,
                          sigma_rate=..., samples=200, seed=84735)

import pymc as pm

with pm.Model() as linear_model:

    # 定义先验分布参数
    beta_0 = pm.Normal("beta_0", mu=5, sigma=2)        
    beta_1 = pm.Normal("beta_1", mu=0, sigma=1)      
    sigma = pm.Exponential("sigma", 0.3)

    # 定义自变量 x
    x = pm.Data("x", df['Label'].to_numpy())

    # 定义 mu，将自变量与先验结合
    mu = beta_0 + beta_1 * x

    # 定义似然：预测值y符合N(mu, sigma)分布
    likelihood = pm.Normal("y_est", mu=mu, sigma=sigma, observed=df['RT_sec'])  # 实际观测数据 y 是 RT

#===========================
#     注意！！！以下代码可能需要运行1-2分钟左右
#===========================
with linear_model:
    trace = pm.sample(draws=1000,                   # 4 链各保留 1000 次，共 4000 次
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
beta_0 = trace.posterior["beta_0"].stack(sample=("chain", "draw")).values[:2]
beta_1 = trace.posterior["beta_1"].stack(sample=("chain", "draw")).values[:2]

# 两组后验参数各生成一条均值回归线。
y_sim_re = beta_0[:, None] + beta_1[:, None] * x_sim[None, :]

# 绘制真实数据的散点图
plt.scatter(df["Label"], df["RT_sec"], c="r", label="observed data")

# 绘制回归线条
plt.plot(x_sim, y_sim_re.T, c="grey", label="Predicted Mean")
plt.scatter(np.tile(x_sim, 2), y_sim_re.ravel(), color="black", s=50)


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

# 将 4000 个后验样本存储在数据框中。
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

# 对每个后验样本重复预测。
for row_i in range(len(df_pos_sample)):
    mu_i = df_pos_sample.beta_0[row_i] + df_pos_sample.beta_1[row_i] * X_i
    df_pos_sample.loc[row_i, "mu"] = mu_i
    df_pos_sample.loc[row_i, "y_new"] = np.random.normal(
        loc=mu_i, scale=df_pos_sample.sigma[row_i]
    )

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

# 从 4000 个后验样本中选 1000 个进行后验预测检查。
az.plot_ppc(ppc_data, num_pp_samples=1000)

# Match the qmd: average predictive density on one shared grid.
ppc_samples = ppc_data.posterior_predictive["y_est"].values.reshape(-1, len(df))
y_observed = df["RT_sec"].to_numpy()
mean_obs = y_observed.mean()
q95_obs = np.quantile(y_observed, 0.95)
ppp_mean = np.mean(ppc_samples.mean(axis=1) >= mean_obs)
ppp_q95 = np.mean(np.quantile(ppc_samples, 0.95, axis=1) >= q95_obs)
print({"mean": ppp_mean, "q95": ppp_q95})

x_grid = np.linspace(ppc_samples.min(), ppc_samples.max(), 512)
average_density = np.mean(
    [st.gaussian_kde(draw)(x_grid) for draw in ppc_samples], axis=0
)
fig, ax = plt.subplots(figsize=(8, 5))
for draw in ppc_samples[:100]:
    ax.plot(x_grid, st.gaussian_kde(draw)(x_grid), color="lightblue",
            alpha=0.25, linewidth=0.5)
ax.plot(x_grid, st.gaussian_kde(y_observed)(x_grid), color="black",
        label="Observed density")
ax.plot(x_grid, average_density, color="orange", linestyle="--",
        label="Mean predictive density")
ax.set(xlabel="RT", ylabel="Density",
       title="Posterior Predictive Check with Mean Predictive Density")
ax.legend()
sns.despine(ax=ax)
plt.show() 

# 筛选编号为“205”的被试的数据
df_new = df_raw[(df_raw["Subject"] == "205") & (df_raw["Matching"] == "Matching")]

# 选择需要的两列
df_new = df_new[["Label", "RT_sec"]].copy()
df_new["Label"] = df_new["Label"].map({1: 0, 2: 1, 3: 1})

# 设置索引。
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
beta1_draws = trace.posterior["beta_1"].values.ravel()
print("95% HDI:", az.hdi(beta1_draws, hdi_prob=0.95))
print("ROPE 样本占比:", np.mean((beta1_draws >= rope_interval[0]) &
                              (beta1_draws <= rope_interval[1])))

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
priors = {"Intercept": beta_0,
          "Label": beta_1,
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
bambi_trace = model.fit(draws=1000,                   # 4 链各保留 1000 次，共 4000 次
                  tune=1000,                    # tune为调整采样策略的次数，可以决定这些结果是否要被保留
                  chains=4,
                  random_seed=84735)

ax = az.plot_trace(bambi_trace, figsize=(7,7), compact=False)
plt.tight_layout()
plt.show()

az.summary(bambi_trace)


