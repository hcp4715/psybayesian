# 导入 pymc 模型包，和 arviz 等分析工具 
import pymc as pm
import arviz as az
import seaborn as sns
import scipy.stats as st
import numpy as np
import matplotlib.pyplot as plt
import xarray as xr
import pandas as pd
import ipywidgets
import bambi as bmb

# 忽略不必要的警告
import warnings
warnings.filterwarnings("ignore")

# 通过 pd.read_csv 加载数据 Data_Sum_HPP_Multi_Site_Share.csv
try:
  df_raw = pd.read_csv('/home/mw/input/bayes3797/Data_Sum_HPP_Multi_Site_Share.csv')
except:
  df_raw = pd.read_csv('data/Data_Sum_HPP_Multi_Site_Share.csv')

# 选取所需站点
first5_site = ['Southampton','Portugal','Kassel','Tsinghua','UCSB']
df_first5 = df_raw.query("Site in @first5_site")
# 生成站点索引
df_first5["site_idx"] = pd.factorize(df_first5.Site)[0]
# 生成被试数索引
df_first5["obs_id"] = range(len(df_first5))
# 将站点、被试id设置为索引
df_first5.set_index(['Site','obs_id'],inplace=True,drop=False)

# 通过完全池化的方式可视化数据
sns.lmplot(df_first5,
           x="stress",
           y="scontrol",
           height=4, aspect=1.5)

# 定义坐标映射
coords = {"obs_id": df_first5.obs_id}

with pm.Model(coords=coords) as complete_pooled_model:

    beta_0 = pm.Normal("beta_0", mu=0, sigma=50)                #定义beta_0          
    beta_1 = pm.Normal("beta_1", mu=0, sigma=5)                 #定义beta_1
    sigma = pm.Exponential("sigma", 1)                          #定义sigma

    x = pm.MutableData("x", df_first5.stress, dims="obs_id")    #x是自变量压力水平

    mu = pm.Deterministic("mu",beta_0 + beta_1 * x, 
                          dims="obs_id")                        #定义mu，讲自变量与先验结合

    likelihood = pm.Normal("y_est", mu=mu, sigma=sigma, observed=df_first5.scontrol,
                           dims="obs_id")                       #定义似然：预测值y符合N(mu, sigma)分布
                                                                #通过 observed 传入实际数据y 自我控制水平
    complete_trace = pm.sample(random_seed=84735)
    
pm.model_to_graphviz(complete_pooled_model)

az.summary(complete_trace,
           var_names=["~mu"],
           filter_vars="like")

#提取不同站点数据对应的索引并储存，便于后续将后验预测数据按照站点进行提取
def get_group_index(data):
    group_index = {}
    for i, group in enumerate(data["Site"].unique()):
        group_index[group] = xr.DataArray(data.query(f"Site == '{group}'"))["obs_id"].values
    return group_index

#定义函数，绘制不同站点下的后验预测回归线
def plot_regression(data, trace, group_index):
    # 定义画布，根据站点数量定义画布的列数
    fig, ax = plt.subplots(1,len(data["Site"].unique()), 
                       sharex=True,
                       sharey=True,
                       figsize=(15,5))
    
    # 根据站点数来分别绘图
    # 我们需要的数据有原始数据，每一个因变量的后验预测均值
    # 这些数据都储存在后验参数采样结果中，也就是这里所用的trace
    for i, group in enumerate(data["Site"].unique()):
        #绘制真实数据的散点图
        x = trace.constant_data.x.sel(obs_id = group_index[f"{group}"])
        y = trace.observed_data.y_est.sel(obs_id = group_index[f"{group}"])
        mu = trace.posterior.mu.sel(obs_id = group_index[f"{group}"])
        ax[i].scatter(x, y,
                color=f"C{i}",
                alpha=0.5)
        #绘制回归线
        ax[i].plot(x, mu.stack(sample=("chain","draw")).mean(dim="sample"),
                color=f"C{i}",
                alpha=0.5)
        #绘制预测值95%HDI
        az.plot_hdi(
            x, mu,
            hdi_prob=0.95,
            fill_kwargs={"alpha": 0.25, "linewidth": 0},
            color=f"C{i}",
            ax=ax[i])
    # 生成横坐标名称
    fig.text(0.5, 0, 'Stress', ha='center', va='center', fontsize=12)
    # 生成纵坐标名称
    fig.text(0.08, 0.5, 'Self control', ha='center', va='center', rotation='vertical', fontsize=12)
    # 生成标题
    plt.suptitle("Posterior regression models", fontsize=15)
        
    sns.despine()
    
# 获取每个站点数据的索引
first5_index = get_group_index(data=df_first5)
# 进行可视化
plot_regression(data=df_first5,
                trace=complete_trace,
                group_index=first5_index)
# 定义函数来构建和采样模型
def run_var_inter_model():

    #定义数据坐标，包括站点和观测索引
    coords = {"site": df_first5["Site"].unique(),
              "obs_id": df_first5.obs_id}

    with pm.Model(coords=coords) as var_inter_model:
        #定义全局参数
        mu_beta0 = pm.Normal("mu_beta0", mu=40, sigma=20) # HCP: 我只修改了这里，后面需要相应调整
        sigma_beta0 = pm.Exponential("sigma_beta0", 1)    # HCP: 我只修改了这里，后面需要相应调整
        mu_beta1 = pm.Normal("mu_beta1", mu=0, sigma=5)   # HCP: 我只修改了这里，后面需要相应调整
        sigma_y = pm.Exponential("sigma_y", 1) 

        #传入自变量、获得观测值对应的站点映射
        x = pm.MutableData("x", df_first5.stress, dims="obs_id")
        site = pm.MutableData("site", df_first5.site_idx, dims="obs_id") 
        
        #模型定义
        beta_0j = pm.Normal("beta_0j", mu=beta_0, sigma=beta_0_sigma, dims="site")

        #线性关系
        mu = pm.Deterministic("mu", beta_0j[site]+beta_1*x, dims="obs_id")

        # 定义 likelihood
        likelihood = pm.Normal("y_est", mu=mu, sigma=sigma_y, observed=df_first5.scontrol, dims="obs_id")

        var_inter_trace = pm.sample(draws=5000,           # 使用mcmc方法进行采样，draws为采样次数
                            tune=1000,                    # tune为调整采样策略的次数，可以决定这些结果是否要被保留
                            chains=4,                     # 链数
                            discard_tuned_samples= True,  # tune的结果将在采样结束后被丢弃
                            random_seed=84735,
                            target_accept=0.99)
    
    return var_inter_model, var_inter_trace
  
# 注意，以下代码可能运行5分钟左右

var_inter_model, var_inter_trace = run_var_inter_model()

pm.model_to_graphviz(var_inter_model)

var_inter_prior = pm.sample_prior_predictive(samples=500,
                                            model=var_inter_model,
                                            random_seed=84735)

# 定义绘制先验预测回归线的函数，其逻辑与绘制后验预测回归线相同
def plot_prior(prior,group_index):
    # 定义画布，根据站点数量定义画布的列数
    fig, ax = plt.subplots(1,len(df_first5["Site"].unique()), 
                        sharex=True,
                        sharey=True,
                        figsize=(20,5))
    # 根据站点数来分别绘图
    # 我们需要的数据有原始数据中的自变量，每一个因变量的先验预测均值
    # 这些数据都储存在先验预测采样结果中，也就是这里所用的prior
    for i, group in enumerate(df_first5["Site"].unique()): 
        #绘制回归线
        ax[i].plot(prior.constant_data["x"].sel(obs_id = group_index[f"{group}"]),
                prior.prior["mu"].sel(obs_id = group_index[f"{group}"]).stack(sample=("chain","draw")),
                c='gray',
                alpha=0.5)
        ax[i].set_title(f"{group}")
    fig.text(0.5, 0, 'Stress', ha='center', va='center', fontsize=12)
    # 生成纵坐标名称
    fig.text(0.08, 0.5, 'Self control', ha='center', va='center', rotation='vertical', fontsize=12)
    # 生成标题
    plt.suptitle("Prior regression models", fontsize=15, y=1)
        
    sns.despine()
    
plot_prior(prior=var_inter_prior,
           group_index=first5_index)

# ~ 和filter_vars="like" 表示在显示结果时去除掉包含这些字符的变量
var_inter_para = az.summary(var_inter_trace,
           var_names=["~mu","~_sigma","~_offset","~sigma_"],
           filter_vars="like")
var_inter_para

az.plot_forest(var_inter_trace,
           var_names=["~mu", "~sigma", "~offset", "~beta_1"],
           filter_vars="like",
           combined = True)

#定义函数，绘制不同站点下的后验预测回归线
def plot_partial_regression(data, trace, group_index):
    # 定义画布，根据站点数量定义画布的列数
    fig, ax = plt.subplots(1,len(data["Site"].unique()), 
                       sharex=True,
                       sharey=True,
                       figsize=(15,5))
    
    # 根据站点数来分别绘图
    # 我们需要的数据有原始数据，每一个因变量的后验预测均值
    # 这些数据都储存在后验参数采样结果中，也就是这里所用的trace
    for i, group in enumerate(data["Site"].unique()):
        #绘制真实数据的散点图
        x = trace.constant_data.x.sel(obs_id = group_index[f"{group}"])
        y = trace.observed_data.y_est.sel(obs_id = group_index[f"{group}"])
        mu = trace.posterior.mu.sel(obs_id = group_index[f"{group}"])
        ax[i].scatter(x, y,
                color=f"C{i}",
                alpha=0.5)
        #绘制回归线
        ax[i].plot(x, mu.stack(sample=("chain","draw")).mean(dim="sample"),
                color=f"C{i}",
                alpha=0.5)
        ax[i].set_title(f"Intercept: {var_inter_para.loc[f'beta_0j[{group}]']['mean']}", fontsize=12)
        #绘制预测值95%HDI
        az.plot_hdi(
            x, mu,
            hdi_prob=0.95,
            fill_kwargs={"alpha": 0.25, "linewidth": 0},
            color=f"C{i}",
            ax=ax[i])
        
    # 生成横坐标名称
    fig.text(0.5, 0, 'Stress', ha='center', va='center', fontsize=12)
    # 生成纵坐标名称
    fig.text(0.08, 0.5, 'Self control', ha='center', va='center', rotation='vertical', fontsize=12)
    # 生成标题
    plt.suptitle("Posterior regression models(varing intercept)", fontsize=15, y=1.05)
        
    sns.despine()
    
plot_partial_regression(data=df_first5,
                trace=var_inter_trace,
                group_index=first5_index)

def calculate_var_odds(trace):
    # 提取组间和组内变异
    para_sum = az.summary(trace,
                        var_names=["mu","sigma_"],
                        filter_vars="like",
                        kind="stats"
                        )
    individual_par = para_sum.filter(like='mu', axis=0)["mean"]
    # 计算组间方差
    individual_par - individual_par.mean()
    normal_par = (individual_par - individual_par.mean()) / individual_par.std()
    tmp_df = df_first5.copy()
    tmp_df["mu"] = normal_par.values
    group_par = tmp_df.groupby("site_idx").mu.mean()
    between_sd = (group_par**2).sum()
    # 计算组内方差
    within_sd = para_sum.loc['sigma_y','mean']**2
    # 计算变异占比
    var = between_sd + within_sd
    print("被组间方差所解释的部分：", between_sd/var)
    print("被组内方差所解释的部分：", within_sd/var)
    print("组内相关：",between_sd/var)

calculate_var_odds(var_inter_trace)

# 定义函数来构建和采样模型
def run_var_slope_model():

    #定义数据坐标，包括站点和观测索引
    coords = {"site": df_first5["Site"].unique(),
            "obs_id": df_first5.obs_id}

    with pm.Model(coords=coords) as model:
        #定义全局参数
        beta_0 = pm.Normal("beta_0", mu=0, sigma=50)     # HCP: 变量命名需要修改
        beta_1 = pm.Normal("beta_1", mu=0, sigma=5) 
        beta_1_sigma = pm.Exponential("beta_1_sigma", 1)
        sigma_y = pm.Exponential("sigma_y", 1) 

        #传入自变量、获得观测值对应的站点映射
        x = pm.MutableData("x", df_first5.stress, dims="obs_id")
        site = pm.MutableData("site", df_first5.site_idx, dims="obs_id") 

        #模型定义
        beta_1j = pm.Normal("beta_1j", mu=beta_1, sigma=beta_1_sigma, dims="site")

        #线性关系
        mu = pm.Deterministic("mu", beta_0+beta_1j[site]*x, dims="obs_id")

        # 定义 likelihood
        likelihood = pm.Normal("y_est", mu=mu, sigma=sigma_y, observed=df_first5.scontrol, dims="obs_id")

        var_slope_trace = pm.sample(draws=5000,           # 使用mcmc方法进行采样，draws为采样次数
                            tune=1000,                    # tune为调整采样策略的次数，可以决定这些结果是否要被保留
                            chains=4,                     # 链数
                            discard_tuned_samples= True,  # tune的结果将在采样结束后被丢弃
                            random_seed=84735,
                            target_accept=0.99)
    
    return model, var_slope_trace
  
# 注意，以下代码可能运行5分钟左右

var_slope_model, var_slope_trace = run_var_slope_model()

pm.model_to_graphviz(var_slope_model)

var_slope_para = az.summary(var_slope_trace,
                            var_names=["beta_0","beta_1j"],
                            filter_vars="like")
var_slope_para 

az.plot_forest(var_slope_trace,
           var_names=["~mu", "~sigma", "~offset", "~beta_0"],
           filter_vars="like",
           combined = True)

#定义函数，绘制不同站点下的后验预测回归线
def plot_partial_regression(data, trace, group_index):
    # 定义画布，根据站点数量定义画布的列数
    fig, ax = plt.subplots(1,len(data["Site"].unique()), 
                       sharex=True,
                       sharey=True,
                       figsize=(15,5))
    
    # 根据站点数来分别绘图
    # 我们需要的数据有原始数据，每一个因变量的后验预测均值
    # 这些数据都储存在后验参数采样结果中，也就是这里所用的trace
    for i, group in enumerate(data["Site"].unique()):
        #绘制真实数据的散点图
        x = trace.constant_data.x.sel(obs_id = group_index[f"{group}"])
        y = trace.observed_data.y_est.sel(obs_id = group_index[f"{group}"])
        mu = trace.posterior.mu.sel(obs_id = group_index[f"{group}"])
        ax[i].scatter(x, y,
                color=f"C{i}",
                alpha=0.5)
        #绘制回归线
        ax[i].plot(x, mu.stack(sample=("chain","draw")).mean(dim="sample"),
                color=f"C{i}",
                alpha=0.5)
        ax[i].set_title(f"Slope: {var_slope_para.loc[f'beta_1j[{group}]']['mean']}", fontsize=12)
        #绘制预测值95%HDI
        az.plot_hdi(
            x, mu,
            hdi_prob=0.95,
            fill_kwargs={"alpha": 0.25, "linewidth": 0},
            color=f"C{i}",
            ax=ax[i])
        
    # 生成横坐标名称
    fig.text(0.5, 0, 'Stress', ha='center', va='center', fontsize=12)
    # 生成纵坐标名称
    fig.text(0.08, 0.5, 'Self control', ha='center', va='center', rotation='vertical', fontsize=12)
    # 生成标题
    plt.suptitle("Posterior regression models(varing slope)", fontsize=15, y=1.05)
        
    sns.despine()
    
plot_partial_regression(data=df_first5,
                trace=var_slope_trace,
                group_index=first5_index)

# 提取组间和组内变异
calculate_var_odds(var_slope_trace)

# 定义函数来构建和采样模型
def run_var_both_model():

    #定义数据坐标，包括站点和观测索引
    coords = {"site": df_first5["Site"].unique(),
            "obs_id": df_first5.obs_id}

    with pm.Model(coords=coords) as model:
        #定义全局参数
        mu_beta0 = pm.Normal("beta_0", mu=0, sigma=50)     # HCP: 变量名
        beta_0_sigma = pm.Exponential("beta_0_sigma", 1)
        mu_beta1 = pm.Normal("beta_1", mu=0, sigma=5) 
        beta_1_sigma = pm.Exponential("beta_1_sigma", 1)
        sigma_y = pm.Exponential("sigma_y", 1) 

        #传入自变量、获得观测值对应的站点映射
        x = pm.MutableData("x", df_first5.stress, dims="obs_id")
        site = pm.MutableData("site", df_first5.site_idx, dims="obs_id") 

        #模型定义
        beta_0j = pm.Normal("beta_0j", mu=beta_0, sigma=beta_0_sigma, dims="site")
        beta_1j = pm.Normal("beta_1j", mu=beta_1, sigma=beta_1_sigma, dims="site")

        #线性关系
        mu = pm.Deterministic("mu", beta_0j[site]+beta_1j[site]*x, dims="obs_id")

        # 定义 likelihood
        likelihood = pm.Normal("y_est", mu=mu, sigma=sigma_y, observed=df_first5.scontrol, dims="obs_id")

        var_both_trace  = pm.sample(draws=5000,           # 使用mcmc方法进行采样，draws为采样次数
                            tune=1000,                    # tune为调整采样策略的次数，可以决定这些结果是否要被保留
                            chains=4,                     # 链数
                            discard_tuned_samples= True,  # tune的结果将在采样结束后被丢弃
                            random_seed=84735,
                            target_accept=0.99)
    
    return model, var_both_trace 
  
# 注意，以下代码可能运行10分钟左右

var_both_model, var_both_trace = run_var_both_model()

pm.model_to_graphviz(var_both_model)

var_both_para = az.summary(var_both_trace,
                            var_names=["beta_0j","beta_1j"],
                            filter_vars="like")
var_both_para

# 设置绘图坐标
figs, (ax1, ax2) = plt.subplots(1,2, figsize = (20,5))
# 绘制变化的截距
az.plot_forest(var_both_trace,
           var_names=["~mu", "~sigma", "~offset", "~beta_1"],
           filter_vars="like",
           combined = True,
           ax=ax1)
# 绘制变化的斜率
az.plot_forest(var_both_trace,
           var_names=["~mu", "~sigma", "~offset", "~beta_0"],
           filter_vars="like",
           combined = True,
           ax=ax2)
plt.show()

#定义函数，绘制不同站点下的后验预测回归线
def plot_partial_regression(data, trace, group_index):
    # 定义画布，根据站点数量定义画布的列数
    fig, ax = plt.subplots(1,len(data["Site"].unique()), 
                       sharex=True,
                       sharey=True,
                       figsize=(15,5))
    
    # 根据站点数来分别绘图
    # 我们需要的数据有原始数据，每一个因变量的后验预测均值
    # 这些数据都储存在后验参数采样结果中，也就是这里所用的trace
    for i, group in enumerate(data["Site"].unique()):
        #绘制真实数据的散点图
        x = trace.constant_data.x.sel(obs_id = group_index[f"{group}"])
        y = trace.observed_data.y_est.sel(obs_id = group_index[f"{group}"])
        mu = trace.posterior.mu.sel(obs_id = group_index[f"{group}"])
        ax[i].scatter(x, y,
                color=f"C{i}",
                alpha=0.5)
        #绘制回归线
        ax[i].plot(x, mu.stack(sample=("chain","draw")).mean(dim="sample"),
                color=f"C{i}",
                alpha=0.5)
        ax[i].set_title(f"Slope: {var_both_para.loc[f'beta_1j[{group}]']['mean']}\nIntercept: {var_both_para.loc[f'beta_0j[{group}]']['mean']}", 
        fontsize=12)
        #绘制预测值95%HDI
        az.plot_hdi(
            x, mu,
            hdi_prob=0.95,
            fill_kwargs={"alpha": 0.25, "linewidth": 0},
            color=f"C{i}",
            ax=ax[i])
        
    # 生成横坐标名称
    fig.text(0.5, 0, 'Stress', ha='center', va='center', fontsize=12)
    # 生成纵坐标名称
    fig.text(0.08, 0.5, 'Self control', ha='center', va='center', rotation='vertical', fontsize=12)
    # 生成标题
    plt.suptitle("Posterior regression models(varing slope and intercept)", fontsize=15, y=1.05)
        
    sns.despine()
    
plot_partial_regression(data=df_first5,
                trace=var_both_trace,
                group_index=first5_index)

# 提取组间和组内变异
calculate_var_odds(var_both_trace)

# 进行后验预测
complete_ppc = pm.sample_posterior_predictive(complete_trace, 
                                            model = complete_pooled_model,
                                            random_seed=84735)
var_inter_ppc = pm.sample_posterior_predictive(var_inter_trace,
                                                model = var_inter_model,
                                                random_seed=84735)
var_slope_ppc = pm.sample_posterior_predictive(var_slope_trace,
                                                model = var_slope_model,
                                                random_seed=84735)                                                                                       
var_both_ppc = pm.sample_posterior_predictive(var_both_trace, 
                                            model = var_both_model,
                                            random_seed=84735)

# 定义计算 MAE 函数
from statistics import median
def MAE(model_ppc):
    # 计算每个X取值下对应的后验预测模型的均值
    pre_x = model_ppc.posterior_predictive["y_est"].stack(sample=("chain", "draw"))
    pre_y_mean = pre_x.mean(axis=1).values

    # 提取观测值Y，提取对应Y值下的后验预测模型的均值
    MAE = pd.DataFrame({
        "scontrol_ppc_mean": pre_y_mean,
        "scontrol_original": model_ppc.observed_data.y_est.values
    })

    # 计算预测误差
    MAE["pre_error"] = abs(MAE["scontrol_original"] -\
                            MAE["scontrol_ppc_mean"])

    # 最后，计算预测误差的中位数
    MAE = median(MAE.pre_error)
    return MAE

# 定义计算 MAE 函数
from statistics import median
def MAE(model_ppc):
    # 计算每个X取值下对应的后验预测模型的均值
    pre_x = model_ppc.posterior_predictive["y_est"].stack(sample=("chain", "draw"))
    pre_y_mean = pre_x.mean(axis=1).values

    # 提取观测值Y，提取对应Y值下的后验预测模型的均值
    MAE = pd.DataFrame({
        "scontrol_ppc_mean": pre_y_mean,
        "scontrol_original": model_ppc.observed_data.y_est.values
    })

    # 计算预测误差
    MAE["pre_error"] = abs(MAE["scontrol_original"] -\
                            MAE["scontrol_ppc_mean"])

    # 最后，计算预测误差的中位数
    MAE = median(MAE.pre_error)
    return MAE
  
# 定义计算 MAE 函数
from statistics import median
def MAE(model_ppc):
    # 计算每个X取值下对应的后验预测模型的均值
    pre_x = model_ppc.posterior_predictive["y_est"].stack(sample=("chain", "draw"))
    pre_y_mean = pre_x.mean(axis=1).values

    # 提取观测值Y，提取对应Y值下的后验预测模型的均值
    MAE = pd.DataFrame({
        "scontrol_ppc_mean": pre_y_mean,
        "scontrol_original": model_ppc.observed_data.y_est.values
    })

    # 计算预测误差
    MAE["pre_error"] = abs(MAE["scontrol_original"] -\
                            MAE["scontrol_ppc_mean"])

    # 最后，计算预测误差的中位数
    MAE = median(MAE.pre_error)
    return MAE

pm.compute_log_likelihood(var_inter_trace, model=var_inter_model)
pm.compute_log_likelihood(var_slope_trace, model=var_slope_model)
pm.compute_log_likelihood(var_both_trace, model=var_both_model)

comparison_list = {
    "model1(hierarchical intercept)":var_inter_trace,
    "model2(hierarchical slope)":var_slope_trace,
    "model3(hierarchy both)":var_both_trace,
}
az.compare(comparison_list)

# 选择站点为"Zurich"的数据
new_group = df_raw[df_raw.Site=="Zurich"]
# 生成被试索引
new_group["obs_id"] = range(len(new_group))
# 生成站点索引
new_group["site_idx"] = pd.factorize(new_group.Site)[0]

new_coords = {"site": new_group["Site"].unique(),
          "obs_id": new_group.obs_id}

with pm.Model(coords=new_coords) as hier_pred:
    #定义全局参数(这部分没有改变)
    beta_0 = pm.Normal("beta_0", mu=40, sigma=20)
    sigma_beta0 = pm.Exponential("sigma_beta0", 1)
    beta_1 = pm.Normal("beta_1", mu=0, sigma=5) 
    beta_1_sigma = pm.Exponential("beta_1_sigma", 1)
    sigma_y = pm.Exponential("sigma_y", 1) 

    #传入自变量
    x = pm.MutableData("x", new_group.stress, dims="obs_id")
    #获得观测值对应的站点映射
    site = pm.MutableData("site", new_group.site_idx, dims="obs_id") 
    
    #注意：在这里需要传入一个新的参数名，因为传入的是一个新站点(除此处外，其余的定义变量名未发生改变)
    new_beta_0_offset = pm.Normal("new_beta_0_offset", 0, sigma=1, dims="site")
    new_beta_0j = pm.Deterministic("new_beta_0j", beta_0 + new_beta_0_offset * sigma_beta0, dims="site")
    new_beta_1_offset = pm.Normal("new_beta_1_offset", 0, sigma=1, dims="site")
    new_beta_1j = pm.Deterministic("new_beta_1j", beta_1 + new_beta_1_offset * beta_1_sigma, dims="site")
    new_mu = pm.Normal("new_mu",  new_beta_0j[site]+new_beta_1j[site]*x, dims="obs_id")

    #似然
    likelihood = pm.Normal("y_est", mu=new_mu, sigma=sigma_y, observed=new_group.scontrol, dims="obs_id")

    # 进行后验预测估计，注意使用的是上一个模型的后验参数估计，partial_trace
    pred_trace = pm.sample_posterior_predictive(var_both_trace,
                                                var_names=["new_beta_0j","new_beta_1j"],
                                                predictions=True,
                                                extend_inferencedata=True,
                                                random_seed=84735)
    
    pred_trace
    
    #建立空dataframe，储存后验预测的结果
col = df_first5["Site"].unique()
pred_result = pd.DataFrame(columns=col)
#对每一个站点，提取后验参数的结果代入公式计算，并将计算结果存储在数据框的不同列
for site in df_first5["Site"].unique():
    pred = (40 * (var_both_trace.posterior['beta_1j'].sel(site = f"{site}")) +\
    (var_both_trace.posterior['beta_0j'].sel(site = f"{site}"))).stack(sample=("chain","draw")).values
    pred_result[site] = pred
    
#对于新站点，同样提取对应的参数并代入公式计算，将结果存在新列中
pred_trace = pred_trace.predictions.stack(sample=("chain","draw","site"))
new_group_pred = (40 * (pred_trace['new_beta_1j']) + (pred_trace['new_beta_0j'])).values
pred_result[new_group["Site"].unique()[0]] = new_group_pred 

pred_result

# 根据列数定义画布的行数
fig, ax = plt.subplots(len(pred_result.columns),1, figsize = (8,8),
                       sharex=True,
                       sharey=True)
# 对于每一个站点，绘制其预测结果的密度分布图
for i,site in enumerate(pred_result.columns):
    az.plot_kde(pred_result[site].to_numpy(),
                fill_kwargs={"alpha": 0.5},
                quantiles=[.25, .75],
                ax=ax[i])
    #设置y轴标题和刻度
    ax[i].set_ylabel(f'{site}', rotation=0, labelpad=40)
    ax[i].set_yticks([])
#设置x轴范围
plt.xlim([20,60])
#设置标题
plt.suptitle("Posterior predictive models for Self Control(X = 40)",
             y=0.95,
             fontsize=15)
sns.despine()

#选择需要的变量
df_temp =df_first5[["site_idx","obs_id","Site","stress","scontrol"]].reset_index(drop=True)
levels = df_temp['Site'].unique()

#生成各站点的夏季平均气温
level_mapping = {
    levels[0]: 17.6,
    levels[1]: 24.8,
    levels[2]: 19.9,
    levels[3]: 30.3,
    levels[4]: 19
}

#将气温信息合并到数据框中
df_temp['avetemp'] = df_temp['Site'].map(level_mapping)
df_temp 

sns.regplot(x="avetemp", y="scontrol", data=df_temp.groupby("Site").mean())
sns.despine()

# 我们采用第二种定义方式

#定义数据坐标，包括站点和观测索引
coords = {"site": df_temp["Site"].unique(),
        "obs_id": df_temp.obs_id}

with pm.Model(coords=coords) as group_pred_model:
    #定义全局参数
    gamma_0 = pm.Normal("gamma_0", mu=0, sigma=50)
    gamma_1 = pm.Normal("gamma_1", mu=0, sigma=5) 
    beta_1 = pm.Normal("beta_1", mu=0, sigma=5) 
    sigma_y = pm.Exponential("sigma_y", 1) 
    sigma_0 = pm.Exponential("sigma_0", 1)

    #传入自变量、获得观测值对应的站点映射
    x = pm.MutableData("x", df_temp.stress, dims="obs_id")
    u = pm.MutableData("u", df_temp.avetemp.unique(), dims="site")
    site_idx = pm.MutableData("site", df_temp.site_idx, dims="obs_id") 

    #定义组层面变量
    beta_0 = pm.Deterministic("beta_0", gamma_0 + gamma_1*u, dims="site")
    beta_0_offset = pm.Normal("beta_0_offset", 0, sigma=1, dims="site")
    beta_0j = pm.Deterministic("beta_0j", beta_0 + sigma_0*beta_0_offset, dims="site")

    #线性关系
    mu = pm.Deterministic("mu", beta_0j[site_idx]+beta_1*x, dims="obs_id")
 
    # 定义 likelihood
    likelihood = pm.Normal("y_est", mu=mu, sigma=sigma_y, observed=df_temp.scontrol, dims="obs_id")

    group_pred_trace = pm.sample(draws=5000,           # 使用mcmc方法进行采样，draws为采样次数
                        tune=1000,                    # tune为调整采样策略的次数，可以决定这些结果是否要被保留
                        chains=4,                     # 链数
                        discard_tuned_samples= True,  # tune的结果将在采样结束后被丢弃
                        random_seed=84735,
                        target_accept=0.99)
    
pm.model_to_graphviz(group_pred_model)

#===============================================
# bambi code (补充内容)

group_pred_bmb = bmb.Model("scontrol ~ stress + avetemp + (stress|Site)",
                      df_temp,
                      categorical="Site")

group_pred_bmb.build()
group_pred_bmb.graph()

az.summary(group_pred_trace,
           var_names=["gamma_0","gamma_1","beta_0j","beta_1"])

group_pred_trace

# 提取每个站点的温度
u = group_pred_trace.constant_data.u
# 提取组层面截距与温度的关系，体现在beta_0: beta_0 = gamma_0 + gamma_1 * u
beta_0 = group_pred_trace.posterior.beta_0.mean(dim=("chain","draw")).values
#提取每个站点的截距
beta_0j = group_pred_trace.posterior.beta_0j.mean(dim=("chain","draw"))
temp_hdi = az.hdi(group_pred_trace.posterior.beta_0j)
# 绘制每个站点的截距均值
plt.scatter(u, beta_0j,
        color="black",
        alpha=0.5,
        label="Mean site-intercept")
#绘制截距与温度之间的关系
plt.plot(u, beta_0,
        color="red",
        alpha=0.5,
        label="Mean intercept")
#绘制每个站点截距95%HDI
az.plot_hdi(
        u, group_pred_trace.posterior.beta_0j,
        hdi_prob=0.95,
        fill_kwargs={"alpha": 0.1, "color": "k", "label": "Mean intercept HPD"}
        )
#生成横坐标名称
plt.xlabel('Site-level temparature', fontsize=12)
# 生成纵坐标名称
plt.ylabel('Intercept estimate', fontsize=12)
plt.legend(loc="upper right")

sns.despine()

#===============================================
# 练习

# 导入 pymc 模型包，和 arviz 等分析工具 
import pymc as pm
import arviz as az
import seaborn as sns
import scipy.stats as st
import numpy as np
import matplotlib.pyplot as plt
import xarray as xr
import pandas as pd
import ipywidgets
import bambi as bmb

# 忽略不必要的警告
import warnings
warnings.filterwarnings("ignore")

# 使用 pandas 导入示例数据
try:
  df_raw  = pd.read_csv("/home/mw/input/bayes3797/evans2020JExpPsycholLearn_exp1_full_data.csv") 
except:
  df_raw  = pd.read_csv('data/evans2020JExpPsycholLearn_exp1_full_data.csv')

df_raw[["subject","RT"]]

# 筛选出特定被试并创建索引
df_first5 = df_raw[df_raw['subject'].isin([81844, 83956, 83824, 66670, 80941])].query("percentCoherence in [5, 10]").copy()

# 为每个被试建立索引 'subj_id' 和 'obs_id'
df_first5['subj_id'] = df_first5['subject']
df_first5['obs_id'] = df_first5.groupby('subject').cumcount() + 1

# 对反应时间取对数
df_first5["log_RTs"] = np.log(df_first5["RT"])

# 为每一行生成全局唯一编号 'global_id'
df_first5['global_id'] = range(len(df_first5))

df_first5.head()

# 建立被试 ID 映射表
subject_mapping = {subj_id: idx for idx, subj_id in enumerate(df_first5["subj_id"].unique())}

# 将被试 ID 转换为索引
mapped_subject_id = df_first5["subj_id"].map(subject_mapping).values

# 定义函数来构建和采样模型
def run_var_inter_model():

    #定义数据坐标
    coords = {
    "subject": df_first5["subj_id"].unique(),
    "obs_id": df_first5["global_id"]}

    with pm.Model(coords=coords) as var_inter_model:
        
        # 对 RT 进行 log 变换
        log_RTs = pm.MutableData("log_RTs", np.log(df_first5['RT']))
        
        #定义全局参数
        beta_0 = pm.Normal("beta_0", mu=7, sigma=1)
        beta_0_sigma = pm.Exponential("beta_0_sigma", 1)
        beta_1 = pm.Normal("beta_1", mu=7.5, sigma=5)
        sigma_y = pm.Exponential("sigma_y", 1) 

        #传入自变量、获得观测值对应的被试映射
        x = pm.MutableData("x", df_first5.percentCoherence, dims="obs_id")
        subject = pm.MutableData("subject_id", mapped_subject_id, dims="obs_id") 
        
        #模型定义
        beta_0j = pm.Normal("beta_0j", mu=beta_0, sigma=beta_0_sigma, dims="subject")

        #线性关系
        mu = pm.Deterministic("mu", beta_0j[subject]+beta_1*x, dims="obs_id")

        # 定义 likelihood
        likelihood = pm.Normal("y_est", mu=mu, sigma=sigma_y, observed=log_RTs, dims="obs_id")

        var_inter_trace = pm.sample(draws=1000,           # 使用mcmc方法进行采样，draws为采样次数
                            tune=1000,                    # tune为调整采样策略的次数，可以决定这些结果是否要被保留
                            chains=4,                     # 链数
                            discard_tuned_samples= True,  # tune的结果将在采样结束后被丢弃
                            random_seed=84735,
                            target_accept=0.99)
    
    return var_inter_model, var_inter_trace
  
  # 注意，以下代码可能运行5分钟左右

var_inter_model, var_inter_trace = run_var_inter_model()

pm.model_to_graphviz(var_inter_model)

var_inter_prior = pm.sample_prior_predictive(samples=500,
                                            model=var_inter_model,
                                            random_seed=84735)

#提取不同站点数据对应的索引并储存，便于后续将后验预测数据按照站点进行提取
def get_group_index(data):
    group_index = {}
    for i, group in enumerate(data["subj_id"].unique()):
        group_index[group] = data.loc[data["subj_id"] == group]["global_id"].values
    return group_index

# 定义绘制先验预测回归线的函数，其逻辑与绘制后验预测回归线相同
def plot_prior(prior,group_index):
    
    # 定义画布，根据站点数量定义画布的列数
    fig, ax = plt.subplots(1,len(df_first5["subj_id"].unique()), 
                        sharex=True,
                        sharey=True,
                        figsize=(20,5))
   
    for i, group in enumerate(df_first5["subj_id"].unique()): 
        #绘制回归线
        ax[i].plot(
            prior.constant_data["x"].sel(obs_id= group_index[group]),
            prior.prior["mu"].sel(obs_id = group_index[group]).stack(sample=("chain","draw")),
            c='gray',
            alpha=0.5
        )
        ax[i].set_title(f"{group}")
    fig.text(0.5, 0, 'percentCoherence', ha='center', va='center', fontsize=12)
    # 生成纵坐标名称
    fig.text(0.08, 0.5, 'RT', ha='center', va='center', rotation='vertical', fontsize=12)
    # 生成标题
    plt.suptitle("Prior regression models", fontsize=15, y=1)
        
    sns.despine()
    
first5_index = get_group_index(data=df_first5)

plot_prior(prior=var_inter_prior,
           group_index=first5_index)

# ~ 和filter_vars="like" 表示在显示结果时去除掉包含这些字符的变量
var_inter_para = az.summary(var_inter_trace,
           var_names=["~mu","~_sigma","~_offset","~sigma_"],
           filter_vars="like")
var_inter_para

az.plot_forest(var_inter_trace,
           var_names=["~mu", "~sigma", "~offset", "~beta_1"],
           filter_vars="like",
           combined = True)

#定义函数，绘制不同站点下的后验预测回归线
def plot_partial_regression(data, trace, group_index):
    # 定义画布，根据站点数量定义画布的列数
    fig, ax = plt.subplots(1,len(data["subj_id"].unique()), 
                       sharex=True,
                       sharey=True,
                       figsize=(15,5))
    
    # 根据站点数来分别绘图
    # 我们需要的数据有原始数据，每一个因变量的后验预测均值
    # 这些数据都储存在后验参数采样结果中，也就是这里所用的trace
    for i, group in enumerate(data["subj_id"].unique()):
        #绘制真实数据的散点图
        x = trace.constant_data.x.sel(obs_id = group_index[group])
        y = trace.observed_data.y_est.sel(obs_id = group_index[group])
        mu = trace.posterior.mu.sel(obs_id = group_index[group])
        ax[i].scatter(x, y,
                color=f"C{i}",
                alpha=0.5)
        #绘制回归线
        ax[i].plot(x, mu.stack(sample=("chain","draw")).mean(dim="sample"),
                color=f"C{i}",
                alpha=0.5)
        ax[i].set_title(f"Intercept: {var_inter_para.loc[f'beta_0j[{group}]']['mean']}", fontsize=12)
        #绘制预测值95%HDI
        az.plot_hdi(
            x, mu,
            hdi_prob=0.95,
            fill_kwargs={"alpha": 0.25, "linewidth": 0},
            color=f"C{i}",
            ax=ax[i])
        
    # 生成横坐标名称
    fig.text(0.5, 0, 'percentCoherence', ha='center', va='center', fontsize=12)
    # 生成纵坐标名称
    fig.text(0.08, 0.5, 'RT', ha='center', va='center', rotation='vertical', fontsize=12)
    # 生成标题
    plt.suptitle("Posterior regression models(varing intercept)", fontsize=15, y=1.05)
        
    sns.despine()
    
plot_partial_regression(data=df_first5,
                trace=var_inter_trace,
                group_index=first5_index)

def calculate_var_odds(trace):
    # 提取组间和组内变异
    para_sum = az.summary(trace,
                        var_names=["mu","sigma_"],
                        filter_vars="like",
                        kind="stats"
                        )
    individual_par = para_sum.filter(like='mu', axis=0)["mean"]
    # 计算组间方差
    individual_par - individual_par.mean()
    normal_par = (individual_par - individual_par.mean()) / individual_par.std()
    tmp_df = df_first5.copy()
    tmp_df["mu"] = normal_par.values
    group_par = tmp_df.groupby("subject").mu.mean()
    between_sd = (group_par**2).sum()
    # 计算组内方差
    within_sd = para_sum.loc['sigma_y','mean']**2
    # 计算变异占比
    var = between_sd + within_sd
    print("被组间方差所解释的部分：", between_sd/var)
    print("被组内方差所解释的部分：", within_sd/var)
    print("组内相关：",between_sd/var)

calculate_var_odds(var_inter_trace)

# 定义函数来构建和采样模型
def run_var_slope_model():

    #定义数据坐标
    coords = {
    "subject": df_first5["subj_id"].unique(),
    "obs_id": df_first5["global_id"]}

    with pm.Model(coords=coords) as model:
        
        # 对 RT 进行 log 变换
        log_RTs = pm.MutableData("log_RTs", np.log(df_first5['RT']))
        
        #定义全局参数
        beta_0 = pm.Normal("beta_0", mu=7, sigma=1)
        beta_1 = pm.Normal("beta_1", mu=7.5, sigma=5)
        beta_1_sigma = pm.Exponential("beta_1_sigma", 1)
        sigma_y = pm.Exponential("sigma_y", 1) 

        #传入自变量、获得观测值对应的被试映射
        x = pm.MutableData("x", df_first5.percentCoherence, dims="obs_id")
        subject = pm.MutableData("subject_id", mapped_subject_id, dims="obs_id") 
        
        #模型定义
        beta_1j = pm.Normal("beta_1j", mu=beta_1, sigma=beta_1_sigma, dims="subject")

        #线性关系
        mu = pm.Deterministic("mu", beta_0+beta_1j[subject]*x, dims="obs_id")

        # 定义 likelihood
        likelihood = pm.Normal("y_est", mu=mu, sigma=sigma_y, observed=log_RTs, dims="obs_id")

        var_slope_trace = pm.sample(draws=1000,           # 使用mcmc方法进行采样，draws为采样次数
                            tune=1000,                    # tune为调整采样策略的次数，可以决定这些结果是否要被保留
                            chains=4,                     # 链数
                            discard_tuned_samples= True,  # tune的结果将在采样结束后被丢弃
                            random_seed=84735,
                            target_accept=0.99)
    
    return model, var_slope_trace
  
  # 注意，以下代码可能运行5分钟左右
var_slope_model, var_slope_trace = run_var_slope_model()

pm.model_to_graphviz(var_slope_model)

var_slope_para = az.summary(var_slope_trace,
                            var_names=["beta_0","beta_1j"],
                            filter_vars="like")
var_slope_para 

az.plot_forest(var_slope_trace,
           var_names=["~mu", "~sigma", "~offset", "~beta_0"],
           filter_vars="like",
           combined = True)

#定义函数，绘制不同站点下的后验预测回归线
def plot_partial_regression(data, trace, group_index):
    # 定义画布，根据站点数量定义画布的列数
    fig, ax = plt.subplots(1,len(data["subj_id"].unique()), 
                       sharex=True,
                       sharey=True,
                       figsize=(15,5))
    
    # 根据站点数来分别绘图
    # 我们需要的数据有原始数据，每一个因变量的后验预测均值
    # 这些数据都储存在后验参数采样结果中，也就是这里所用的trace
    for i, group in enumerate(data["subj_id"].unique()):
        #绘制真实数据的散点图
        x = trace.constant_data.x.sel(obs_id = group_index[group])
        y = trace.observed_data.y_est.sel(obs_id = group_index[group])
        mu = trace.posterior.mu.sel(obs_id = group_index[group])
        ax[i].scatter(x, y,
                color=f"C{i}",
                alpha=0.5)
        #绘制回归线
        ax[i].plot(x, mu.stack(sample=("chain","draw")).mean(dim="sample"),
                color=f"C{i}",
                alpha=0.5)
        ax[i].set_title(f"Slope: {var_slope_para.loc[f'beta_1j[{group}]']['mean']}", fontsize=12)
        #绘制预测值95%HDI
        az.plot_hdi(
            x, mu,
            hdi_prob=0.95,
            fill_kwargs={"alpha": 0.25, "linewidth": 0},
            color=f"C{i}",
            ax=ax[i])
        
    # 生成横坐标名称
    fig.text(0.5, 0, 'percentCoherence', ha='center', va='center', fontsize=12)
    # 生成纵坐标名称
    fig.text(0.08, 0.5, 'RT', ha='center', va='center', rotation='vertical', fontsize=12)
    # 生成标题
    plt.suptitle("Posterior regression models(varing slope)", fontsize=15, y=1.05)
        
    sns.despine()
    
plot_partial_regression(data=df_first5,
                trace=var_slope_trace,
                group_index=first5_index)

def calculate_var_odds(trace):
    # 提取组间和组内变异
    para_sum = az.summary(trace,
                        var_names=["mu","sigma_"],
                        filter_vars="like",
                        kind="stats"
                        )
    individual_par = para_sum.filter(like='mu', axis=0)["mean"]
    # 计算组间方差
    individual_par - individual_par.mean()
    normal_par = (individual_par - individual_par.mean()) / individual_par.std()
    tmp_df = df_first5.copy()
    tmp_df["mu"] = normal_par.values
    group_par = tmp_df.groupby("subject").mu.mean()
    between_sd = (group_par**2).sum()
    # 计算组内方差
    within_sd = para_sum.loc['sigma_y','mean']**2
    # 计算变异占比
    var = between_sd + within_sd
    print("被组间方差所解释的部分：", between_sd/var)
    print("被组内方差所解释的部分：", within_sd/var)
    print("组内相关：",between_sd/var)

calculate_var_odds(var_slope_trace)

# 定义函数来构建和采样模型
def run_var_both_model():

    #定义数据坐标
    coords = {
    "subject": df_first5["subj_id"].unique(),
    "obs_id": df_first5["global_id"]}

    with pm.Model(coords=coords) as model:
        # 对 RT 进行 log 变换
        log_RTs = pm.MutableData("log_RTs", np.log(df_first5['RT']))

        #定义全局参数
        beta_0 = pm.Normal("beta_0", mu=7, sigma=1)
        beta_1 = pm.Normal("beta_1", mu=7.5, sigma=5) 
        beta_0_sigma = pm.Exponential("beta_0_sigma", 1)
        beta_1_sigma = pm.Exponential("beta_1_sigma", 1)
        sigma_y = pm.Exponential("sigma_y", 1) 

        #传入自变量、获得观测值对应的被试映射
        x = pm.MutableData("x", df_first5.percentCoherence, dims="obs_id")
        subject = pm.MutableData("subject_id", mapped_subject_id, dims="obs_id") 

        #模型定义
        beta_0j = pm.Normal("beta_0j", mu=beta_0, sigma=beta_0_sigma, dims="subject")
        beta_1j = pm.Normal("beta_1j", mu=beta_1, sigma=beta_1_sigma, dims="subject")

        #线性关系
        mu = pm.Deterministic("mu", beta_0j[subject]+beta_1j[subject]*x, dims="obs_id")

        # 定义 likelihood
        likelihood = pm.Normal("y_est", mu=mu, sigma=sigma_y, observed=log_RTs, dims="obs_id")

        var_both_trace = pm.sample(draws=1000,           # 使用mcmc方法进行采样，draws为采样次数
                            tune=1000,                    # tune为调整采样策略的次数，可以决定这些结果是否要被保留
                            chains=4,                     # 链数
                            discard_tuned_samples= True,  # tune的结果将在采样结束后被丢弃
                            random_seed=84735,
                            target_accept=0.99)
    
    return model, var_both_trace
  
  # 注意，以下代码可能运行10分钟左右

var_both_model, var_both_trace = run_var_both_model()

pm.model_to_graphviz(var_both_model)

var_both_para = az.summary(var_both_trace,
                            var_names=["beta_0j","beta_1j"],
                            filter_vars="like")
var_both_para

# 设置绘图坐标
figs, (ax1, ax2) = plt.subplots(1,2, figsize = (20,5))
# 绘制变化的截距
az.plot_forest(var_both_trace,
           var_names=["~mu", "~sigma", "~offset", "~beta_1"],
           filter_vars="like",
           combined = True,
           ax=ax1)
# 绘制变化的斜率
az.plot_forest(var_both_trace,
           var_names=["~mu", "~sigma", "~offset", "~beta_0"],
           filter_vars="like",
           combined = True,
           ax=ax2)
plt.show()

#定义函数，绘制不同站点下的后验预测回归线
def plot_partial_regression(data, trace, group_index):
    # 定义画布，根据站点数量定义画布的列数
    fig, ax = plt.subplots(1,len(data["subj_id"].unique()), 
                       sharex=True,
                       sharey=True,
                       figsize=(15,5))
    
    for i, group in enumerate(data["subj_id"].unique()):
        #绘制真实数据的散点图
        x = trace.constant_data.x.sel(obs_id = group_index[group])
        y = trace.observed_data.y_est.sel(obs_id = group_index[group])
        mu = trace.posterior.mu.sel(obs_id = group_index[group])
        ax[i].scatter(x, y,
                color=f"C{i}",
                alpha=0.5)
        #绘制回归线
        ax[i].plot(x, mu.stack(sample=("chain","draw")).mean(dim="sample"),
                color=f"C{i}",
                alpha=0.5)
        ax[i].set_title(f"Slope: {var_both_para.loc[f'beta_1j[{group}]']['mean']}\nIntercept: {var_both_para.loc[f'beta_0j[{group}]']['mean']}", 
        fontsize=12)
        #绘制预测值95%HDI
        az.plot_hdi(
            x, mu,
            hdi_prob=0.95,
            fill_kwargs={"alpha": 0.25, "linewidth": 0},
            color=f"C{i}",
            ax=ax[i])
        
    # 生成横坐标名称
    fig.text(0.5, 0, 'percentCoherence', ha='center', va='center', fontsize=12)
    # 生成纵坐标名称
    fig.text(0.08, 0.5, 'RT', ha='center', va='center', rotation='vertical', fontsize=12)
    # 生成标题
    plt.suptitle("Posterior regression models(varing slope and intercept)", fontsize=15, y=1.05)
        
    sns.despine()
    
    plot_partial_regression(data=df_first5,
                trace=var_both_trace,
                group_index=first5_index)
    
    calculate_var_odds(var_inter_trace)
    
# 对三种不同情况下的部分池化模型进行后验预测 
var_inter_ppc = pm.sample_posterior_predictive(var_inter_trace,
                                                model = var_inter_model,
                                                random_seed=84735)
var_slope_ppc = pm.sample_posterior_predictive(var_slope_trace,
                                                model = var_slope_model,
                                                random_seed=84735)                                                                                       
var_both_ppc = pm.sample_posterior_predictive(var_both_trace, 
                                            model = var_both_model,
                                            random_seed=84735)

# 定义计算 MAE 函数
from statistics import median
def MAE(model_ppc):
    # 计算每个X取值下对应的后验预测模型的均值
    pre_x = model_ppc.posterior_predictive["y_est"].stack(sample=("chain", "draw"))
    pre_y_mean = pre_x.mean(axis=1).values

    # 提取观测值Y，提取对应Y值下的后验预测模型的均值
    MAE = pd.DataFrame({
        "scontrol_ppc_mean": pre_y_mean,
        "scontrol_original": model_ppc.observed_data.y_est.values
    })

    # 计算预测误差
    MAE["pre_error"] = abs(MAE["scontrol_original"] -\
                            MAE["scontrol_ppc_mean"])

    # 最后，计算预测误差的中位数
    MAE = median(MAE.pre_error)
    return MAE
  
# 定义
def counter_outlier(model_ppc, hdi_prob=0.95):
    # 将az.summary生成的结果存到hdi_multi这个变量中，该变量为数据框
    hdi = az.summary(model_ppc, kind="stats", hdi_prob=hdi_prob)
    lower = hdi.iloc[:,2].values
    upper = hdi.iloc[:,3].values

    # 将原数据中的自我控制分数合并，便于后续进行判断
    y_obs = model_ppc.observed_data["y_est"].values

    # 判断原数据中的压力分数是否在后验预测的95%可信区间内，并计数
    hdi["verify"] = (y_obs <= lower) | (y_obs >= upper)
    hdi["y_obs"] = y_obs
    hdi_num = sum(hdi["verify"])

    return hdi_num
  
# 将每个模型的PPC储存为列表
ppc_samples_list = [var_inter_ppc, var_slope_ppc, var_both_ppc]
model_names = ["变化截距", "变化斜率", "变化截距、斜率"]

# 建立一个空列表来存储结果
results_list = []

# 遍历模型并计算MAE和超出95%hdi的值
for model_name, ppc_samples in zip(model_names, ppc_samples_list):
    outliers = counter_outlier(ppc_samples)
    MAEs = MAE(ppc_samples)
    results_list.append({'Model': model_name, 'MAE':MAEs, 'Outliers': outliers})

# 从结果列表创建一个DataFrame
results_df = pd.DataFrame(results_list)

results_df

pm.compute_log_likelihood(var_inter_trace, model=var_inter_model)
pm.compute_log_likelihood(var_slope_trace, model=var_slope_model)
pm.compute_log_likelihood(var_both_trace, model=var_both_model)

comparison_list = {
    "model1(hierarchical intercept)":var_inter_trace,
    "model2(hierarchical slope)":var_slope_trace,
    "model3(hierarchy both)":var_both_trace,
}
az.compare(comparison_list)