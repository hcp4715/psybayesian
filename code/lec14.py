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

# 计算每个被试的平均反应时间和标准误差
subject_stats = df_raw.groupby("subject")["RT"].agg(['mean', 'std', 'count']).reset_index()
subject_stats['sem'] = subject_stats['std'] / np.sqrt(subject_stats['count'])

# 按照平均反应时间从高到低排序
subject_stats.sort_values(by="mean", ascending=False, inplace=True, ignore_index=True)
subject_stats.subject = subject_stats.subject.astype(str)

# 绘制平均反应时间的可视化
plt.figure(figsize=(13, 6))
sns.barplot(x="subject", y="mean", data=subject_stats, color = "skyblue")

# 添加误差线
for i, row in subject_stats.iterrows():
    plt.errorbar(x=i, y=row['mean'], yerr=row['sem'], linestyle='', capsize=5, color = "darkblue")

plt.title("Average Reaction Time by Subject")
plt.xlabel("Subject")
plt.ylabel("Mean Reaction Time (RT)")
plt.xticks(rotation=45)
plt.tight_layout()
sns.despine()
plt.show()

# 筛选出特定被试并创建索引
df_first5 = df_raw[df_raw['subject'].isin([81844, 83956, 83824, 66670, 80941]) & (df_raw['percentCoherence'] == 5)]

# 为每个被试建立索引 'subj_id' 和 'obs_id'
df_first5['subj_id'] = df_first5['subject']
df_first5['obs_id'] = df_first5.groupby('subject').cumcount() + 1

df_first5["log_RTs"] = np.log(df_first5["RT"])

df_first5.head()

# 创建一个包含两个子图的 2 行 5 列布局
fig, axes = plt.subplots(2, 5, figsize=(20, 6))  

# 绘制第一个子图：原始的RT
df_first5.hist(column="RT", by="subject", ax=axes[0], figsize=(13, 3), layout=(1, 5))
for ax in axes[0]: 
    ax.tick_params(axis='x', rotation=0)

# 绘制第二个子图：logRT
df_first5.assign(logRT = np.log(df_first5['RT'])).hist(column="logRT", by="subject", ax=axes[1], figsize=(13, 3), layout=(1, 5))
for ax in axes[1]:  
    ax.tick_params(axis='x', rotation=0)

# 调整布局
plt.tight_layout()
sns.despine()
plt.show()

# 绘制所有被试的反应时间 (RT) 的分布图
plt.figure(figsize=(7, 4))
plt.hist(df_first5['RT'], bins=50, edgecolor='black', alpha=0.7)
plt.title('Distribution of Reaction Times (RT)', fontsize=16)
plt.xlabel('Reaction Time (ms)', fontsize=14)
plt.ylabel('Frequency', fontsize=14)
sns.despine()
plt.show()

with pm.Model() as complete_pooled_model:

    # 对 RT 进行 log 变换
    log_RTs = pm.MutableData("log_RTs", np.log(df_first5['RT']))

    #定义 mu, 注意已经考虑到 RT 已经进行 log 转换
    mu = pm.Normal("mu", mu=7.5, sigma=5)  
    #定义sigma                  
    sigma = pm.Exponential("sigma", 1) 

    #定义似然：预测值y符合N(mu, sigma)分布；传入实际数据y 反应时间 log_RTs
    likelihood = pm.Normal("y_est", mu=mu, sigma=sigma, observed=log_RTs)  

    complete_trace = pm.sample(random_seed=84735)
    
axes = az.plot_trace(complete_trace,
              compact=False,
              figsize=(7,4))
plt.tight_layout()
plt.show()

az.summary(complete_trace)

def inv_log(mu, sigma):
    return np.exp(mu + (sigma ** 2) / 2)

pred_rt = inv_log(6.950,0.756)
print("The estimated mean of RT is: ", pred_rt.round(3))

print("The posterior mean of mu is: ", az.summary(complete_trace)["mean"][0].round(3))
print("The truth log RT mean is:", np.log(df_first5['RT']).mean().round(3))

# 进行后验预测
complete_ppc = pm.sample_posterior_predictive(complete_trace,
                                              model=complete_pooled_model)

# 定义函数，计算 95%hdi
def ppc_sum(ppc, data, y = "RT"):
    
    hdi_sum = az.summary(ppc, hdi_prob=0.95, kind="stats")
    hdi_sum["y"] = data[y].values
    hdi_sum["obs_id"] = data.reset_index(drop=True).index.values
    hdi_sum["subject"] = data["subject"].values

    return hdi_sum
  
# 计算后验预测的 95%hdi
complete_hdi_sum = ppc_sum(ppc = complete_ppc, data=df_first5)
complete_hdi_sum.head()

def inv_log(mu, sigma):
    return np.exp(mu + (sigma ** 2) / 2)

def inv_log_hdi_sum(hdi_sum):
    
    df = hdi_sum.copy()
    
    df["mean"] = inv_log(df["mean"], df["sd"])
    df.iloc[:, 2] = inv_log(df.iloc[:, 2], df["sd"])
    df.iloc[:, 3] = inv_log(df.iloc[:, 3], df["sd"])
    
    return df

complete_hdi_sum = inv_log_hdi_sum(complete_hdi_sum)

import matplotlib.pyplot as plt
import numpy as np
import seaborn as sns
from matplotlib.lines import Line2D  # 确保导入 Line2D

def ppc_plot(hdi_sum, ax=None, show_plot=True):

    df = hdi_sum.copy()
    
    df["obs_id"] = df.reset_index(drop=True).index

    # 选择一个Seaborn调色板
    palette = sns.color_palette("husl", len(df['subject'].unique()))

    # 创建一个颜色映射字典，深色和浅色
    color_map = {subject: (palette[i], sns.light_palette(palette[i], reverse=True)[3])
                for i, subject in enumerate(df['subject'].unique())}

    # 映射颜色到新列
    df['color_dark'] = df['subject'].map(lambda x: color_map[x][0])
    df['color_light'] = df['subject'].map(lambda x: color_map[x][1])

    # 根据是否落在可信区间内分配颜色
    df["color_dark"] = np.where(
        (df["y"] >= df["hdi_2.5%"]) & (df["y"] <= df["hdi_97.5%"]), 
        df["color_dark"], '#C00000'
    )

    # 设置画布
    if ax is None:
        fig, ax = plt.subplots(figsize=(15, 6))

    # 绘制 94% 的可信区间
    ax.vlines(df["obs_id"], 
                df["hdi_2.5%"], 
                df["hdi_97.5%"], 
                color=df["color_light"], 
                alpha=0.1, 
                label="94% HDI")

    # 各被试散点图数据
    ax.scatter(df["obs_id"], df["y"], 
                color=df["color_dark"],
                alpha=0.8, 
                zorder=2)

    # 绘制后验预测均值
    ax.scatter(df["obs_id"], df["mean"], 
                marker="_", 
                c='black', 
                alpha=0.7, 
                zorder=2, 
                label="Posterior mean")

    # 设置图例
    handles = [
        Line2D([0], [0], marker='o', color='w', markerfacecolor='#C00000', markersize=10, label='Outside HDI'),
        Line2D([0], [0], color='gray', alpha=0.5, label='94% HDI'),
        Line2D([0], [0], marker='_', color='black', alpha=0.7, label='Posterior mean'),
    ]
    ax.legend(handles=handles, loc='upper right', bbox_to_anchor=(1.08, 1))

    # 设置 x 轴刻度
    count_per_subject = df.groupby("subject").size().values
    cumulative_count = count_per_subject.cumsum()
    xtick = cumulative_count - count_per_subject / 2
    ax.set_xticks(xtick, df["subject"].unique())

    # 设置图形标题和标签
    ax.set_title("Posterior Predictive Check (PPC) by Subject", fontsize=16)
    ax.set_xlabel("Subject ID", fontsize=14)
    ax.set_ylabel("Reaction Time (ms)", fontsize=14)

    if show_plot:
        sns.despine()
        plt.show()
    else:
        return ax
      
ppc_plot(hdi_sum=complete_hdi_sum)

# 设置绘图风格
sns.set(style="white")

# 创建子图
fig, axes = plt.subplots(1, 2, figsize=(14, 6))

# 左侧箱线图：显示每个被试的反应时间分布
sns.boxplot(
    data=df_first5,
    x="subject",
    y="RT",
    palette="Set2",
    ax=axes[0]
)

# 右侧核密度估计图：显示每个被试的反应时间密度分布
sns.kdeplot(
    data=df_first5,
    x="RT",
    hue="subject",
    palette="Set2",
    common_norm=False,
    alpha=0.5,
    ax=axes[1]
)

# 显示图形
plt.tight_layout()
sns.despine()
plt.show()

# 建立被试 ID 映射表
subject_mapping = {subj_id: idx for idx, subj_id in enumerate(df_first5["subj_id"].unique())}

# 将被试 ID 转换为索引
mapped_subject_id = df_first5["subj_id"].map(subject_mapping).values

# 定义 pymc 模型坐标
coords = {
    "subject": df_first5["subj_id"].unique(),
    "obs_id": df_first5.index.values
}

with pm.Model(coords=coords) as no_pooled_model:
    
    # 对 RT 进行 log 变换
    log_RTs = pm.MutableData("log_RTs", np.log(df_first5['RT']))

    # 定义被试特定的均值和标准差
    mu = pm.Normal("mu",  mu=7.5, sigma=5, dims="subject")      
    sigma = pm.Exponential("sigma", 1, dims="subject")       

    # 定义观测数据的映射 (obs_id -> subject)
    subject_id = pm.MutableData("subject_id", mapped_subject_id, dims="obs_id")

    # 定义观测值 (obs_id 映射到对应 subject 的 mu 和 sigma)
    y = pm.Normal("y_est", mu=mu[subject_id], sigma=sigma[subject_id],
                  observed=log_RTs, dims="obs_id")

    # MCMC 采样
    no_pooled_trace = pm.sample(1000, return_inferencedata=True)
    
pm.model_to_graphviz(no_pooled_model)

ax = az.plot_trace(
    no_pooled_trace,
    var_names=["mu"],
    filter_vars="like",
    compact=False,
    figsize=(7,12))
plt.tight_layout()

az.summary(no_pooled_trace)

no_ppc = pm.sample_posterior_predictive(no_pooled_trace,
                                        model=no_pooled_model)

no_hdi_sum = ppc_sum(ppc = no_ppc,
                data=df_first5)

no_hdi_sum = inv_log_hdi_sum(no_hdi_sum)

ppc_plot(hdi_sum=no_hdi_sum)

# 建立被试 ID 映射表
subject_mapping = {subj_id: idx for idx, subj_id in enumerate(df_first5["subj_id"].unique())}

# 将被试 ID 转换为索引
mapped_subject_id = df_first5["subj_id"].map(subject_mapping).values

# 定义 pymc 模型坐标
coords = {
    "subject": df_first5["subj_id"].unique(),
    "obs_id": df_first5.index.values
}

with pm.Model(coords=coords) as partial_pooled_model:
    
    # 对 RT 进行 log 变换
    log_RTs = pm.MutableData("log_RTs", np.log(df_first5['RT']))

    # Hyperpriors,定义全局参数
    var_y = pm.Exponential("within_variability", 1)
    var_mu = pm.Exponential("between_variability", 1)
    hyper_mu = pm.Normal("hyper_mu", mu=7.5, sigma=5)

    # 定义被试参数
    mu = pm.Normal("mu", mu=hyper_mu, sigma=var_mu, dims="subject")
    #获得观测值对应的被试映射
    subject_id = pm.MutableData("subject_id", mapped_subject_id, dims="obs_id")
    # 定义 likelihood
    likelihood = pm.Normal("y_est", mu=mu[subject_id], sigma=var_y, observed=log_RTs, dims="obs_id")

    partial_trace = pm.sample(draws=5000,                   # 使用mcmc方法进行采样，draws为采样次数
                                tune=1000,                    # tune为调整采样策略的次数，可以决定这些结果是否要被保留
                                chains=4,                     # 链数
                                discard_tuned_samples= True,  # tune的结果将在采样结束后被丢弃
                                random_seed=84735)
    
az.summary(partial_trace)

ax = az.plot_trace(
    partial_trace,
    var_names=["mu"],
    filter_vars="like",
    compact=False,
    figsize=(7,14))
plt.tight_layout()

partial_ppc = pm.sample_posterior_predictive(partial_trace,
                                            model=partial_pooled_model)

partial_hdi_sum = ppc_sum(ppc=partial_ppc,
                  data=df_first5)     

partial_hdi_sum = inv_log_hdi_sum(partial_hdi_sum)

ppc_plot(hdi_sum=partial_hdi_sum)

fig, axes = plt.subplots(3,1, figsize=(7,9))

ax = axes[0]
ax = ppc_plot(hdi_sum=complete_hdi_sum, ax = ax, show_plot=False)
ax.set_title("Complete pooling model")

ax = axes[1]
ax = ppc_plot(hdi_sum=partial_hdi_sum, ax = ax, show_plot=False)
ax.set_title("Patial pooling model")

ax = axes[2]
ax = ppc_plot(hdi_sum=no_hdi_sum, ax = ax, show_plot=False)
ax.set_title("No pooling model")

sns.despine()
plt.tight_layout()
plt.show()

# 提取三个模型参数后验，筛选中其中含有mu参数的部分
partial_stats = az.summary(partial_trace, var_names=["mu"], filter_vars = "like", kind="stats")
no_stats = az.summary(no_pooled_trace, var_names=["mu"], filter_vars = "like", kind="stats")
complete_stats = az.summary(complete_trace, var_names=["mu"], filter_vars = "like", kind="stats")

partial_stats = inv_log_hdi_sum(partial_stats)
no_stats = inv_log_hdi_sum(no_stats)
complete_stats = inv_log_hdi_sum(complete_stats)

# 设置一列，表明参数来源
complete_stats['source'] = 'Complete pool'
no_stats['source'] = 'No pool'
partial_stats['source'] = 'Partial pool'

# 合并三个模型的结果
df_compare = pd.concat([complete_stats.reset_index(),
                        no_stats.reset_index(),
                        partial_stats.reset_index()])

#设置索引，表明参数来源
df_compare.set_index(['source', df_compare.index], inplace=True)
df_compare

# 设置三个绘制坐标轴
fig, (ax1, ax2, ax3) = plt.subplots(1,3, figsize=(12,4), sharex=True)

# 绘制三个模型参数后验
az.plot_forest(partial_trace, var_names=["mu"], filter_vars = "like", combined=True, ax=ax1)
ax1.set_title("Patial Pooling")
az.plot_forest(no_pooled_trace, var_names=["mu"], filter_vars = "like", combined=True, ax=ax2)
ax2.set_title("No Pooling")
az.plot_forest(complete_trace, var_names=["mu"], filter_vars = "like", combined=True, ax=ax3)
ax3.set_title("Complete Pooling")

plt.tight_layout()
plt.show()

#设置画布大小1
plt.figure(figsize=(9,4))

#绘制完全池化模型下每个点对应的后验预测均值
plt.scatter(complete_hdi_sum["obs_id"],
            complete_hdi_sum["mean"],
            alpha=0.15,
            s=80,
            label="Complete pooling")

#绘制非池化模型下每个点对应的后验预测均值
plt.scatter(no_hdi_sum["obs_id"],
            no_hdi_sum["mean"],
            alpha=0.15,
            s=80,
            label="No pooling")

#绘制部分池化模型下每个点对应的后验预测均值
plt.scatter(partial_hdi_sum["obs_id"],
            partial_hdi_sum["mean"],
            alpha=0.15,
            s=80,
            label="Partial pooling")

#设置图例
plt.legend()

#计算每个被试的数据量，并根据数据量大小在x轴上进行刻度标识
count_per_subject = df_first5.groupby("subject").size().values
cumulative_count = count_per_subject.cumsum()
xtick = cumulative_count - count_per_subject / 2
plt.xticks(xtick,df_first5["subject"].unique())

#设置标题
plt.title("Posterior mean of observed data",
          fontsize=15)
sns.despine()

def calculate_mae(hdi_sum, obs = "y", pred = "mean"):
    """
    计算后验预测均值和 MAE (Median Absolute Error)。
    """

    # 提取后验预测值
    observed_data = hdi_sum[obs]
    posterior_predictive = hdi_sum[pred]
    
    # 计算 MAE（绝对误差的中位数）
    mae = np.median(np.abs(observed_data - posterior_predictive))
    
    return mae
  
pd.DataFrame({
    "Complate Pooling Model": [calculate_mae(complete_hdi_sum)],
    "No Pooling Model": [calculate_mae(no_hdi_sum)],
    "Partilar Pooling Model": [calculate_mae(partial_hdi_sum)],
})

# 选择被试为"31727"的数据
new_data = df_raw[(df_raw.subject == 31727) & (df_raw['percentCoherence'] == 5)]

# 建立索引 'subj_id' 和 'obs_id'
new_data['subj_id'] = new_data['subject']
new_data['obs_id'] = new_data.groupby('subject').cumcount() + 1

new_data.head()

# 建立被试 ID 映射表
subject_mapping = {subj_id: idx for idx, subj_id in enumerate(new_data["subj_id"].unique())}

# 将被试 ID 转换为索引
mapped_subject_id = new_data["subj_id"].map(subject_mapping).values

# 定义 pymc 模型坐标
new_coords = {
    "subject": new_data["subj_id"].unique(),
    "obs_id": new_data.index.values
}

with pm.Model(coords=new_coords) as partial_pooled_pred:
    
    # 对 RT 进行 log 变换
    log_RTs = pm.MutableData("log_RTs", np.log(new_data['RT']))

    # 定义新预测数据对应的新参数
    hyper_mu = pm.Normal("new_hyper_mu", mu=7.105, sigma=0.340)
    var_y = pm.Exponential("new_within_variability", 5)
    var_mu = pm.Exponential("new_between_variability", 5)

    # 定义被试参数
    new_mu = pm.Normal("new_mu", mu=hyper_mu, sigma=var_mu, dims="subject")
    #获得观测值对应的被试映射
    subject_id = pm.MutableData("subject_id", mapped_subject_id, dims="obs_id")
    
    # 定义 likelihood
    likelihood = pm.Normal("y_est", mu=new_mu[subject_id], sigma=var_y, observed=log_RTs, dims="obs_id")
    
    # 进行后验预测估计，注意使用的是上一个模型的后验参数估计，partial_trace
    pred_trace = pm.sample_posterior_predictive(partial_trace,
                                                var_names=["y_est"],
                                                predictions=True,
                                                random_seed=84735)  

pred_hdi_sum = ppc_sum(ppc=pred_trace.predictions,data=new_data)
pred_hdi_sum = inv_log_hdi_sum(pred_hdi_sum)
ppc_plot(pred_hdi_sum)

#======================================================
# 练习
#======================================================                                        
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

df_raw[["Site","scontrol"]]

sns.boxplot(data=df_raw,
            x="Site",
            y="scontrol")

plt.xticks(rotation=90) 
sns.despine()
plt.show()

# 选取5个被试
first5_site = ['Southampton','METU','Kassel','Tsinghua','Oslo']
df_first5 = df_raw.query("Site in @first5_site")

#为被试生成索引，为被试生成索引
df_first5["site_idx"] = pd.factorize(df_first5.Site)[0]
df_first5["obs_id"] = range(len(df_first5))

#设置索引，方便之后调用数据
df_first5.set_index(['Site','obs_id'],inplace=True,drop=False)
df_first5.head(10)

##--------------------------------------------------
#      提示：对...中的内容进行修改
#---------------------------------------------------

with pm.Model() as complete_pooled_model:

    #定义beta_0
    mu = pm.Normal("mu", mu=..., sigma=...)  
    #定义sigma                  
    sigma = pm.Exponential(...)       

    #定义似然：预测值y符合N(mu, sigma)分布；传入实际数据y 自我控制水平 df_first5.scontrol

    likelihood = pm.Normal(...)   

    # 进行采样，默认为 chains=4, samples=1000,burn=1000
    complete_trace = pm.sample(random_seed=84735)
    
az.plot_trace(complete_trace,
              compact=False,
              figsize=(15,6))

az.summary(complete_trace)

##--------------------------------------------------
#      提示：对...中的内容进行修改
#---------------------------------------------------

# 进行后验预测
complete_ppc = pm.sample_posterior_predictive(...)

# 定义函数，计算 95%hdi
def ppc_sum(ppc, data):
    
    hdi_sum = az.summary(ppc, hdi_prob=0.95)
    hdi_sum["obs_id"] = data["obs_id"].values
    hdi_sum["y"] = data["scontrol"].values
    hdi_sum["site"] = data["Site"].values

    return hdi_sum

# 计算后验预测的 95%hdi
complete_hdi_sum = ppc_sum(ppc = complete_ppc, data=df_first5)
complete_hdi_sum

# 定义函数绘制超出 95%hdi 的点
from matplotlib.lines import Line2D

def ppc_plot(hdi_sum):
    fig, ax =  plt.subplots(figsize=(15,6))

    #生成颜色条件，根据被试生成不同的颜色（可信区间）
    unique_sites = hdi_sum["site"].unique()
    conditions=[]
    colors=[]
    for i, site in enumerate(unique_sites):
        condition = hdi_sum["site"] == site
        conditions.append(condition)
        color = f"C{i}"
        colors.append(color)
        
    hdi_colors = np.select(conditions,colors)
    #绘制94%的可信区间
    HDI = ax.vlines(hdi_sum["obs_id"], 
            hdi_sum["hdi_2.5%"], hdi_sum["hdi_97.5%"], 
            color=hdi_colors, 
            alpha=0.5,
            label="94% HDI")
    #绘制后验预测均值
    pos_mean = ax.scatter(hdi_sum["obs_id"], hdi_sum["mean"],
            marker="_",
            c = 'black',
            alpha=0.2,
            zorder = 2,
            label="Posterior mean")
    #根据是否落在可信区间内选择不同的颜色
    colors = np.where((hdi_sum["y"] >= hdi_sum["hdi_2.5%"]) & (hdi_sum["y"] <= hdi_sum["hdi_97.5%"]), 
                    '#2F5597', '#C00000')
    #绘制真实值
    ax.scatter(hdi_sum["obs_id"], hdi_sum["y"],
            c = colors,
            alpha=0.7,
            zorder = 2)
    # 设置图例的颜色、形状、名称
    legend_color = ['#2F5597', '#C00000']
    handles = [plt.Line2D([0], [0], 
                        marker='o', 
                        color='w', 
                        markerfacecolor=color, markersize=10) for color in legend_color]
    handles += [HDI]
    handles += [pos_mean]
    labels = ['Within HDI', 'Outside HDI','94%HDI','Posterior mean']

    plt.legend(handles=handles, 
               labels=labels,
               loc='upper right',
               bbox_to_anchor=(1.08, 1))
    # 设置x轴的刻度，根据每个类别的数量确定刻度位置
    count_per_site = hdi_sum.groupby("site").size().values
    cumulative_count = count_per_site.cumsum()
    xtick = cumulative_count - count_per_site / 2
    plt.xticks(xtick, hdi_sum["site"].unique())

    sns.despine()
    
##--------------------------------------------------
#      提示：对...中的内容进行修改
#---------------------------------------------------

ppc_plot(...)

##--------------------------------------------------
#      提示：对...中的内容进行修改
#---------------------------------------------------

coords = {"site": df_first5["Site"].unique(),
          "obs_id": df_first5.obs_id}

with pm.Model(coords=coords) as no_pooled_model:

    #定义mu，指定dims="site"，生成不同的mu 
    mu = pm.Normal(...)                  
    #定义sigma，指定dims="site"，生成不同的sigma
    sigma = pm.Exponential(...)            
    #获得观测值对应的被试映射
    site = pm.MutableData(...) 
    # 定义 likelihood
    likelihood = pm.Normal(...)

    no_pooled_trace = pm.sample(random_seed=84735)
    
##--------------------------------------------------
#      提示：对...中的内容进行修改
#---------------------------------------------------

ax = az.plot_trace(
    no_pooled_trace,
    compact=False,
    figsize=(20,50))

az.summary(...)

##--------------------------------------------------
#      提示：对...中的内容进行修改
#---------------------------------------------------

no_ppc = pm.sample_posterior_predictive(...)

no_hdi_sum = ppc_sum(ppc = no_ppc,
                data=df_first5)

##--------------------------------------------------
#      提示：对...中的内容进行修改
#---------------------------------------------------

ppc_plot(...)

##--------------------------------------------------
#      提示：对...中的内容进行修改
#---------------------------------------------------


with pm.Model(coords=coords) as partial_pooled_model:
    # Hyperpriors,定义全局参数
    var_y = pm.Exponential(...)
    var_mu = pm.Exponential(...)
    hyper_mu = pm.Normal(...)
    # 定义被试参数
    mu = pm.Normal(...)
    #获得观测值对应的被试映射
    site = pm.MutableData(...)
    # 定义 likelihood
    likelihood = pm.Normal(...)

    partial_trace = pm.sample(draws=5000,                   # 使用mcmc方法进行采样，draws为采样次数
                                tune=1000,                    # tune为调整采样策略的次数，可以决定这些结果是否要被保留
                                chains=4,                     # 链数
                                discard_tuned_samples= True,  # tune的结果将在采样结束后被丢弃
                                random_seed=84735)
    
##--------------------------------------------------
#      提示：对...中的内容进行修改
#---------------------------------------------------

az.summary(...)

##--------------------------------------------------
#      提示：对...中的内容进行修改
#---------------------------------------------------

with partial_pooled_model:
    az.plot_trace(partial_trace,
                  compact=False,
                  figsize=(20,40))
    
##--------------------------------------------------
#      提示：对...中的内容进行修改
#---------------------------------------------------

# 提取组间和组内变异
partial_para_sum = az.summary(partial_trace)
between_sd = partial_para_sum.loc[...]
within_sd = partial_para_sum.loc[...]

# 计算变异占比
var = between_sd**2 + within_sd**2
print("被组间方差所解释的部分：", between_sd**2/var)
print("被组内方差所解释的部分：", within_sd**2/var)
print("组内相关：",between_sd**2/var)

##--------------------------------------------------
#      提示：对...中的内容进行修改
#---------------------------------------------------

partial_ppc = pm.sample_posterior_predictive(...,
                                            model=...)
partial_hdi_sum = ppc_sum(ppc=...,
                  data=...)     

##--------------------------------------------------
#      提示：对...中的内容进行修改
#---------------------------------------------------

ppc_plot(hdi_sum=...)

##--------------------------------------------------
#      提示：对...中的内容进行修改
#---------------------------------------------------

az.summary(...)