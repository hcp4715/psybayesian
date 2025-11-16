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

# 忽略不必要的警告
import warnings
warnings.filterwarnings("ignore")

# 使用 pandas 导入示例数据
try:
  df = pd.read_csv("/home/mw/input/bayes3797/evans2020JExpPsycholLearn_exp1_full_data.csv") 
except:
  df = pd.read_csv('data/evans2020JExpPsycholLearn_exp1_full_data.csv')

# 筛选编号为 31727 的数据，并且筛选出两个不同的 percentCoherence
df_clean = df[(df['subject'] == 31727) & (df['percentCoherence'].isin([10, 40]))]
df_clean  = df_clean[["subject", "percentCoherence", "correct"]]

df_clean.groupby("percentCoherence").correct.mean()

# 因变量分布
ax = df_clean.groupby("percentCoherence").correct.mean().plot.bar()

ax.set_ylabel("accuracy")
sns.despine()
plt.show()

# 数据准备
Treatment_Coding,_ = df_clean['percentCoherence'].factorize() # 适用 treatment 编码
y = df_clean['correct'].values  # 目标变量

# 模型构建
with pm.Model() as log_model1:

    # 添加数据，方便后续绘图
    pm.MutableData("percentCoherence", df_clean['percentCoherence'])

    # 设置先验
    # 通常我们会为截距和系数设置正态分布的先验
    intercept = pm.Normal('beta_0', mu=0, sigma=10)
    coefficient = pm.Normal('beta_1', mu=0, sigma=10)
    
    # 线性预测
    linear_predictor = intercept + coefficient * Treatment_Coding
    
    # 似然函数
    # 使用逻辑函数将线性预测转换为概率
    # 方法一：自行进行 logit link 转换
    pi = pm.Deterministic('pi', pm.math.invlogit(linear_predictor))
    likelihood = pm.Bernoulli('likelihood', p=pi, observed=y)
    # 方法二：直接使用 logit_p 进行转换
    # likelihood = pm.Bernoulli('likelihood', logit_p=linear_predictor, observed=y)
    
    def inv_logit(x):
    return np.exp(x) / (1 + np.exp(x))
  
  log1_prior = pm.sample_prior_predictive(samples=50, 
                                          model=log_model1,
                                          random_seed=84735)
                                          
#对于一次抽样，可以绘制出一条曲线，结合循环绘制出50条曲线
for i in range(log1_prior.prior.dims["draw"]):
    sns.lineplot(x = log1_prior.constant_data["percentCoherence"],
                y = log1_prior.prior["pi"].stack(sample=("chain", "draw"))[:,i], c="grey" )

#设置x、y轴标题和总标题    
plt.xlabel("percentCoherence",
           fontsize=12)
plt.ylabel("probability of correct",
           fontsize=12)
plt.suptitle("Relationships between percentCoherence and the probability of correct",
           fontsize=14)
sns.despine()
plt.show()

#===========================
#     注意！！！以下代码可能需要运行35s~1分钟左右
#===========================
with log_model1:
    # 模型编译和采样
    log_model1_trace = pm.sample(draws=5000,                 
                                tune=1000,                  
                                chains=4,                     
                                discard_tuned_samples=True, 
                                random_seed=84735)

az.plot_trace(log_model1_trace,
              var_names=["beta_0","beta_1"],
              figsize=(7, 6),
              compact=False)
plt.show()

fitted_parameters = az.summary(log_model1_trace, var_names=["beta_0","beta_1"])

def inv_logit(log_odds):
    return np.exp(log_odds) / (1 + np.exp(log_odds))

p_coh10 = inv_logit( fitted_parameters.loc["beta_0", "mean"])
p_coh40 = inv_logit( 
    fitted_parameters.loc["beta_1", "mean"] + \
        fitted_parameters.loc["beta_1", "mean"]
)

print(f"p(coherence=10) = {p_coh10:.3f}", f"p(coherence=40) = {p_coh40:.3f}")

# 通过 inv_logit 将 beta 参数进行转换
az.plot_posterior(log_model1_trace, var_names=["beta_0"], transform = inv_logit)
plt.show()

#对于一次抽样，可以绘制出一条曲线，结合循环绘制出50条曲线
ys = log_model1_trace.posterior["pi"].stack(sample=("chain", "draw"))
for i in range(100):
    sns.lineplot(x = log_model1_trace.constant_data["percentCoherence"],
            y = ys[:,i], 
            c="grey",
            alpha=0.4)
    
#设置x、y轴标题和总标题    
plt.xlabel("percentCoherence",
           fontsize=12)
plt.ylabel("probability of correct",
           fontsize=12)
plt.suptitle("100 posterior plausible models",
           fontsize=14)
sns.despine()
plt.show()

odds = log_model1_trace.posterior["beta_0"]+ log_model1_trace.posterior["beta_1"] * 1
pi = inv_logit(odds)
Y_hat = np.random.binomial(n=1, p=pi)[0]

# 统计其中0和1的个数，并除以总数，得到0和1对应的比例值
y_pred_freq = np.bincount(Y_hat)/len(Y_hat)

# 绘制柱状图
bars = plt.bar([0, 1], y_pred_freq, color="#70AD47")

# 用于在柱状图上标明比例值
for bar, freq in zip(bars, y_pred_freq):
    plt.text(bar.get_x() + bar.get_width() / 2, bar.get_height(), f"{freq:.2f}", ha='center', va='bottom')

#对刻度、标题、坐标轴标题进行设置
plt.xticks([0, 1])
plt.suptitle("Out-of-sample prediction(X=1)")
plt.xlabel("correct")
plt.ylabel("proportion")
sns.despine()

ys = log_model1_trace.posterior["pi"].stack(sample=("chain", "draw"))
df_clean["pi"] = ys.mean(dim="sample").values

predictions = []
for i in df_clean["pi"]:
    prediction = np.random.binomial(n=1, p=i)
    predictions.append(prediction)

df_clean["prediction"] = predictions
#df_clean

def calculate_contingency_table(df, y="correct", yhat="prediction"):
    
    # 计算各种情况的数量
    TN = ((df[y] == 0) & (df[yhat] == 0)).sum()  # 真阴性
    FP = ((df[y] == 0) & (df[yhat] == 1)).sum()  # 假阳性
    FN = ((df[y] == 1) & (df[yhat] == 0)).sum()  # 假阴性
    TP = ((df[y] == 1) & (df[yhat] == 1)).sum()  # 真阳性
    
    # 创建一个DataFrame来表示列联表
    contingency_df = pd.DataFrame({
        '$\\hat{Y} = 0$': [TN, FN],
        '$\\hat{Y} = 1$': [FP, TP]
    }, index=['$Y=0$', '$Y=1$'])
    
    return (TN, FP, TN, FN), contingency_df

# 计算两个 percentCoherence 值下的列联表
(true_positive, false_positive, true_negative, false_negative), contingency_table = calculate_contingency_table(df_clean)

#contingency_table

# 定义计算指标函数
def calculate_metrics(TP, FP, TN, FN):
    # 计算准确性
    accuracy = (TP + TN) / (TP + TN + FP + FN)

    # 计算敏感性
    sensitivity = TP / (TP + FN) if (TP + FN) != 0 else 0

    # 计算特异性
    specificity = TN / (TN + FP) if (TN + FP) != 0 else 0

    return accuracy, sensitivity, specificity

# 计算指标
accuracy, sensitivity, specificity = calculate_metrics(true_positive, false_positive, true_negative, false_negative)

# 打印结果
print(f"True Positive: {true_positive}")
print(f"False Positive: {false_positive}")
print(f"True Negative: {true_negative}")
print(f"False Negative: {false_negative}")
print(f"准确性: {accuracy}")
print(f"敏感性: {sensitivity}")
print(f"特异性: {specificity}")

def inv_logit(log_odds):
    return np.exp(log_odds) / (1 + np.exp(log_odds))
  
import bambi as bmb

bambi_logit = bmb.Model("correct ~ C(percentCoherence)", df_clean, family="bernoulli")
#bambi_logit

model_fitted = bambi_logit.fit(random_seed=84735)

fitted_parameters = az.summary(model_fitted)

p_coh10 = inv_logit( fitted_parameters.loc["Intercept", "mean"])
p_coh40 = inv_logit( 
    fitted_parameters.loc["Intercept", "mean"] + \
        fitted_parameters.loc["C(percentCoherence)[40]", "mean"]
)

print(f"p(coherence=10) = {p_coh10:.3f}", f"p(coherence=40) = {p_coh40:.3f}")

posterior_predictive = bambi_logit.predict(model_fitted, kind="pps")

az.plot_ppc(model_fitted, num_pp_samples=50)
sns.despine()

# 通过 pd.read_csv 加载数据 Data_Sum_HPP_Multi_Site_Share.csv
try:
  df_raw = pd.read_csv('/home/mw/input/bayes3797/Data_Sum_HPP_Multi_Site_Share.csv')
except:
  df_raw = pd.read_csv('data/Data_Sum_HPP_Multi_Site_Share.csv')

# 选取清华站点的数据
df = df_raw[df_raw["Site"] == "Tsinghua"]

# 选取本节课涉及的变量
df = df[["romantic", "avoidance_r", "sex"]]

#重新编码，编码后的数据：1 = "yes"; 2 = "no"
df["romantic"] =  np.where(df['romantic'] == 2, 0, 1)

#设置索引
df["index"] = range(len(df))
df = df.set_index("index")
warnings.filterwarnings("ignore")

# 对数据进行可视化
# 绘制散点图
sns.scatterplot(data=df,
                x="avoidance_r",
                y="romantic",
                alpha=0.6)
# 设置x轴标题
plt.xlabel("avoidance")
# 设置y轴刻度
plt.yticks([0,1],['no','yes'])
sns.despine()

#===========================
#     提示：参照之前的代码，对...中的内容进行修改
#===========================

with pm.Model() as log_model2:

    # 添加数据，方便后续绘图
    x = pm.MutableData("avoidance_r", df['avoidance_r'])
    y = pm.MutableData("romantic", df['romantic'])

    # 设置先验
    # 通常我们会为截距和系数设置正态分布的先验
    intercept = pm.Normal('...', mu=..., sigma=...)
    coefficient = pm.Normal('...', mu=..., sigma=...)
    
    # 线性预测
    linear_predictor = ...
    
    # 似然函数
    # 使用逻辑函数将线性预测转换为概率
    # 方法一：自行进行 logit link 转换
    pi = pm.Deterministic('...', pm.math.invlogit(...))
    likelihood = pm.Bernoulli('...', p=..., observed=...)
    # 方法二：直接使用 logit_p 进行转换
    # likelihood = pm.Bernoulli('likelihood', logit_p=linear_predictor, observed=y)
    
    #===========================
#     注意！！！以下代码可能需要运行1-2分钟左右
#===========================
with log_model2:
    # MCMC 近似后验分布
    log_model2_trace = pm.sample(
                                draws=5000,                   # 使用mcmc方法进行采样，draws为采样次数
                                tune=1000,                    # tune为调整采样策略的次数，可以决定这些结果是否要被保留
                                chains=4,                     # 链数
                                discard_tuned_samples= True,  # tune的结果将在采样结束后被丢弃
                                random_seed=84735)
                                
az.plot_trace(...,
              var_names=["...","..."],
              figsize=(15,8),
              compact=False)
plt.show()

##---------------------------------------------------------------------------
#      无需修改，直接运行即可
#---------------------------------------------------------------------------

#画出每个自变量对应的恋爱概率94%hdi值
az.plot_hdi(
    df.avoidance_r,
    log_model2_trace.posterior.pi,
    hdi_prob=0.95,
    fill_kwargs={"alpha": 0.25, "linewidth": 0},
    color="C1"
)
#得到每个自变量对应的恋爱概率均值，并使用sns.lineplot连成一条光滑的曲线
post_mean = log_model2_trace.posterior.pi.mean(("chain", "draw"))
sns.lineplot(x = df.avoidance_r, 
             y= post_mean, 
             label="posterior mean", 
             color="C1")
#绘制真实数据散点图
sns.scatterplot(x = df.avoidance_r, 
                y= df.romantic,label="observed data", 
                color='#C00000', 
                alpha=0.5)
#设置图例位置
plt.legend(loc="upper right",
           bbox_to_anchor=(1.5, 1),
           fontsize=12)
sns.despine()

#===========================
#     提示：参照之前的代码，对...中的内容进行修改
#===========================

odds = ...
pi = ...
Y_hat = ...

# 统计其中0和1的个数，并除以总数，得到0和1对应的比例值
y_pred_freq = np.bincount(Y_hat)/len(Y_hat)

#绘制柱状图
bars = plt.bar([0, 1], y_pred_freq, color="#70AD47")

#用于在柱状图上标明比例值
for bar, freq in zip(bars, y_pred_freq):
    plt.text(bar.get_x() + bar.get_width() / 2, bar.get_height(), f"{freq:.2f}", ha='center', va='bottom')

#对刻度、标题、坐标轴标题进行设置
plt.xticks([0, 1])
plt.suptitle("Out-of-sample prediction(X=1)")
plt.xlabel("romantic")
plt.ylabel("proportion")
sns.despine()

##---------------------------------------------------------------------------
#      提示：参照之前的代码与先验定义，对...中的内容进行修改
#---------------------------------------------------------------------------

# 定义计算指标函数
def calculate_metrics(TP, FP, TN, FN):
    # 计算准确性
    accuracy = (TP + TN) / (TP + TN + FP + FN)

    # 计算敏感性
    sensitivity = TP / (TP + FN) if (TP + FN) != 0 else 0

    # 计算特异性
    specificity = TN / (TN + FP) if (TN + FP) != 0 else 0

    return accuracy, sensitivity, specificity

# 计算指标
accuracy, sensitivity, specificity = calculate_metrics(...)

# 打印结果
print(f"True Positive: {true_positive}")
print(f"False Positive: {false_positive}")
print(f"True Negative: {true_negative}")
print(f"False Negative: {false_negative}")
print(f"准确性: {accuracy}")
print(f"敏感性: {sensitivity}")
print(f"特异性: {specificity}")
