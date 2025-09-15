# 导入必要的库
# 显示ModuleNotFoundError时在控制台运行 pip install xxx；例如pip install seaborn
import arviz as az
import scipy.stats as st
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import preliz as pz

from scipy.stats import binom

# 为 preliz 绘图设置图形样式
pz.style.library["preliz-doc"]["figure.dpi"] = 100
pz.style.library["preliz-doc"]["figure.figsize"] = (10, 4)
pz.style.use("preliz-doc")

# 使用 pandas 导入示例数据（本地运行时需要修改数据路径）
try:
  data = pd.read_csv("/home/mw/input/bayes3797/evans2020JExpPsycholLearn_exp1_clean_data.csv") #平台路径
except:
  data = pd.read_csv('/Users/liumingyu/Desktop/1/PyBayesian/data/evans2020JExpPsycholLearn_exp1_clean_data.csv') #本地路径

# 选择特定模式的数据进行演示：
# 筛选 correct 为 1 的数据，并随机选取30个
data_correct_1 = data[data['correct'] == 1].sample(n=30, random_state=42)

# 筛选 correct 为 0 的数据，并随机选取20个
data_correct_0 = data[data['correct'] == 0].sample(n=20, random_state=42)

# 合并两部分数据，形成新的数据集
df = pd.concat([data_correct_1, data_correct_0], ignore_index=True)

# 只显示特定列
df[['subject', 'numberofDots', 'percentCoherence', 'correct', 'RT']].head()

# 创建离散先验分布数据
prior_disc = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0]         # 离散先验分布的取值
prior_dis_prob = [0, 0, 0, 0, 0, 0.10, 0.80, 0.10, 0, 0]  # 对应的概率
prior_disc_data = pd.DataFrame({'ACC': prior_disc, 'f(ACC)': prior_dis_prob})

# 将'ACC'列的数据类型转换为字符串，以便在图表中正确显示
prior_disc_data['ACC'] = prior_disc_data['ACC'].astype('str')

# 创建一个单独的图表
plt.figure(figsize=(8, 5))

# 绘制离散先验分布的条形图
sns.barplot(data=prior_disc_data, x='ACC', y='f(ACC)', palette="deep")

# 设置图表的标题
plt.title("Discrete Prior")

# 移除图的上边框和右边框
sns.despine()

# 显示图表
plt.show()

# 创建一个3x3的网格子图，每个子图的尺寸为10x10
fig, axs = plt.subplots(3, 3, figsize=(10, 10))

# 绘制 Beta(1, 5) 分布的PDF，并显示置信区间
pz.Beta(1, 5).plot_pdf(pointinterval=True,ax=axs[0, 0], legend="title")
pz.Beta(1, 2).plot_pdf(pointinterval=True,ax=axs[0, 1], legend="title")
pz.Beta(3, 7).plot_pdf(pointinterval=True,ax=axs[0, 2], legend="title")
pz.Beta(1, 1).plot_pdf(pointinterval=True,ax=axs[1, 0], legend="title")
pz.Beta(5, 5).plot_pdf(pointinterval=True,ax=axs[1, 1], legend="title")
pz.Beta(20, 20).plot_pdf(pointinterval=True,ax=axs[1, 2], legend="title")
pz.Beta(7, 3).plot_pdf(pointinterval=True,ax=axs[2, 0], legend="title")
pz.Beta(2, 1).plot_pdf(pointinterval=True,ax=axs[2, 1], legend="title")
pz.Beta(5, 1).plot_pdf(pointinterval=True,ax=axs[2, 2], legend="title")

# 自动调整子图之间的间距，以防止标签重叠
plt.tight_layout()

# 显示绘制的图形
plt.show()

# 创建离散先验分布数据
prior_disc = [0.1, 0.2, 0.3,0.4,0.5,0.6, 0.7, 0.8,0.9, 1.0]         # 离散先验分布的取值
prior_dis_prob = [0, 0, 0, 0, 0, 0.10, 0.80, 0.10, 0, 0]  # 对应的概率
prior_disc_data = pd.DataFrame({'ACC': prior_disc, 'f(ACC)': prior_dis_prob})
# 将'ACC'列的数据类型转换为字符串，以便在图表中正确显示
prior_disc_data['ACC'] = prior_disc_data['ACC'].astype('str')

# 创建一个包含两个子图的图表
f, (ax1, ax2) = plt.subplots(1, 2, figsize=(15, 4))

# 在第一个子图中绘制离散先验分布的条形图
sns.barplot(data=prior_disc_data, x='ACC', y='f(ACC)', palette="deep", ax=ax1)
# 设置第一个子图的标题
ax1.set_title("discrete prior")

# 在第二个子图中绘制连续先验分布的线图
pz.Beta(70, 30).plot_pdf(pointinterval=True,ax=ax2, legend="title") # 在这里示范了一个 Beta 分布的参数，你可以根据需要修改这些参数
# # 设置第二个子图的标题
ax2.set_title("continuous prior\n(Beta(alpha=70,beta=30))")

# 设置第二个子图的 x 轴范围为 0 到 1
ax2.set_xlim(0, 1)

# # # 移除图的上边框和右边框
sns.despine()
# 创建一个 Binomial 分布，参数为 n=50（试验次数），p=0.1（正确率概率）
binom = pz.Binomial(n=50, p=0.1)

# 绘制该分布的概率密度函数 (PDF)
binom.plot_pdf()

# 移除图的上边框和右边框
sns.despine()

# 显示图表
plt.show()
# 导入必要的库
import numpy as np
import pandas as pd
from scipy.stats import binom
import seaborn as sns
import matplotlib.pyplot as plt

# 设置二项分布的参数
n = 50  # 总试验次数
p_values = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9]  # 不同的 p 值列表
k = np.arange(0, 51)                                      # 创建一个包含从0到50的整数的数组

# 创建一个包含 'Y' 列的 DataFrame
dist_all_pi = pd.DataFrame({'Y': k})

# 计算每个 'Y' 对应的概率，并将结果存储在相应列中
for p in p_values:
    column_name = f'{p}'
    dist_all_pi[column_name] = dist_all_pi['Y'].apply(lambda x: binom.pmf(x, n, p))

# 使用 stack() 和 reset_index() 转换数据为长格式
melted_data = dist_all_pi.set_index('Y').stack().reset_index()
melted_data.columns = ['Y', 'p', 'prob']

# 创建一个 FacetGrid 对象，用于绘制子图
plot_all_pi = sns.FacetGrid(melted_data, col='p', col_wrap=3)

# 使用柱状图绘制概率分布
plot_all_pi.map(sns.barplot, 'Y', 'prob', color="grey", order=None)

# 设置 x 和 y 轴的刻度和范围
plot_all_pi.set(xticks=[0, 10, 20, 30, 40, 50],
                yticks=[0.00, 0.05, 0.10, 0.15],
                ylim=(0, 0.20))

# 设置 y 轴标签
plot_all_pi.set_ylabels("$f(y|ACC)$")

# 设置子图的标题模板
plot_all_pi.set_titles(col_template="Bin(50,{col_name})")

# 显示x=30的点
# for ax in g.axes.flat:
#     ax.scatter(x=30, y=0, color='red', marker='o', s=60)
# g.show()

import seaborn as sns
from scipy.special import comb

# 定义似然函数
def likelihood(ACC, Y=30, N=50):
    return comb(N, Y) * (ACC**Y) * ((1-ACC)**(N-Y))

# 定义ACC范围在[0, 1]之间
ACC_values = np.linspace(0, 1, 1000)

# 计算每个ACC对应的似然值
likelihood_values = likelihood(ACC_values)

# 设置Seaborn的绘图样式
sns.despine() # 移除图的上边框和右边框

# 绘制似然函数，使用Seaborn的绘图功能
sns.lineplot(x=ACC_values, y=likelihood_values, color="grey", label="Likelihood L(ACC | Y=30)")

# 设置图表标题和标签
plt.xlabel("ACC")
plt.ylabel("Likelihood")

# 在ACC=0.6处画出一条虚线
plt.axvline(x=0.6, color='red', linestyle='--', label="ACC=0.6 (Max)")

# 显示图例
plt.legend()
# 展示图表
plt.show()

# 导入统计建模工具包 scipy.stats 为 st
import scipy.stats as st

# 设置 x 轴范围 [0,1]
x = np.linspace(0,1,10000)
# 设置 Beta 分布参数
a,b = 70,30
# 形成先验分布
prior = st.beta.pdf(x,a,b)/np.sum(st.beta.pdf(x,a,b))

# 形成似然
k = 30     # k 代表正确率为1的次数
n = 50     # n 代表总次数
likelihood = st.binom.pmf(k,n,x)

# 计算后验
unnorm_posterior = prior * likelihood                  # 计算分子
posterior = unnorm_posterior/np.sum(unnorm_posterior)  # 结合分母进行计算
likelihood = likelihood /np.sum(likelihood)            # 为了方便可视化，对似然进行类似后验的归一化操作

# 绘图
plt.plot(x,posterior, color="#009e74", alpha=0.5, label="posterior")
plt.plot(x,likelihood, color="#0071b2", alpha=0.5, label="likelihood")
plt.plot(x,prior, color="#f0e442", alpha=0.5, label="prior")
plt.legend()
plt.xlabel("ACC")
plt.fill_between(x, prior, color="#f0e442", alpha=0.5)
plt.fill_between(x, likelihood, color="#0071b2", alpha=0.5)
plt.fill_between(x, posterior, color="#009e74", alpha=0.5)
sns.despine()
# 导入数据加载和处理包：pandas
import pandas as pd
# 导入数字和向量处理包：numpy
import numpy as np
# 导入基本绘图工具：matplotlib
import matplotlib.pyplot as plt

# 设置随机种子，以便后续可以重复结果
np.random.seed(84735)

# 模拟 10000 次数据
king_sim = pd.DataFrame({'ACC': np.random.beta(45, 55, size=10000)})  # 从Beta(45,55)先验中模拟10,000个ACC值
king_sim['y'] = np.random.binomial(n=50, p=king_sim['ACC'])       # 从每个ACC值中模拟Bin(50,ACC)的潜在正确判断次数Y

# 显示部分数据
king_sim.head()
# 绘制散点图：正确次数 (Y!=30)部分，用黑色表示
plt.scatter(king_sim['ACC'][king_sim['y']!=30],
            king_sim['y'][king_sim['y']!=30],
            c='black', s = 3,
            label='FALSE')
# 绘制散点图：正确次数 (Y=30)部分，用蓝色表示
plt.scatter(king_sim['ACC'][king_sim['y']==30],
            king_sim['y'][king_sim['y']==30],
            c='b', s = 20,
            label='TRUE')

# 显示图片
plt.legend(title = "y==30")
plt.xlabel('ACC')
plt.ylabel('Y')
plt.show()

# 导入绘图工具 seaborn 为 sns
import seaborn as sns

# 从模拟数据中筛选出 y 值为 30 的样本，生成对应的后验分布。
king_posterior = king_sim[king_sim['y'] == 30]

# 绘制分布图：概率密度+柱状图
sns.displot(king_posterior['ACC'], kde=True)
plt.xlabel('ACC')
plt.ylabel('Density')
plt.show()

# 导入统计建模工具包 scipy.stats 为 st
import scipy.stats as st

# 设置 x 轴范围 [0,1]
x = np.linspace(0,1,10000)
# 设置 Beta 分布参数
a,b = 45,55

# 形成先验分布
prior = st.beta.pdf(x,a,b)/np.sum(st.beta.pdf(x,a,b))

# 形成似然
k = 30                  # k 代表正确率为1的次数
n = 50                  # n 代表总次数
likelihood = st.binom.pmf(k,n,x)

# 计算后验
unnorm_posterior = prior * likelihood                  # 计算分子
posterior = unnorm_posterior/np.sum(unnorm_posterior)  # 结合分母进行计算
likelihood = likelihood /np.sum(likelihood)            # 为了方便可视化，对似然进行类似后验的归一化操作

# 绘图
plt.plot(x,posterior, color="#009e74", alpha=0.5, label="posterior")
plt.plot(x,likelihood, color="#0071b2", alpha=0.5, label="likelihood")
plt.plot(x,prior, color="#f0e442", alpha=0.5, label="prior")
plt.legend()
plt.xlabel("ACC")
plt.fill_between(x, prior, color="#f0e442", alpha=0.5)
plt.fill_between(x, likelihood, color="#0071b2", alpha=0.5)
plt.fill_between(x, posterior, color="#009e74", alpha=0.5)
plt.xlim([0,1])
plt.show()

print(
  "近似值：",
  "均值，",
  king_posterior['ACC'].mean(),
  "。标准差，",
  king_posterior['ACC'].std()
)
print(f"10,000次模拟中, {king_posterior.shape[0]}次与观测到的Y = 30数据匹配")

# 模拟新的数据
size = 50000 # 不同于之前的 10000
king_sim2 = pd.DataFrame({'ACC': np.random.beta(45, 55, size=size)})
king_sim2['y'] = np.random.binomial(n=50, p=king_sim2['ACC'])
king_posterior2 = king_sim2[king_sim2['y'] == 30]
print(f"50,000次模拟中, {king_posterior2.shape[0]}次与观测到的Y = 30数据匹配")

import numpy as np
from scipy.stats import beta
import matplotlib.pyplot as plt

fig, ax = plt.subplots(figsize=(13, 5))
pz.Beta(70, 30).plot_pdf(pointinterval=True,ax=ax)
pz.Beta(7, 3).plot_pdf(pointinterval=True,ax=ax)
pz.Beta(14, 6).plot_pdf(pointinterval=True,ax=ax)
plt.tight_layout()
plt.show()