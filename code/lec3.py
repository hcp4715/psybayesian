# 导入必要的库
# 显示ModuleNotFoundError时在控制台运行 pip install xxx；例如pip install seaborn
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

# 本例数据来自 Evans et al. (2020) 随机点运动任务 5% 一致性条件：
# 取 30 个正确试次 + 20 个错误试次，共 50 个试次
# （下文所有分析固定使用 n = 50, y = 30，不再依赖原始数据文件）

# 创建离散先验分布数据
prior_disc = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0]         # 离散先验分布的取值
prior_dis_prob = [0, 0, 0, 0, 0, 0.10, 0.80, 0.10, 0, 0]  # 对应的概率
prior_disc_data = pd.DataFrame({'theta': prior_disc, 'f(theta)': prior_dis_prob})

# 将'theta'列的数据类型转换为字符串，以便在图表中正确显示
prior_disc_data['theta'] = prior_disc_data['theta'].astype('str')

# 创建一个单独的图表
plt.figure(figsize=(8, 5))

# 绘制离散先验分布的条形图
sns.barplot(data=prior_disc_data, x='theta', y='f(theta)', palette="deep")

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
prior_disc_data = pd.DataFrame({'theta': prior_disc, 'f(theta)': prior_dis_prob})
# 将'theta'列的数据类型转换为字符串，以便在图表中正确显示
prior_disc_data['theta'] = prior_disc_data['theta'].astype('str')

# 创建一个包含两个子图的图表
f, (ax1, ax2) = plt.subplots(1, 2, figsize=(15, 4))

# 在第一个子图中绘制离散先验分布的条形图
sns.barplot(data=prior_disc_data, x='theta', y='f(theta)', palette="deep", ax=ax1)
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
binom_dist = pz.Binomial(n=50, p=0.1)

# 绘制该分布的概率密度函数 (PDF)
binom_dist.plot_pdf()

# 移除图的上边框和右边框
sns.despine()

# 显示图表
plt.show()
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
plot_all_pi.set_ylabels(r"$f(y|\theta)$")

# 设置子图的标题模板
plot_all_pi.set_titles(col_template="Bin(50,{col_name})")

from scipy.special import comb

# 定义似然函数
def likelihood(theta, Y=30, N=50):
    return comb(N, Y) * (theta**Y) * ((1-theta)**(N-Y))

# 定义 theta 范围在 [0, 1] 之间
theta_values = np.linspace(0, 1, 1000)

# 计算每个theta对应的似然值
likelihood_values = likelihood(theta_values)

# 设置Seaborn的绘图样式
sns.despine() # 移除图的上边框和右边框

# 绘制似然函数，使用Seaborn的绘图功能
sns.lineplot(x=theta_values, y=likelihood_values, color="grey", label="Likelihood L(θ | Y=30)")

# 设置图表标题和标签
plt.xlabel(r'$\theta$')
plt.ylabel("Likelihood")

# 在theta=0.6处画出一条虚线
plt.axvline(x=0.6, color='red', linestyle='--', label="θ=0.6 (Max)")

# 显示图例
plt.legend()
# 展示图表
plt.show()

# 设置 x 轴范围 [0,1]
x = np.linspace(0,1,10000)
# 设置 Beta 分布参数
a,b = 70,30
# 形成先验分布
prior = st.beta.pdf(x, a, b)                     # 真密度（积分为 1）

# 形成似然
k = 30     # k 代表正确率为1的次数
n = 50     # n 代表总次数
likelihood = st.binom.pmf(k,n,x)

# 计算后验：Beta 是 Binomial 的共轭先验，后验解析式为 Beta(a+k, b+n-k)
likelihood = likelihood / likelihood.max() * prior.max()  # 缩放到与先验同高，便于比较
posterior = st.beta.pdf(x, a + k, b + n - k)              # 即 Beta(100, 50)

# 绘图
plt.plot(x,posterior, color="#009e74", alpha=0.5, label="posterior")
plt.plot(x,likelihood, color="#0071b2", alpha=0.5, label="likelihood")
plt.plot(x,prior, color="#f0e442", alpha=0.5, label="prior")
plt.legend()
plt.xlabel(r'$\theta$')
plt.fill_between(x, prior, color="#f0e442", alpha=0.5)
plt.fill_between(x, likelihood, color="#0071b2", alpha=0.5)
plt.fill_between(x, posterior, color="#009e74", alpha=0.5)
sns.despine()
# 设置随机种子，以便后续可以重复结果
np.random.seed(84735)

# 模拟 10000 次数据
king_sim = pd.DataFrame({'theta': np.random.beta(70, 30, size=10000)})  # 从Beta(70,30)先验中模拟10,000个theta值
king_sim['y'] = np.random.binomial(n=50, p=king_sim['theta'])       # 从每个theta值中模拟Bin(50,theta)的潜在正确判断次数Y

# 显示部分数据
king_sim.head()
# 绘制散点图：正确次数 (Y!=30)部分，用黑色表示
plt.scatter(king_sim['theta'][king_sim['y']!=30],
            king_sim['y'][king_sim['y']!=30],
            c='black', s = 3,
            label='FALSE')
# 绘制散点图：正确次数 (Y=30)部分，用蓝色表示
plt.scatter(king_sim['theta'][king_sim['y']==30],
            king_sim['y'][king_sim['y']==30],
            c='b', s = 20,
            label='TRUE')

# 显示图片
plt.legend(title = "y==30")
plt.xlabel(r'$\theta$')
plt.ylabel('Y')
plt.show()

# 从模拟数据中筛选出 y 值为 30 的样本，生成对应的后验分布。
king_posterior = king_sim[king_sim['y'] == 30]

# 绘制分布图：概率密度 + 柱状图，并叠加解析后验曲线
g = sns.displot(king_posterior['theta'], kde=True, stat="density")
ax = g.axes.flat[0]
xs = np.linspace(0, 1, 500)
ax.plot(xs, st.beta.pdf(xs, 100, 50), color="#009e74", lw=2, label="Beta(100,50)")
ax.set_xlabel(r'$\theta$')
ax.set_ylabel('Density')
ax.legend()
plt.show()

# 设置 x 轴范围 [0,1]
x = np.linspace(0,1,10000)
# 设置 Beta 分布参数
a,b = 70,30

# 形成先验分布
prior = st.beta.pdf(x, a, b)                     # 真密度（积分为 1）

# 形成似然
k = 30                  # k 代表正确率为1的次数
n = 50                  # n 代表总次数
likelihood = st.binom.pmf(k,n,x)

# 计算后验：Beta 是 Binomial 的共轭先验，后验解析式为 Beta(a+k, b+n-k)
likelihood = likelihood / likelihood.max() * prior.max()  # 缩放到与先验同高，便于比较
posterior = st.beta.pdf(x, a + k, b + n - k)              # 即 Beta(100, 50)

# 绘图
plt.plot(x,posterior, color="#009e74", alpha=0.5, label="posterior")
plt.plot(x,likelihood, color="#0071b2", alpha=0.5, label="likelihood")
plt.plot(x,prior, color="#f0e442", alpha=0.5, label="prior")
plt.legend()
plt.xlabel(r'$\theta$')
plt.fill_between(x, prior, color="#f0e442", alpha=0.5)
plt.fill_between(x, likelihood, color="#0071b2", alpha=0.5)
plt.fill_between(x, posterior, color="#009e74", alpha=0.5)
plt.xlim([0,1])
plt.show()

print(
  "近似值：",
  "均值，",
  king_posterior['theta'].mean(),
  "。标准差，",
  king_posterior['theta'].std()
)
print(f"10,000次模拟中, {king_posterior.shape[0]}次与观测到的Y = 30数据匹配")

# 模拟新的数据
size = 50000 # 不同于之前的 10000
king_sim2 = pd.DataFrame({'theta': np.random.beta(70, 30, size=size)})
king_sim2['y'] = np.random.binomial(n=50, p=king_sim2['theta'])
king_posterior2 = king_sim2[king_sim2['y'] == 30]
print(f"50,000次模拟中, {king_posterior2.shape[0]}次与观测到的Y = 30数据匹配")

fig, ax = plt.subplots(figsize=(13, 5))
pz.Beta(70, 30).plot_pdf(pointinterval=True,ax=ax)
pz.Beta(7, 3).plot_pdf(pointinterval=True,ax=ax)
pz.Beta(14, 6).plot_pdf(pointinterval=True,ax=ax)
plt.tight_layout()
plt.show()