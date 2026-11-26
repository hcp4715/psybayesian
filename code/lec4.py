import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from scipy.stats import beta as beta_dist
from IPython.display import display

data = pd.read_csv("data/evans2020JExpPsycholLearn_exp1_full_data.csv")

print("被试数量：", len(data.subject.unique()))
data.groupby(["subject"])[["correct"]].mean().head(10)

# 选取需要的列
data = data[["subject", "percentCoherence", "correct", "RT"]]

# 筛选符合条件的数据
data_subj1 = data.query('subject == 82111 & percentCoherence == 5')

# 打印前 10 条抽取的数据
print("被试 82111 在 5% 一致性正确率数据：", data_subj1.correct.mean())
data_subj1.head(5)

# 统计 'binary' 列中各个值的出现次数
data_subj1['correct'].value_counts()

def bayesian_analysis_plot(
        alpha, beta, y, n,
        ax=None,
        plot_prior=True,
        plot_likelihood=True,
        plot_posterior=True,
        xlabel=r"ACC $\pi$",
        show_legend=True,
        legend_loc="upper left"):
    """
    该函数绘制先验分布、似然分布和后验分布的 PDF 图示在指定的子图上。

    参数:
    - alpha: Beta 分布的 alpha 参数（先验）
    - beta: Beta 分布的 beta 参数（先验）
    - y: 观测数据中的支持次数
    - n: 总样本数
    - ax: 子图对象，在指定子图上绘制图形
    """

    if ax is None:
        ax = plt.gca()

    if plot_prior:
        # 先验分布
        x_prior = np.linspace(
            beta_dist.ppf(0.0001, alpha, beta),
            beta_dist.ppf(0.9999, alpha, beta),
            100,
        )
        prior_density = beta_dist.pdf(x_prior, alpha, beta)
        ax.plot(x_prior, prior_density, color="black", label="prior")
        ax.fill_between(x_prior, prior_density, color="#f0e442", alpha=0.5)

    if plot_likelihood:
        # 固定数据 (y, n) 后，二项似然关于参数 pi 的归一化曲线
        x = np.linspace(0, 1, 1000)
        likelihood = beta_dist.pdf(x, y + 1, n - y + 1)
        ax.plot(
            x,
            likelihood,
            color="black",
            label=r"$\mathbf{Binomial}$" + rf"(n={n},p={round(y / n, 2)})",
        )
        ax.fill_between(x, likelihood, color="#0071b2", alpha=0.5, label="likelihood")

    if plot_posterior:
        # 后验分布
        post_alpha = alpha + y
        post_beta = beta + n - y
        x_posterior = np.linspace(
            beta_dist.ppf(0.0001, post_alpha, post_beta),
            beta_dist.ppf(0.9999, post_alpha, post_beta),
            100,
        )
        posterior_density = beta_dist.pdf(x_posterior, post_alpha, post_beta)
        ax.plot(x_posterior, posterior_density, color="black", label="posterior")
        ax.fill_between(x_posterior, posterior_density, color="#009e74", alpha=0.5)

    if show_legend:
        ax.legend(loc=legend_loc)
    else:
        ax.legend().set_visible(False)

    # 设置图形
    ax.set_xlabel(xlabel)
    sns.despine(ax=ax)
    return ax

# 创建一个单独的图和轴
fig, ax = plt.subplots(figsize=(9, 5))

# 先验参数 alpha=70, beta=30, 观测数据 y=152, n=253
bayesian_analysis_plot(alpha=70, beta=30, y=152, n=253, ax=ax)
ax.set_xlim(0.4, 0.9)

# 显示图像
plt.tight_layout()
plt.show()

# 定义先验分布的 alpha 和 beta
alpha = 70
beta = 30

# 根据数据定义不同的二项分布数据 (y, n)
data_list = [(77, 128), (152, 253), (231, 385)]

# 创建一个包含三个子图的画布
fig, axes = plt.subplots(1, 3, figsize=(15, 5), sharex=True, sharey=True)

for i, ax in enumerate(axes):
    bayesian_analysis_plot(alpha=alpha, beta=beta, y=data_list[i][0], n=data_list[i][1], ax=ax, plot_posterior=False)
    ax.set_xlim(0.4,0.9)

# 显示图形
plt.tight_layout()
plt.show()

# 定义先验分布的 alpha 和 beta
alpha = 70
beta = 30

# 根据数据定义不同的二项分布数据 (y, n)
data_list = [(77, 128), (152, 253), (231, 385)]

# 创建一个包含三个子图的画布
fig, axes = plt.subplots(1, 3, figsize=(15, 5), sharex=True, sharey=True)

for i, ax in enumerate(axes):
    bayesian_analysis_plot(alpha=alpha, beta=beta, y=data_list[i][0], n=data_list[i][1], ax=ax, plot_posterior=True)
    ax.set_xlim(0.4,0.9)

# 显示图形
plt.tight_layout()
plt.show()

import ipywidgets as widgets
import warnings

# 忽略 FutureWarning
warnings.simplefilter(action='ignore', category=FutureWarning)

# 初始化计数器
count = -1

# 定义按钮点击时调用的函数
def on_button_clicked(b):
    global count
    count += 1
    update_plot()  # 调用更新函数

# 更新函数，它会重新执行 interactive_plot
def update_plot():
    interactive_plot.update()  # 更新 interactive_plot 的输出

# 创建按钮并绑定点击事件
button = widgets.Button(description="Update with more data",
                        layout=widgets.Layout(width='400px', height='60px'))
# 设置按钮的背景颜色为蓝色，字体颜色为白色
button.style.button_color = '#1E90FF'  # 浅蓝色 (可以调整为其他蓝色)
button.style.text_color = 'white'
button.on_click(on_button_clicked)


def plot_func(
        data: pd.Series,
        prior_alpha=1,
        prior_beta=1,
        init_trial=20,
        step=1,
        show_prior=True,
        show_last_post=True
):
    """
    绘制贝叶斯更新过程中的后验分布和先验分布。

    参数:
    - data: pd.Series, 包含每次试验的结果(0 或 1)。
    - prior_alpha: float, beta分布的先验参数alpha。
    - prior_beta: float, beta分布的先验参数beta。
    - init_trial: int, 初始试验的编号。
    - step: int, 每次更新的步长。
    - show_prior: bool, 是否显示先验分布。
    - show_last_post: bool, 是否显示上一次试验的后验分布。
    返回:
    - ax: 当前绘制的图表对象。
    """

    # 使用全局变量`count`来记录当前试验轮次
    global count

    # 计算上一次试验和当前试验的编号
    trial_number_last = init_trial + (count - 1) * step
    trial_number_current = init_trial + count * step

    # 获取当前的绘图对象
    ax = plt.gca()

    # 定义x轴上从0到1的1000个点
    x = np.linspace(0, 1, 1000)

    # 如果show_prior为True，绘制先验分布
    if show_prior:
        y = beta_dist.pdf(x, prior_alpha, prior_beta)
        ax.plot(x, y, "-.", label="prior", color="navy")

    # 如果count小于0，只显示先验分布并退出
    if count < 0:
        ax.set_title(f"Prior Beta: alpha={prior_alpha}, beta={prior_beta}")
        return ax
    # 如果当前试验编号超出数据长度，显示所有试验的结果并退出
    elif trial_number_current > data.shape[0]:
        ax.set_title(f"All Trials {data.shape[0]} with {data.sum()} corrects")
        return ax
    # 如果count等于0，显示初始试验的结果
    elif count == 0:
        tmp_data = data[:trial_number_current]
        ax.set_title(
            f"Trial {trial_number_current - init_trial} with {tmp_data.shape[0]} trials and {tmp_data.sum()} corrects")
    # 如果count大于0，显示上一次试验的后验分布（如果需要）和当前试验结果
    elif count > 0:
        tmp_data = data[trial_number_last:trial_number_current]
        ax.set_title(f"Trial {trial_number_last} with {tmp_data.shape[0]} trials and {tmp_data.sum()} corrects")

        # 如果show_last_post为True，显示上一次试验的后验分布
        if show_last_post:
            n_correct = data[:trial_number_last].sum()
            n_false = data[:trial_number_last].shape[0] - n_correct
            post_alpha = prior_alpha + n_correct
            post_beta = prior_beta + n_false
            y = beta_dist.pdf(x, post_alpha, post_beta)
            ax.plot(x, y, label="posterior (t-1)", color="olive", alpha=0.3)

    # 计算当前试验的后验分布并绘制
    n_correct = data[:trial_number_current].sum()
    n_false = data[:trial_number_current].shape[0] - n_correct
    post_alpha = prior_alpha + n_correct
    post_beta = prior_beta + n_false

    # 绘制当前试验的后验分布
    y = beta_dist.pdf(x, post_alpha, post_beta)
    ax.plot(x, y, label="posterior", color="orangered")

    # 显示图例并去除图框
    ax.legend()
    sns.despine(ax=ax)

# 使用 interactive 创建界面
interactive_plot = widgets.interactive(
    plot_func,
    data=widgets.fixed(data_subj1.correct),
    prior_alpha=(1, 200, 1),
    prior_beta=(1, 200, 1),
    init_trial=(1, 100, 1),
    step=(1, 20, 1)
)

# 显示按钮和 interactive 组件
display(button, interactive_plot)

# ----------------------------------------
# ----------------------------------------

def plot_pdf(alpha, beta, level=0.95, line_color="#008b92", ax=None, baseline=0):
    """绘制 Beta 密度及 95%、50% 分位区间和均值。"""
    if ax is None:
        ax = plt.gca()

    x = np.linspace(0, 1, 1000)
    density = beta_dist.pdf(x, alpha, beta)
    tail = (1 - level) / 2
    outer_low, outer_high = beta_dist.ppf([tail, 1 - tail], alpha, beta)
    inner_low, inner_high = beta_dist.ppf([0.25, 0.75], alpha, beta)
    mean = alpha / (alpha + beta)

    visible_density = np.where(density >= baseline, density, np.nan)
    ax.plot(x, visible_density, color=line_color, linewidth=1.6)
    ax.axhline(baseline, linestyle="--", color="0.6")
    ax.plot([outer_low, outer_high], [baseline, baseline], color="black", linewidth=2)
    ax.plot([inner_low, inner_high], [baseline, baseline], color="black", linewidth=4)
    ax.scatter(mean, baseline, s=70, facecolor="white", edgecolor="black", zorder=3)
    ax.set_xlim(0, 1)
    ax.set_ylim(baseline, density.max() * 1.08)
    ax.set_title(f"Beta(alpha={alpha}, beta={beta})")
    ax.set_xlabel("x")
    ax.set_ylabel("Density")
    ax.tick_params(axis="y", left=False, labelleft=False)
    sns.despine(ax=ax, left=True)
    return ax

# 创建一个1x3的网格子图
fig, axs = plt.subplots(1, 3, figsize=(10, 4))

# 绘制 Beta 分布的 PDF，并显示与 R 版本一致的分位区间
plot_pdf(70, 30, ax=axs[0])
plot_pdf(10, 1, ax=axs[1])
plot_pdf(1, 1, ax=axs[2])

# 设置每个子图的X轴范围为0到1
plt.tight_layout()
plt.show()

data = pd.read_csv("data/evans2020JExpPsycholLearn_exp1_full_data.csv")

# 选取需要的列
data = data[["subject", "percentCoherence", "correct", "RT"]]

# 筛选符合条件的数据
data_subj1 = data.query('subject == 82111 & percentCoherence == 5')

#统计 'binary' 列中各个值的出现次数
print(data_subj1['correct'].value_counts())
data_subj1.head(5)

# 定义不同的 Beta 分布参数
params = [(70, 30), (10, 1), (1, 1)]

fig, axes = plt.subplots(nrows=1, ncols=len(params), figsize=(15, 4))

# 循环遍历不同的参数组合
for (alpha_, beta_), ax in zip(params, axes.flatten()):
    bayesian_analysis_plot(alpha=alpha_, beta=beta_, y=152, n=253, ax=ax, plot_posterior=False)

    # 设置子图标题
    ax.set_title(f'prior: Beta({alpha_},{beta_})')

plt.tight_layout()
plt.show()

# 定义不同的 Beta 分布参数
params = [(70, 30), (10, 1), (1, 1)]

fig, axes = plt.subplots(nrows=1, ncols=len(params), figsize=(15, 4))

# 循环遍历不同的参数组合
for (alpha_, beta_), ax in zip(params, axes.flatten()):
    bayesian_analysis_plot(alpha=alpha_, beta=beta_, y=152, n=253, ax=ax)

    # 设置子图标题
    ax.set_title(f'prior: Beta({alpha_},{beta_})')

plt.tight_layout()
plt.show()

# ----------------------------------------
# ----------------------------------------

# 定义不同的 Beta 分布参数
params = [(70, 30), (700, 300), (7000, 3000)]

fig, axes = plt.subplots(nrows=1, ncols=len(params), figsize=(15, 4))

# 循环遍历不同的参数组合
for (alpha_, beta_), ax in zip(params, axes.flatten()):
    bayesian_analysis_plot(alpha=alpha_, beta=beta_, y=152, n=253, ax=ax)

    # 设置子图标题
    ax.set_title(f'prior: Beta({alpha_},{beta_})')

plt.tight_layout()
plt.show()

# 创建 3x3 的子图布局
fig, axes = plt.subplots(3, 3, figsize=(18, 15))

# 调用绘制函数，对不同的先验和似然进行组合
bayesian_analysis_plot(70, 30, 77, 128, axes[0, 0])
bayesian_analysis_plot(70, 30, 152, 253, axes[0, 1])
bayesian_analysis_plot(70, 30, 231, 385, axes[0, 2])
bayesian_analysis_plot(10, 1, 77, 128, axes[1, 0])
bayesian_analysis_plot(10, 1, 152, 253, axes[1, 1])
bayesian_analysis_plot(10, 1, 231, 385, axes[1, 2])
bayesian_analysis_plot(1, 1, 77, 128, axes[2, 0])
bayesian_analysis_plot(1, 1, 152, 253, axes[2, 1])
bayesian_analysis_plot(1, 1, 231, 385, axes[2, 2])

# 设置 x 轴范围
for ax in axes.flatten():
    ax.set_xlim(0.4, 0.9)

# 调整布局
plt.tight_layout()
plt.show()

#---------------------------------------------------------------------------
#                            请替换...填入 Beta 分布参数, alpha 和 beta
#---------------------------------------------------------------------------
# 设置 Beta 分布参数
alpha = ...     # alpha
beta  = ...     # beta

#---------------------------------------------------------------------------
#                            请替换...请填入观测数据 y 和 n
#---------------------------------------------------------------------------
y = ...     # y 代表支持数
n = ...     # n 代表总人数

#---------------------------------------------------------------------------
#                            请使用 bayesian_analysis_plot 进行绘图
#---------------------------------------------------------------------------
# 完成上述参数后，取消下一行注释并运行：
# bayesian_analysis_plot(alpha, beta, y, n)

# 答案

#---------------------------------------------------------------------------
#                            请填入 Beta 分布参数，alpha 和 beta
#---------------------------------------------------------------------------
# 设置 Beta 分布参数
alpha = 10     # alpha
beta  = 50     # beta

#---------------------------------------------------------------------------
#                            请填入观测数据 y 和 n
#---------------------------------------------------------------------------
y = 80     # y 代表支持数
n = 180     # n 代表总人数

bayesian_analysis_plot(alpha, beta, y, n)
plt.show()
