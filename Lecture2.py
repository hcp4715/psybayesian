# -*- coding: utf-8 -*-
"""
Lecture 2: Bayes' Rule — Python 对照代码
=========================================
与 Lecture2.qmd 对应的 Python 实现:
  Part 2  计数、概率与贝叶斯定理 (4 颗小球例: 计数 -> 后验)
  Part 3  随机变量的贝叶斯模型 (二项分布)
  延伸练习 Herzenstein 等 (2024) 可重复性数据 (旧版讲义内容)
随机种子统一为 84735, 保证结果可复现。

依赖: pandas, numpy, matplotlib, scipy
运行: python Lecture2.py
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy import stats as st

# ---------- Part 2: 计数例 (4 颗小球, 蓝/白数量未知) ----------
# 5 种推测: 蓝球占比 theta = [0, 0.25, 0.5, 0.75, 1]

# 数据 "蓝白蓝" 下, 各推测产生该数据的方式数 (计数!)
ways = np.array([0, 3, 8, 9, 0])
print("数据'蓝白蓝'的方式数 ways =", ways)

# 归一化 -> 后验 (默认各推测先验相等)
post1 = ways / ways.sum()
print("后验 (plausibility) =", post1.round(4))   # [0, .15, .4, .45, 0]

# 第 4 次抽到蓝: 新似然 P(蓝|theta) = theta, 旧后验成为新先验
theta = np.array([0, 0.25, 0.5, 0.75, 1])
product = theta * post1
post2 = product / product.sum()
print("顺序更新后验 ('蓝白蓝蓝') =", post2.round(4))   # [0, .065, .348, .587, 0]
print("证据 (归一化分母) =", round(product.sum(), 4))

# ---------- 0. 延伸练习: 读取数据 (Herzenstein 2024, 可重复性) ----------
try:
    df = pd.read_csv("/home/mw/input/bayes3797/replicated_language_cleaned.csv")  # 和鲸平台路径
except FileNotFoundError:
    df = pd.read_csv("data/replicated_language_cleaned.csv")                      # 本地路径

print("数据规模:", df.shape)
print(df.head())

# ---------- 1. 数据预处理: 中位数二值化 ----------
median_certain = df["certain"].median()
df["language_style"] = (df["certain"] > median_certain).astype(int)  # 1=确切语言, 0=一般

print("\n[1] 可重复性比例 (1=可重复):")
level_counts = df["replicated"].value_counts().sort_index()
print(pd.DataFrame({
    "数量": level_counts,
    "百分比": (100 * level_counts / len(df)).round(2),
}))

print("\n[1] 语言风格 × 可重复性 交叉表 (列内为数量):")
cross = df.groupby(["replicated", "language_style"]).size().unstack(fill_value=0)
print(cross)

# 可重复/不可重复组内使用确切语言的比例
p_A_given_B = cross.loc[1, 1] / cross.loc[1].sum()
p_A_given_Bc = cross.loc[0, 1] / cross.loc[0].sum()
print(f"\nP(A|B)  = {p_A_given_B:.4f}   (可重复研究中使用确切语言的比例)")
print(f"P(A|Bc) = {p_A_given_Bc:.4f}  (不可重复研究中使用确切语言的比例)")

# ---------- 2. Part 2: 从先验模拟 10000 项研究 ----------
print("\n[2] 从先验模拟研究可重复性:")
article = pd.DataFrame({"replicated": ["yes", "no"]})
prior = [0.4, 0.6]  # P(B), P(B^c)

np.random.seed(84735)
article_sim = article.sample(n=10000, weights=prior, replace=True)

sim_counts = article_sim["replicated"].value_counts().sort_index()
print(pd.DataFrame({"数量": sim_counts, "百分比": (100 * sim_counts / 10000).round(2)}))

# 按数据模型模拟语言风格: 可重复 56%, 不可重复 45% 用确切语言
rng = np.random.default_rng(84735)
data_model = np.where(article_sim["replicated"] == "no", 0.45, 0.56)
article_sim["language"] = [
    rng.choice(["certain", "uncertain"], p=[p, 1 - p]) for p in data_model
]

print("\n[2] 语言风格 × 可重复性 (模拟):")
print(article_sim.groupby(["language", "replicated"]).size().unstack(fill_value=0))

usage_yes = article_sim[article_sim["language"] == "certain"]
sim_post = usage_yes["replicated"].value_counts()
sim_post_p = sim_post / sim_post.sum()
print(f"\n模拟后验 P(B|A) = {sim_post_p.get('yes', 0):.4f}  (精确值 0.453)")

# ---------- 3. Part 3: 二项分布 PMF ----------
print("\n[3] 二项 PMF: n=6, π=0.5")
y = np.arange(0, 7)
n, p = 6, 0.5
prob = st.binom.pmf(y, n, p)
pmf_table = pd.DataFrame({"成功次数 y": y, "f(y|π)": prob.round(4)})
print(pmf_table)
print("所有概率之和 =", prob.sum())

# ---------- 4. 三派观点与似然 ----------
p_values = np.array([0.5, 0.8, 0.2])          # 中立/乐观/悲观
pmfs = np.array([st.binom.pmf(y, n, p) for p in p_values])

print("\n[4] 不同 π 下的 f(y|π):")
print(pd.DataFrame(
    np.vstack([y, pmfs]).T.round(4),
    columns=["y", "π=0.5 (中立)", "π=0.8 (乐观)", "π=0.2 (悲观)"],
))

lik_obs = st.binom.pmf(1, n, p_values)        # 观察到 y=1 时的似然
print("\n[4] 观察到 y=1 时, 不同 π 下的似然 L(π|y=1):")
print(pd.DataFrame({"π": p_values, "L(π|y=1)": lik_obs.round(4)}))

# ---------- 5. 先验 → 后验 (精确计算) ----------
pi_grid = np.array([0.2, 0.5, 0.8])
prior_pi = np.array([0.10, 0.25, 0.65])
lik_pi = st.binom.pmf(1, n, pi_grid)
post_pi = prior_pi * lik_pi / np.sum(prior_pi * lik_pi)

print("\n[5] 精确后验 f(π|y=1):")
print(pd.DataFrame({
    "π": pi_grid,
    "f(π)": prior_pi,
    "L(π|y=1)": lik_pi.round(4),
    "f(π)L(π|y=1)": (prior_pi * lik_pi).round(5),
    "f(π|y=1)": post_pi.round(3),
}))

# ---------- 6. Posterior simulation ----------
np.random.seed(84735)
sim_pi = pd.DataFrame({"pi": pi_grid}).sample(
    n=10000, weights=prior_pi, replace=True
)
sim_pi["y"] = np.random.binomial(n=6, p=sim_pi["pi"])

print("\n[6] 模拟: 抽到的 π 比例:")
print(sim_pi["pi"].value_counts(normalize=True).sort_index().round(3))

sim_post = sim_pi[sim_pi["y"] == 1]
sim_post_pi = sim_post["pi"].value_counts(normalize=True).sort_index()
print("\n[6] 模拟后验 f(π|y=1) (应接近 0.617/0.368/0.015):")
print(sim_post_pi.round(3))

# ---------- 绘图 (运行时弹出) ----------
if __name__ == "__main__":
    fig, axes = plt.subplots(1, 3, figsize=(12, 4))
    for ax, (pi_v, pmf) in zip(axes, zip(p_values, pmfs)):
        ax.vlines(y, 0, pmf, color="gray", linewidth=1.5)
        ax.plot(y, pmf, "ko")
        ax.set_title(rf"$\pi = {pi_v}$")
        ax.set_xlim(-0.5, 6.5)
        ax.set_ylim(0, 0.5)
    fig.suptitle(r"$f(y|\pi)$,  Binomial(n=6)")
    plt.tight_layout()
    plt.show()
