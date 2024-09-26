# 高级心理统计
## 《贝叶斯推断及其在Python中的实现》

## 教学目标：
（1）理解贝叶斯推断的基本原理；
（2）了解Python工具包PyMC3的语法和结构
（3）能够使用PyMC解决心理学/社会科学中的统计问题（如层级线性模型）

## 考核方式：
考勤10%
小作业：45%
（1）使用`numpy`与`pandas`进行基本的数据预处理
（2）使用PyMC对随机变量进行简单线性回归模型的建模
（3）使用arviz进行统计推断

大作业：45%
	真实的数据分析（分工、完整度、呈现）

## 风格：
内容有挑战、考核不复杂

## 课时
8.29 ～ 12.30，18周

## 大纲
### Intro
1: 课程介绍（为什么要用贝叶斯/PyMC，展示一个回归分析例子，课程安排）

### I Basics：
2: 贝叶斯公式
* 单一事件的贝叶斯模型(先验、似然、分母和后验)
* 随机变量的贝叶斯模型(先验、似然、分母和后验)

3: 建立一个完成的贝叶斯模型：Beta-Binomial model
* Beta先验
* Binomial与似然
* Beta-Binomial model

4: 贝叶斯模型的特点：数据-先验与动态更新
* 先验与数据对后验的影响
* 数据序列不影响后验

5: 经典的贝叶斯模型：Conjugate families
* Gamma-Poisson
* Normal-Normal

### II 近似后验估计
6: 近似后验的方法
* 网格法
* MCMC

7: 深入一种MCMC算法
* M-H 算法

8: 基于后验的推断
* 后验估计
* 假设检验
* 后验预没

### III Bayesian回归模型
9: 简单的Bayesian线性回归模型
* 建立模型
* 调整先验
* 近似后验
* 基于近似后验的推断
* 序列分析

10: 对回归模型的评估
* 评估模型的标准
* 对简单线性模型的估计
  
11: 扩散线性模型
* 多自变量的线性回归

12: GLM: Possion & Negative Binomial Regression

13: GLM: Logistic Regression

14: Hierachical Bayesian Model (1)

15: Hierachical Bayesian Model (2)

16: 学术报告：贝叶斯结构方程模型(中山大学潘俊豪教授)

17: 大作业汇报


