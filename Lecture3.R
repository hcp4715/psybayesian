# 安装和加载包
options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
if (!requireNamespace('pacman', quietly = TRUE)) {
  install.packages('pacman')
}
pacman::p_load("tidyverse","ggplot2", "dplyr","gridExtra","papaja")
options(warn = -1)  # 抑制警告

#导入数据
data <- tryCatch({
  read.csv('/home/mw/input/bayes3797/evans2020JExpPsycholLearn_exp1_clean_data.csv') #平台路径
}, error = function(e) {
  read.csv('/Users/liumingyu/Desktop/1/PyBayesian/data/evans2020JExpPsycholLearn_exp1_clean_data.csv') #本地路径
})

# 选择特定模式的数据进行演示：
#筛选 correct 为 1 的数据，并选取前 30 个
#(原python文件为随机选取，但未设置随机种子，为保证可重复性这里改为固定选取前30个)
data_correct_1 <- data[data$correct == 1, ] %>%
  slice_head(n = 30)
#筛选 correct 为 0 的数据，并选取前 20 个
data_correct_0 <- data[data$correct == 0, ] %>%
  slice_head(n = 20)
#合并两部分数据，形成新的数据集
df <- rbind(data_correct_1, data_correct_0)
# 只显示特定列
head(df[,c("subject", "numberofDots", "percentCoherence", "correct", "RT")])

# 创建离散先验分布数据
prior_disc <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0) # 离散先验分布的取值
prior_dis_prob <- c(0, 0, 0, 0, 0, 0.1, 0.8, 0.1, 0, 0)         # 对应的概率
prior_disc_data <- data.frame(ACC = prior_disc, f_ACC = prior_dis_prob)

# 将'ACC'列的数据类型转换为字符型，以便在图表中正确显示
prior_disc_data$ACC <- as.character(prior_disc_data$ACC)

# 创建一个单独的图表
options(repr.plot.width=8, repr.plot.height=5) #自定义画布大小
ggplot2::ggplot(prior_disc_data, aes(x = ACC, y = f_ACC)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Discrete Prior", x = "ACC", y = "f(ACC)") +
  scale_y_continuous(expand = c(0,0)) + 
  scale_x_discrete(expand = c(0,0)) +
  papaja::theme_apa()

# 创建 Beta 分布的 PDF 图
# 设置绘图区域，确保大小适合 3x3 图
options(repr.plot.width=14, repr.plot.height=8) #自定义画布大小
par(mfrow = c(3, 3), mar = c(3, 3, 2, 1))

# Beta 分布参数列表
beta_params <- list(c(1, 5), c(1, 2), c(3, 7), 
                    c(1, 1), c(5, 5), c(20, 20), 
                    c(7, 3), c(2, 1), c(5, 1))

# 绘制 Beta 分布的 PDF
x_values <- seq(0, 1, by = 0.01)  # 生成0到1之间的序列用于绘图

# 计算并绘制每个 Beta 分布
for (params in beta_params) {
  y_values <- dbeta(x_values, shape1 = params[1], shape2 = params[2])  # 计算 PDF
  
  # 计算 95% 最高密度区间（HDI）
  lower_bound_95 <- qbeta(0.05, shape1 = params[1], shape2 = params[2])  # 2.5%
  upper_bound_95 <- qbeta(0.95, shape1 = params[1], shape2 = params[2])  # 97.5%
  
  # 计算 50% 最高密度区间
  lower_bound_50 <- qbeta(0.25, shape1 = params[1], shape2 = params[2])  # 25%
  upper_bound_50 <- qbeta(0.75, shape1 = params[1], shape2 = params[2])  # 75%
  
  # 计算均值
  mean_value <- params[1] / (params[1] + params[2])  # Beta 分布的均值
  
  # 绘制 PDF
  plot(x_values, y_values, type = 'l', 
       main = paste("Beta(alpha=", params[1], ", beta=", params[2], ")", sep = ""),
       xlab = "x", ylab = "Density", 
       col = "cyan4",lwd = 1.6,
       yaxt = "n",
       cex.main = 1.5,  # 字体大小（标题）
       cex.axis = 1.5)  # 字体大小（坐标轴刻度标签）) 
  
  # 在置信区间设置水平加粗黑线
  segments(lower_bound_95, 0, upper_bound_95, 0, col = "black", lwd = 2)  # 在 y=0 的水平线
  segments(lower_bound_50, 0, upper_bound_50, 0, col = "black", lwd = 4)  # 在 y=0 的水平线
  # 添加代表均值的圆
  points(mean_value, 0, pch = 21, cex = 1.2, col = "black", bg = "white")
}

# 创建离散先验分布数据
prior_disc <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0) # 离散先验分布的取值
prior_dis_prob <- c(0, 0, 0, 0, 0, 0.1, 0.8, 0.1, 0, 0)         # 对应的概率
prior_disc_data <- data.frame(ACC = prior_disc, f_ACC = prior_dis_prob)

# 将'ACC'列的数据类型转换为字符型，以便在图表中正确显示
prior_disc_data$ACC <- as.character(prior_disc_data$ACC)
# 绘制离散先验分布的线图
p1 <- ggplot2::ggplot(prior_disc_data, aes(x = ACC, y = f_ACC)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Discrete Prior\n", x = "ACC", y = "f(ACC)") +
  scale_y_continuous(expand = c(0, 0)) + 
  scale_x_discrete(expand = c(0,0)) +
  papaja::theme_apa()

# 绘制连续先验分布的线图
# 创建 Beta 分布的数据
x_values <- seq(0, 1, length.out = 100)
y_values <- dbeta(x_values, shape1 = 70, shape2 = 30)

# 绘制 Beta 分布的 PDF
p2 <- ggplot2::ggplot(data.frame(x = x_values, y = y_values),aes(x = x, y = y)) +
  geom_line(col = 'cyan4', size = 1) +
  labs( x = " ", y = NULL, title = "Continuous Prior\n(Beta(alpha = 70, beta = 30))")  +
  xlim(0, 1) +
  papaja::theme_apa()

#并排显示
options(repr.plot.width=14, repr.plot.height=5) #自定义画布大小
gridExtra::grid.arrange(p1, p2, ncol = 2)


# 设置二项分布的参数
n <- 50  # 试验次数
p <- 0.1  # 成功的概率
k <- 0:n
probabilities <- dbinom(k, size = n, prob = p)
# 创建数据框以便绘图
binom_data <- data.frame(k = k, probability = probabilities)
# 绘制二项分布的概率密度函数 (PDF)
options(repr.plot.width=8, repr.plot.height=5) #自定义画布大小
ggplot2::ggplot(binom_data, aes(x = k, y = probability)) +
  geom_line(stat = "identity", color = "cyan4", linetype = 3, size = 1) +
  geom_point(color = 'cyan4', size = 3) +  # 在每个点处添加标记
  labs(title = paste("Binomial (n =", n, ", p =", p, ")"),
       x = "Number of Successes",
       y = "Probability") +
  scale_x_continuous(expand = c(0.05,0.05), limits = c(0, 14), breaks = seq(0, 14, by = 2)) +
  papaja::theme_apa()

# 定义成功次数和总试验次数
y <- 0:50  # 成功次数
n <- 50    # 研究总次数

# 不同的 p 值列表与对应概率
p_values <- c(0.1, 0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9) 
probs <- sapply(p_values, function(p) dbinom(y, size = n, prob = p))

# 绘制三个子图，每个子图对应不同的成功概率
plots <- list()

for (i in 1:length(p_values)) {
  y_label <- ifelse(i %in% c(1, 4, 7), "f(y|ACC)", "")
  x_label <- ifelse(i %in% c(7, 8, 9), "y", "")
  plots[[i]] <- ggplot2::ggplot(data.frame(y = y, prob = probs[,i]), aes(x = y, y = prob)) +
    geom_bar(stat = "identity", fill = "grey") +
    labs(title = paste("Bin(", n, ",", p_values[i], ")", sep = ""),
         x = x_label,
         y = y_label) +
    xlim(0,50) +
    scale_y_continuous(expand = c(0, 0) , limits = c(0, 0.2)) + 
    papaja::theme_apa()
}

# 将三个图以子图的形式并排显示
options(repr.plot.width=14, repr.plot.height=10) #自定义画布大小
gridExtra::grid.arrange(grobs = plots, ncol = 3)

# 显示y=30的取值点
plots <- list()

for (i in 1:length(p_values)) {
  y_1  <- dbinom(30, size = n, prob = p_values[i])
  y_label <- ifelse(i %in% c(1, 4, 7), "f(y|π)", "")
  x_label <- ifelse(i %in% c(7, 8, 9), "y", "")
  plots[[i]] <- ggplot2::ggplot(data.frame(y = y, prob = probs[,i]), aes(x = y, y = prob)) +
    geom_segment(aes(xend = y, yend = 0), color = "gray", size = 1) +
    geom_segment(x = 30, y = 0, xend = 30, yend = y_1,, color = "black", size = 1) +
    geom_point(color = "gray", size = 1.5) +
    geom_point(x = 30, y = y_1, color = "black", size = 2) +
    labs(title = paste("Bin(", n, ",", p_values[i], ")", sep = ""),
         x = x_label,
         y = y_label) +
    xlim(0,50) +
    scale_y_continuous(limits = c(0, 0.2)) + 
    papaja::theme_apa()
}

# 将三个图以子图的形式并排显示
options(repr.plot.width=14, repr.plot.height=10) #自定义画布大小
gridExtra::grid.arrange(grobs = plots, ncol = 3)

# 定义似然函数
likelihood <- function(ACC, Y = 30, N = 50) {
  choose(N, Y) * (ACC^Y) * ((1 - ACC)^(N - Y))
}

# 定义 ACC 范围在 [0, 1] 之间
ACC_values <- seq(0, 1, length.out = 1000)

# 计算每个 ACC 对应的似然值
likelihood_values <- likelihood(ACC_values)

# 创建一个数据框以绘图
data <- data.frame(ACC = ACC_values, Likelihood = likelihood_values)

# 绘制似然函数
options(repr.plot.width=10, repr.plot.height=5) #自定义画布大小
ggplot2::ggplot(data, aes(x = ACC, y = Likelihood)) +
  geom_line(aes(color = "Likelihood L(ACC |Y=30)")) +  
  geom_vline(aes(xintercept = 0.6, color = 'ACC=0.6 (Max)'), linetype = 'dashed') + 
  labs(
    title = "Likelihood Function",
    x = "ACC",
    y = "Likelihood",
    color = NULL) +
  papaja::theme_apa() +
  theme(legend.position = "right",
        legend.text = element_text(size = 14)) +
  scale_color_manual(values = c("ACC=0.6 (Max)" = "red", "Likelihood L(ACC |Y=30)" = "grey"))

# 定义正确率的取值范围
acc_values <- seq(0, 1, length.out = 1000)

# 定义先验分布 Beta(70, 30)
alpha_prior <- 70
beta_prior <- 30
prior_distribution <- dbeta(acc_values, shape1 = alpha_prior, shape2 = beta_prior)

# 定义似然分布 Bin(50, ACC)
n_trials <- 50
y_observed <- 30
likelihood <- dbinom(y_observed, size = n_trials, prob = acc_values)

# 缩放似然分布
likelihood_scaled <- likelihood / max(likelihood) * max(prior_distribution)

# 创建绘图数据框
plot_data <- data.frame(
  ACC = acc_values,
  Prior = prior_distribution,
  Likelihood_Scaled = likelihood_scaled
)

# 绘图
options(repr.plot.width=10, repr.plot.height=5) #自定义画布大小
ggplot2::ggplot(plot_data, aes(x = ACC)) +
  geom_area(aes(y = Prior, fill = "Prior"), alpha = 0.6, size = 1, show.legend = TRUE) +
  geom_area(aes(y = Likelihood_Scaled, fill = "Likelihood"), alpha = 0.6, size = 1, show.legend = TRUE) +
  labs(x = "ACC", y = "Density", fill = NULL) +
  papaja::theme_apa() +
  scale_y_continuous(expand = c(0, 0)) +  
  ggtitle("Prior and Likelihood Density") +
  theme(legend.position = "right",
        legend.text = element_text(size = 14)) + 
  scale_fill_manual(values = c("Prior" = "#f0e442", "Likelihood" = "#0071b2"))  

# 设置 x 轴范围 [0,1]
x <- seq(0, 1, length.out = 10000)

# 设置 Beta 分布参数
a <- 70
b <- 30

# 形成先验分布 
prior <- dbeta(x, shape1 = a, shape2 = b) / sum(dbeta(x, shape1 = a, shape2 = b))

# 形成似然
k <- 30     # k 代表正确率为1的次数
n <- 50     # n 代表总次数
likelihood <- dbinom(k, size = n, prob = x)

# 计算后验
unnorm_posterior <- prior * likelihood                  # 计算分子
posterior <- unnorm_posterior / sum(unnorm_posterior)  # 结合分母进行计算
likelihood <- likelihood / sum(likelihood)               # 归一化似然以便于可视化

# 创建数据框以方便 ggplot 绘图
plot_data <- data.frame(
  ACC = x,
  Prior = prior,
  Likelihood = likelihood,
  Posterior = posterior
)

# 绘图
options(repr.plot.width=14, repr.plot.height=6) #自定义画布大小
ggplot2::ggplot(plot_data, aes(x = ACC)) +
  geom_line(aes(y = Prior, color = "Prior"), alpha = 0.5, size = 1, linetype = "solid") +
  geom_line(aes(y = Likelihood, color = "Likelihood"), alpha = 0.5, size = 1, linetype = "solid") +
  geom_line(aes(y = Posterior, color = "Posterior"), alpha = 0.5, size = 1, linetype = "solid") +
  geom_area(aes(y = Prior), fill = "#f0e442", alpha = 0.5) +
  geom_area(aes(y = Likelihood), fill = "#0071b2", alpha = 0.5) +
  geom_area(aes(y = Posterior), fill = "#009e74", alpha = 0.5) +
  labs(title = "Prior, Likelihood, and Posterior Distributions", x = "ACC", y = "Density",color = NULL) +
  scale_y_continuous(expand = c(0, 0)) + 
  papaja::theme_apa() +
  theme(legend.position = "right",
        legend.text = element_text(size = 14)) +
  scale_color_manual(values = c("Prior" = "#f0e442", "Likelihood" = "#0071b2", "Posterior" = "#009e74"))

# 设置随机种子，以便后续可以重复结果
set.seed(84735)

# 模拟 10000 次数据
n_simulations <- 10000
king_sim <- data.frame(ACC = rbeta(n_simulations, shape1 = 45, shape2 = 55))  # 从 Beta(45,55) 先验中模拟 10,000 个 ACC 值
king_sim$y <- rbinom(n_simulations, size = 50, prob = king_sim$ACC)  # 从每个 ACC 值中模拟 Bin(50, ACC) 的潜在正确判断次数 Y

# 显示部分数据
head(king_sim)

#创建一个新的变量用以区分
king_sim$Label <- ifelse(king_sim$y == 30, "TRUE", "FALSE")

# 绘制散点图：正确次数 (Y != 30) 部分，用黑色表示
options(repr.plot.width=10, repr.plot.height=6) #自定义画布大小
ggplot2::ggplot(king_sim, aes(x = ACC, y = y, color = Label)) +
  geom_point(data = subset(king_sim, y != 30), size = 1, alpha = 0.5) +  # 黑色点
  geom_point(data = subset(king_sim, y == 30), size = 2) +              # 蓝色点
  labs(x = "ACC",y = "Y") +
  scale_color_manual(values = c("TRUE" = "blue", "FALSE" = "black"),
                     name = "y == 30",    # 图例标题
                     labels = c("TRUE", "FALSE")) + # 图例标签
  papaja::theme_apa() +
  theme(legend.position = "right")  # 图例位置

# 从模拟数据中筛选出 y 值为 30 的样本，生成对应的后验分布
king_posterior <- king_sim %>% filter(y == 30)

# 绘制分布图：概率密度和柱状图
options(repr.plot.width=8, repr.plot.height=6) #自定义画布大小
ggplot2::ggplot(king_posterior, aes(x = ACC)) +
  geom_histogram(aes(y = ..density..), bins = 12, fill = "lightblue", alpha = 0.6, color = "black") +
  geom_density(color = "blue", size = 1) +
  labs(x = "ACC", y = "Density", title = "Posterior Distribution of ACC (y = 30)") +
  papaja::theme_apa()

# 设置 x 轴范围 [0,1]
x <- seq(0, 1, length.out = 10000)

# 设置 Beta 分布参数
a <- 45
b <- 55

# 形成先验分布 
prior <- dbeta(x, shape1 = a, shape2 = b) / sum(dbeta(x, shape1 = a, shape2 = b))

# 形成似然
k <- 30     # k 代表正确率为1的次数
n <- 50     # n 代表总次数
likelihood <- dbinom(k, size = n, prob = x)

# 计算后验
unnorm_posterior <- prior * likelihood                  # 计算分子
posterior <- unnorm_posterior / sum(unnorm_posterior)  # 结合分母进行计算
likelihood <- likelihood / sum(likelihood)               # 归一化似然以便于可视化

# 创建数据框以方便 ggplot 绘图
plot_data <- data.frame(
  ACC = x,
  Prior = prior,
  Likelihood = likelihood,
  Posterior = posterior
)

# 绘图
options(repr.plot.width=14, repr.plot.height=6) #自定义画布大小
ggplot2::ggplot(plot_data, aes(x = ACC)) +
  geom_line(aes(y = Prior, color = "Prior"), alpha = 0.5, size = 1, linetype = "solid") +
  geom_line(aes(y = Likelihood, color = "Likelihood"), alpha = 0.5, size = 1, linetype = "solid") +
  geom_line(aes(y = Posterior, color = "Posterior"), alpha = 0.5, size = 1, linetype = "solid") +
  geom_area(aes(y = Prior), fill = "#f0e442", alpha = 0.5) +
  geom_area(aes(y = Likelihood), fill = "#0071b2", alpha = 0.5) +
  geom_area(aes(y = Posterior), fill = "#009e74", alpha = 0.5) +
  labs(title = "Prior, Likelihood, and Posterior Distributions", x = "ACC", y = "Density",color = NULL) +
  scale_y_continuous(expand = c(0, 0)) + 
  papaja::theme_apa() +
  theme(legend.position = "right",
        legend.text = element_text(size = 14)) +  
  scale_color_manual(values = c("Prior" = "#f0e442", "Likelihood" = "#0071b2", "Posterior" = "#009e74"))

cat("近似值：均值 =", 
    mean(king_posterior$ACC), 
    "；标准差 =", 
    sd(king_posterior$ACC))

# 观测到的 Y = 30 数据匹配的次数
cat("10,000次模拟中,", nrow(king_posterior), "次与观测到的 Y = 30 数据匹配\n")

# 模拟新的数据
size <- 50000  # 不同于之前的 10,000
king_sim2 <- data.frame(ACC = rbeta(size, shape1 = 45, shape2 = 55))  # 创建新的 ACC 数据
king_sim2$y <- rbinom(size, size = 50, prob = king_sim2$ACC)            # 创建对应的 y 数据
king_posterior2 <- king_sim2[king_sim2$y == 30, ]                     # 筛选出 y = 30 的数据

# 新的匹配次数
cat("50,000次模拟中,", nrow(king_posterior2), "次与观测到的 Y = 30 数据匹配\n")


# 设置 x 轴范围
x <- seq(0, 1, length.out = 1000)

# 定义 Beta 参数
params <- list(
  list(alpha = 70, beta = 30),
  list(alpha = 7, beta = 3),
  list(alpha = 14, beta = 6)
)

# 创建一个空的数据框用于存放各个Beta分布的结果
data <- data.frame()

# 计算各个 Beta 分布的概率密度函数
for (p in params) {
  dist <- data.frame(
    x = x,
    y = dbeta(x, shape1 = p$alpha, shape2 = p$beta),
    label = paste("Beta(", p$alpha, ",", p$beta, ")", sep = "")
  )
  data <- rbind(data, dist)
}

# 绘图
options(repr.plot.width=14, repr.plot.height=6) #自定义画布大小
ggplot2::ggplot(data, aes(x = x, y = y, color = label)) +
  geom_line(size = 1) +
  labs(x = "ACC", y = "Density", title = "Beta Distributions") +
  papaja::theme_apa() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 14)) + 
  scale_color_manual(values = c("indianred3", "chartreuse2", "cyan4"))  # 自定义颜色

