# 安装和加载包
options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
if (!requireNamespace('pacman', quietly = TRUE)) {
  install.packages('pacman')
}
pacman::p_load("tidyverse","ggplot2", "dplyr","gridExtra","papaja")
options(warn = -1)  # 抑制警告

# 本例数据来自 Evans et al. (2020) 随机点运动任务 5% 一致性条件：
# 取 30 个正确试次 + 20 个错误试次，共 50 个试次
# （下文所有分析固定使用 n = 50, y = 30，不再依赖原始数据文件）

# 创建离散先验分布数据
prior_disc <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0) # 离散先验分布的取值
prior_dis_prob <- c(0, 0, 0, 0, 0, 0.1, 0.8, 0.1, 0, 0)         # 对应的概率
prior_disc_data <- data.frame(theta = prior_disc, f_theta = prior_dis_prob)

# 将'theta'列的数据类型转换为字符型，以便在图表中正确显示
prior_disc_data$theta <- as.character(prior_disc_data$theta)

# 创建一个单独的图表
options(repr.plot.width=8, repr.plot.height=5) #自定义画布大小
ggplot2::ggplot(prior_disc_data, aes(x = theta, y = f_theta)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Discrete Prior", x = expression(theta), y = expression(f(theta))) +
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
prior_disc_data <- data.frame(theta = prior_disc, f_theta = prior_dis_prob)

# 将'theta'列的数据类型转换为字符型，以便在图表中正确显示
prior_disc_data$theta <- as.character(prior_disc_data$theta)
# 绘制离散先验分布的线图
p1 <- ggplot2::ggplot(prior_disc_data, aes(x = theta, y = f_theta)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Discrete Prior\n", x = expression(theta), y = expression(f(theta))) +
  scale_y_continuous(expand = c(0, 0)) + 
  scale_x_discrete(expand = c(0,0)) +
  papaja::theme_apa()

# 绘制连续先验分布的线图
# 创建 Beta 分布的数据
x_values <- seq(0, 1, length.out = 1000)
y_values <- dbeta(x_values, shape1 = 70, shape2 = 30)

# 绘制 Beta 分布的 PDF
p2 <- ggplot2::ggplot(data.frame(x = x_values, y = y_values),aes(x = x, y = y)) +
  geom_line(col = 'cyan4', linewidth = 1) +
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
  geom_line(stat = "identity", color = "cyan4", linetype = 3, linewidth = 1) +
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
  y_label <- ifelse(i %in% c(1, 4, 7), "f(y|θ)", "")
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
  y_label <- ifelse(i %in% c(1, 4, 7), "f(y|θ)", "")
  x_label <- ifelse(i %in% c(7, 8, 9), "y", "")
  plots[[i]] <- ggplot2::ggplot(data.frame(y = y, prob = probs[,i]), aes(x = y, y = prob)) +
    geom_segment(aes(xend = y, yend = 0), color = "gray", linewidth = 1) +
    geom_segment(x = 30, y = 0, xend = 30, yend = y_1, color = "black", linewidth = 1) +
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
likelihood <- function(theta, Y = 30, N = 50) {
  choose(N, Y) * (theta^Y) * ((1 - theta)^(N - Y))
}

# 定义 theta 的取值范围 [0, 1]
theta_values <- seq(0, 1, length.out = 1000)

# 计算每个 theta 对应的似然值
likelihood_values <- likelihood(theta_values)

# 创建一个数据框以绘图
data <- data.frame(theta = theta_values, Likelihood = likelihood_values)

# 绘制似然函数
options(repr.plot.width=10, repr.plot.height=5) #自定义画布大小
ggplot2::ggplot(data, aes(x = theta, y = Likelihood)) +
  geom_line(aes(color = "Likelihood L(θ|Y=30)")) +  
  geom_vline(aes(xintercept = 0.6, color = 'θ=0.6 (Max)'), linetype = 'dashed') + 
  labs(
    title = "Likelihood Function",
    x = expression(theta),
    y = "Likelihood",
    color = NULL) +
  papaja::theme_apa() +
  theme(legend.position = "right",
        legend.text = element_text(size = 14)) +
  scale_color_manual(values = c("θ=0.6 (Max)" = "red", "Likelihood L(θ|Y=30)" = "grey"))

# 定义正确率的取值范围
theta_values <- seq(0, 1, length.out = 1000)

# 定义先验分布 Beta(70, 30)
alpha_prior <- 70
beta_prior <- 30
prior_distribution <- dbeta(theta_values, shape1 = alpha_prior, shape2 = beta_prior)

# 定义似然分布 Bin(50, theta)
n_trials <- 50
y_observed <- 30
likelihood <- dbinom(y_observed, size = n_trials, prob = theta_values)

# 缩放似然分布
likelihood_scaled <- likelihood / max(likelihood) * max(prior_distribution)

# 创建绘图数据框
plot_data <- data.frame(
  theta = theta_values,
  Prior = prior_distribution,
  Likelihood_Scaled = likelihood_scaled
)

# 绘图
options(repr.plot.width=10, repr.plot.height=5) #自定义画布大小
ggplot2::ggplot(plot_data, aes(x = theta)) +
  geom_area(aes(y = Prior, fill = "Prior"), alpha = 0.6, linewidth = 1, show.legend = TRUE) +
  geom_area(aes(y = Likelihood_Scaled, fill = "Likelihood"), alpha = 0.6, linewidth = 1, show.legend = TRUE) +
  labs(x = expression(theta), y = "Density", fill = NULL) +
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
prior <- dbeta(x, shape1 = a, shape2 = b)                # 真密度（积分为 1）

# 形成似然
k <- 30     # k 代表正确率为1的次数
n <- 50     # n 代表总次数
likelihood <- dbinom(k, size = n, prob = x)

# 计算后验：Beta 是 Binomial 的共轭先验，后验解析式为 Beta(a + k, b + n - k)
likelihood <- likelihood / max(likelihood) * max(prior)      # 缩放到与先验同高，便于比较
posterior <- dbeta(x, shape1 = a + k, shape2 = b + n - k)    # 即 Beta(100, 50)

# 创建数据框以方便 ggplot 绘图
plot_data <- data.frame(
  theta = x,
  Prior = prior,
  Likelihood = likelihood,
  Posterior = posterior
)

# 绘图
options(repr.plot.width=14, repr.plot.height=6) #自定义画布大小
ggplot2::ggplot(plot_data, aes(x = theta)) +
  geom_line(aes(y = Prior, color = "Prior"), alpha = 0.5, linewidth = 1, linetype = "solid") +
  geom_line(aes(y = Likelihood, color = "Likelihood"), alpha = 0.5, linewidth = 1, linetype = "solid") +
  geom_line(aes(y = Posterior, color = "Posterior"), alpha = 0.5, linewidth = 1, linetype = "solid") +
  geom_area(aes(y = Prior), fill = "#f0e442", alpha = 0.5) +
  geom_area(aes(y = Likelihood), fill = "#0071b2", alpha = 0.5) +
  geom_area(aes(y = Posterior), fill = "#009e74", alpha = 0.5) +
  labs(title = "Prior, Likelihood, and Posterior Distributions", x = expression(theta), y = "Density",color = NULL) +
  scale_y_continuous(expand = c(0, 0)) + 
  papaja::theme_apa() +
  theme(legend.position = "right",
        legend.text = element_text(size = 14)) +
  scale_color_manual(values = c("Prior" = "#f0e442", "Likelihood" = "#0071b2", "Posterior" = "#009e74"))

# 设置随机种子，以便后续可以重复结果
set.seed(84735)

# 模拟 10000 次数据
n_simulations <- 10000
king_sim <- data.frame(theta = rbeta(n_simulations, shape1 = 70, shape2 = 30))  # 从 Beta(70,30) 先验中模拟 10,000 个 theta 值
king_sim$y <- rbinom(n_simulations, size = 50, prob = king_sim$theta)  # 从每个 theta 值中模拟 Bin(50, theta) 的潜在正确判断次数 Y

# 显示部分数据
head(king_sim)

#创建一个新的变量用以区分
king_sim$Label <- ifelse(king_sim$y == 30, "TRUE", "FALSE")

# 绘制散点图：正确次数 (Y != 30) 部分，用黑色表示
options(repr.plot.width=10, repr.plot.height=6) #自定义画布大小
ggplot2::ggplot(king_sim, aes(x = theta, y = y, color = Label)) +
  geom_point(data = subset(king_sim, y != 30), size = 1, alpha = 0.5) +  # 黑色点
  geom_point(data = subset(king_sim, y == 30), size = 2) +              # 蓝色点
  labs(x = expression(theta),y = "Y") +
  scale_color_manual(values = c("TRUE" = "blue", "FALSE" = "black"),
                     name = "y == 30",    # 图例标题
                     labels = c("TRUE", "FALSE")) + # 图例标签
  papaja::theme_apa() +
  theme(legend.position = "right")  # 图例位置

# 从模拟数据中筛选出 y 值为 30 的样本，生成对应的后验分布
king_posterior <- king_sim %>% filter(y == 30)

# 绘制分布图：概率密度和柱状图
options(repr.plot.width=8, repr.plot.height=6) #自定义画布大小
ggplot2::ggplot(king_posterior, aes(x = theta)) +
  geom_histogram(aes(y = after_stat(density)), bins = 12, fill = "lightblue", alpha = 0.6, color = "black") +
  geom_density(aes(color = "Simulation"), linewidth = 1) +
  stat_function(fun = dbeta, args = list(shape1 = 100, shape2 = 50),
                aes(color = "Beta(100, 50)"), linewidth = 1.2) +
  scale_color_manual(values = c("Simulation" = "blue", "Beta(100, 50)" = "#009e74")) +
  labs(x = expression(theta), y = "Density",
       title = "Posterior Distribution of θ (y = 30)", color = NULL) +
  papaja::theme_apa()

# 设置 x 轴范围 [0,1]
x <- seq(0, 1, length.out = 10000)

# 设置 Beta 分布参数
a <- 70
b <- 30

# 形成先验分布 
prior <- dbeta(x, shape1 = a, shape2 = b)                # 真密度（积分为 1）

# 形成似然
k <- 30     # k 代表正确率为1的次数
n <- 50     # n 代表总次数
likelihood <- dbinom(k, size = n, prob = x)

# 计算后验：Beta 是 Binomial 的共轭先验，后验解析式为 Beta(a + k, b + n - k)
likelihood <- likelihood / max(likelihood) * max(prior)      # 缩放到与先验同高，便于比较
posterior <- dbeta(x, shape1 = a + k, shape2 = b + n - k)    # 即 Beta(100, 50)

# 创建数据框以方便 ggplot 绘图
plot_data <- data.frame(
  theta = x,
  Prior = prior,
  Likelihood = likelihood,
  Posterior = posterior
)

# 绘图
options(repr.plot.width=14, repr.plot.height=6) #自定义画布大小
ggplot2::ggplot(plot_data, aes(x = theta)) +
  geom_line(aes(y = Prior, color = "Prior"), alpha = 0.5, linewidth = 1, linetype = "solid") +
  geom_line(aes(y = Likelihood, color = "Likelihood"), alpha = 0.5, linewidth = 1, linetype = "solid") +
  geom_line(aes(y = Posterior, color = "Posterior"), alpha = 0.5, linewidth = 1, linetype = "solid") +
  geom_area(aes(y = Prior), fill = "#f0e442", alpha = 0.5) +
  geom_area(aes(y = Likelihood), fill = "#0071b2", alpha = 0.5) +
  geom_area(aes(y = Posterior), fill = "#009e74", alpha = 0.5) +
  labs(title = "Prior, Likelihood, and Posterior Distributions", x = expression(theta), y = "Density",color = NULL) +
  scale_y_continuous(expand = c(0, 0)) + 
  papaja::theme_apa() +
  theme(legend.position = "right",
        legend.text = element_text(size = 14)) +  
  scale_color_manual(values = c("Prior" = "#f0e442", "Likelihood" = "#0071b2", "Posterior" = "#009e74"))

cat("近似值：均值 =", 
    mean(king_posterior$theta), 
    "；标准差 =", 
    sd(king_posterior$theta))

# 观测到的 Y = 30 数据匹配的次数
cat("10,000次模拟中,", nrow(king_posterior), "次与观测到的 Y = 30 数据匹配\n")

# 模拟新的数据
size <- 50000  # 不同于之前的 10,000
king_sim2 <- data.frame(theta = rbeta(size, shape1 = 70, shape2 = 30))  # 创建新的 theta 数据
king_sim2$y <- rbinom(size, size = 50, prob = king_sim2$theta)            # 创建对应的 y 数据
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
  geom_line(linewidth = 1) +
  labs(x = expression(theta), y = "Density", title = "Beta Distributions") +
  papaja::theme_apa() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 14)) + 
  scale_color_manual(values = c("indianred3", "chartreuse2", "cyan4"))  # 自定义颜色

