# 安装和加载包
options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
if (!requireNamespace('pacman', quietly = TRUE)) {
    install.packages('pacman')
}
pacman::p_load("tidyverse","ggplot2","dplyr",'rstan',
               "BayesFactor","bayestestR","papaja","bayesplot")
options(warn = -1)  # 抑制警告

# 生成从 0.5 到 1 的 11 个值
pi_grid <- seq(0.5, 1, length.out = 11)
cat("从0.5~1内的连续变量π中取出11个值:", pi_grid, "\n")

# 计算先验、似然和后验
prior <- dbeta(pi_grid, 70, 30)
likelihood <- dbinom(90, 100, pi_grid)
posterior <- prior * likelihood / sum(prior * likelihood)

# 绘制后验分布茎状图（）
plot_data <- data.frame(pi_grid = pi_grid, posterior = posterior)
ggplot2::ggplot(plot_data, ggplot2::aes(x = pi_grid, y = posterior)) +
  ggplot2::geom_segment(ggplot2::aes(xend = pi_grid, yend = 0), linewidth = 0.5) +
  ggplot2::geom_point(size = 2) +
  ggplot2::ylim(0, 1) + # 零点与x轴重叠
  ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  ggplot2::labs(x = "pi_grid", y = "Posterior") +
  papaja::theme_apa()

# 设置随机种子
set.seed(84735)

# 从 posterior 分布中抽取 10000 个样本
posterior_sample <- sample(
  pi_grid, 
  size = 10000,
  prob = posterior,  # R中使用prob参数指定概率
  replace = TRUE
)

# 将抽取的样本存储在数据框中，列名为 "pi_sample"
posterior_sample <- data.frame(pi_sample = posterior_sample)

# 对 posterior_sample 中的样本进行计数，并转换为相对频率
result <- as.data.frame(table(posterior_sample) / nrow(posterior_sample))
colnames(result) <- c("pi_sample", "proportion")

head(result)

# 生成10000个点，范围在[0, 1]之间
x_beta <- seq(0, 1, length.out = 10000)

# 生成Beta(160, 40)分布的概率密度值
y_beta <- dbeta(x_beta, 160, 40)

# 转换为数据框用于ggplot2绘图
beta_data <- data.frame(x = x_beta, y = y_beta)

# 绘制共轭方法计算得到的后验beta(160,40)和网格方法抽样结果
options(repr.plot.width=10, repr.plot.height=5) #自定义画布大小
ggplot2::ggplot() +
  # 绘制Beta分布曲线 
  ggplot2::geom_line(
    data = beta_data,
    ggplot2::aes(x = x, y = y, color = "posterior of conjucated distribution"),
    linewidth = 1
  ) +
  # 绘制网格搜索后验样本的直方图
  ggplot2::geom_histogram(
    data = posterior_sample,  # 使用之前创建的posterior_sample数据框
    ggplot2::aes(x = pi_sample, y = ..density.., fill = "posterior of grid search"),
    color = "black",
    alpha = 0.5,
    bins = 20  # 控制直方图的箱数
  ) +
  # 统一设置填充色和线条色的图例
  ggplot2::scale_color_manual(values = "#4169E1") +
  ggplot2::scale_fill_manual(values = "#E28903") +
  ggplot2::labs(x = NULL, y = NULL, color = NULL, fill = NULL) +
  scale_y_continuous(expand = c(0, 0)) +
  papaja::theme_apa() +
  # 设置图例位置
  theme(legend.position = "right")

# 生成101个点，范围在[0, 1]之间
pi_grid <- seq(0, 1, length.out = 101)

# 生成Beta(70,30)先验分布
prior <- dbeta(pi_grid, shape1 = 70, shape2 = 30)

# 生成二项分布似然函数，参数为n=100，k=90
likelihood <- dbinom(90, size = 100, prob = pi_grid)

# 计算后验概率
unstd_posterior <- prior * likelihood
# 归一化后验概率
posterior <- unstd_posterior / sum(unstd_posterior)

# 创建数据框用于绘图
df <- data.frame(pi = pi_grid, posterior = posterior)

# 画图
ggplot2::ggplot(df, ggplot2::aes(x = pi, y = posterior)) +
  ggplot2::geom_segment(ggplot2::aes(xend = pi, yend = 0), color = "#1f77b4") + # 添加茎线
  ggplot2::geom_point(color = "#1f77b4", size = 2) +  # 添加顶部点
  papaja::theme_apa() +
  ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 0.3)) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_line(color = "black"),
    axis.line.y = ggplot2::element_line(color = "black")
  ) +
  ggplot2::labs(x = "", y = "")

set.seed(84735)

# 从 posterior 分布中抽取 10000 个样本
posterior_sample <- dplyr::tibble(
  pi_sample = sample(
    x = pi_grid,  # 待抽样的向量
    size = 10000, # 样本量
    prob = posterior,  # 每个元素被抽中的概率
    replace = TRUE  # 有放回抽样
  )
)

# 对样本进行计数并转换为相对频率
result <- posterior_sample %>%
  dplyr::count(pi_sample) %>%  # 计数
  dplyr::mutate(relative_frequency = n / sum(n)) %>%  # 计算相对频率
  dplyr::select(pi_sample, relative_frequency)  # 选择需要的列

head(result, n = 20)

# 生成10000个点，范围在[0, 1]之间
x_beta <- seq(from = 0, to = 1, length.out = 10000)

# 生成Beta(160,40)
y_beta <- stats::dbeta(x_beta, shape1 = 160, shape2 = 40)

# 创建共轭分布的数据集
conj_data <- data.frame(x = x_beta, y = y_beta, type = "posterior of conjucated distribution")

# 生成网格搜索后验样本（模拟数据）
posterior_sample <- data.frame(pi_sample = stats::rbeta(n = 10000, shape1 = 155, shape2 = 38))

# 绘图
options(repr.plot.width=10, repr.plot.height=5) #自定义画布大小
ggplot2::ggplot() +
  # 绘制共轭分布的后验曲线
  ggplot2::geom_line(
    data = conj_data,
    ggplot2::aes(x = x, y = y, color = type),
    linewidth = 1
  ) +
  # 绘制网格搜索的后验直方图
  ggplot2::geom_histogram(
    data = posterior_sample,
    ggplot2::aes(x = pi_sample, y = ..density.., fill = "posterior of grid search"),
    alpha = 0.7,  # 调整透明度
    bins = 30,    # 调整分箱数量
    color = "black"  # 直方图边框颜色
  ) +
  # 设置颜色
  ggplot2::scale_color_manual(values = "#4169E1") +
  ggplot2::scale_fill_manual(values = "#E28903") +
  ggplot2::labs(x = NULL, y = NULL, color = NULL, fill = NULL) +
  # 合并图例
  ggplot2::guides(color = ggplot2::guide_legend(order = 1),fill = ggplot2::guide_legend(order = 2)) +
  scale_y_continuous(expand = c(0, 0)) +
  papaja::theme_apa() +
  # 设置图例位置
  theme(legend.position = "right")

# 练习部分

set.seed(0)  # 设置随机种子
data <- stats::rnorm(n = 10, mean = 550, sd = 80)  # 生成正态分布数据

# 展示数据
print(data)

##---------------------------------------------------------------------------
#                            设置网格范围和步长
#                            1. 假设被试反应的反应时范围为 200 到 800 ms
#                            2. 设定网格步长为10 (后续可以修改为20,50,100等)
# ---------------------------------------------------------------------------
# theta_grid <- base::seq(from = 200, to = 800, length.out = 20)
n_step <- 200
mu_grid <- base::seq(from = 200, to = 800, length.out = n_step)


##---------------------------------------------------------------------------
#                            计算先验概率
#                            1. 设定先验概率服从正态分布
#                            2. 先验均值为500, 标准差为100
# ---------------------------------------------------------------------------
prior_mean <- 500
prior_std <- 100
prior_prob <- stats::dnorm(mu_grid, mean = prior_mean, sd = prior_std)


##---------------------------------------------------------------------------
#                            计算似然函数
# ---------------------------------------------------------------------------
likelihood <- sapply(mu_grid, function(mu) {
  # 计算每个数据点的正态密度并求乘积
  prod(stats::dnorm(data, mean = mu, sd = 80))
})


##---------------------------------------------------------------------------
#                            计算后验
# ---------------------------------------------------------------------------
posterior_prob <- prior_prob * likelihood

# 归一化后验概率
posterior_prob <- posterior_prob / sum(posterior_prob)


##---------------------------------------------------------------------------
#                            计算找到后验概率的最大值对应的参数
# ---------------------------------------------------------------------------

max_posterior <- which.max(posterior_prob)
cat("最大后验概率对应的参数值：", mu_grid[max_posterior], "\n")

# 绘制结果（直接运行即可）
options(repr.plot.width=10, repr.plot.height=5) #自定义画布大小
ggplot2::ggplot() +
  # 绘制先验分布
  ggplot2::geom_line(
    data = data.frame(mu = mu_grid, density = prior_prob / base::sum(prior_prob), type = "prior"),
    # ggplot2::aes(x = mu, y = density),
    ggplot2::aes(x = mu, y = density, fill = type),  # 在这里映射颜色
    color = "orange",
    linewidth = 0.7
  ) +
  # 绘制后验分布
  ggplot2::geom_line(
    data = data.frame(mu = mu_grid, density = posterior_prob, type = "posterior of grid method"),
    ggplot2::aes(x = mu, y = density, color = type),
    color = "blue",
    linewidth = 0.7
  ) +
  # 绘制真实数据均值的垂直线
  ggplot2::geom_vline(
    xintercept = base::mean(data),
    color = "red",
    linewidth = 0.7
  ) +
  # 添加标题和坐标轴标签
  ggplot2::ggtitle("Grid search posterior distribution") +
  ggplot2::xlab(expression(mu)) +
  ggplot2::ylab("Density") +
  # 设置坐标轴范围
  ggplot2::xlim(200, 800) +
  ggplot2::ylim(0, max(posterior_prob) * 1.1) +
  scale_y_continuous(expand = c(0, 0)) +
  papaja::theme_apa() 

##---------------------------------------------------------------------------
#                            通过共轭方法计算后验概率 (具体算法见补充材料)
# ---------------------------------------------------------------------------
x <- base::seq(from = 200, to = 800, length.out = 10000)
prior_mean <- 500
prior_variance <- 200^2  # 先验方差
sigma2 <- 80^2           # 数据方差
n <- base::length(data)  # 观测数据的数量

# 计算后验均值和标准差
posterior_mean <- (prior_mean / prior_variance + base::sum(data) / sigma2) / 
                 (1 / prior_variance + n / sigma2)
posterior_std <- base::sqrt(1 / (1 / prior_variance + n / sigma2))

# 计算共轭后验分布的概率密度
posterior_conjugate <- stats::dnorm(x, mean = posterior_mean, sd = posterior_std)

# 准备绘图数据
plot_data <- data.frame(
  mu = x,
  density = posterior_conjugate,
  type = "posterior of conjugated method"
)

# 计算数据均值（真实数据参考线）
data_mean <- base::mean(data)

# 绘制结果
ggplot2::ggplot() +
  # 绘制共轭后验分布曲线
  ggplot2::geom_line(
    data = plot_data,
    ggplot2::aes(x = mu, y = density, color = type),
    linewidth = 0.7
  ) +
  # 绘制真实数据均值的垂直线
  ggplot2::geom_vline(
    xintercept = data_mean,
    color = "red",
    linewidth = 0.7
  ) +
  # 设置颜色方案
  ggplot2::scale_color_manual(
    values = c("posterior of conjugated method" = "blue", "true data" = "red"),
    labels = c("posterior of conjugated method", "true data")
  ) +
  # 添加标题和坐标轴标签
  ggplot2::ggtitle("Conjugated posterior distribution") +
  ggplot2::xlab(expression(mu)) +
  ggplot2::ylab("Density") +
  # 设置坐标轴范围
  ggplot2::xlim(200, 800) +
  ggplot2::ylim(0, base::max(posterior_conjugate) * 1.1) +
  # 图例设置
  ggplot2::guides(color = ggplot2::guide_legend(title = NULL)) +
  # 主题设置
  scale_y_continuous(expand = c(0, 0)) +
  papaja::theme_apa() +
  # 设置图例位置
  theme(legend.position = "right")


##---------------------------------------------------------------------------
#                            设置网格范围和步长
#                            1. 假设被试反应的反应时范围为 200 到 800 ms
#                            2. 假设被试反应时的方差范围为 20 到 200
# ---------------------------------------------------------------------------
n_step <- 20
mean_grid <- seq(200, 800, length.out = n_step)  
std_grid <- seq(20, 200, length.out = n_step)  

# 生成网格数据
grid_data <- expand.grid(mean = mean_grid, std = std_grid)
mean_mesh <- matrix(grid_data$mean, nrow = n_step, ncol = n_step)
std_mesh <- matrix(grid_data$std, nrow = n_step, ncol = n_step)


##---------------------------------------------------------------------------
#                            计算先验概率
#                            1. 设定先验概率服从正态分布
#                            2. 先验均值为500，标准差为100
# ---------------------------------------------------------------------------

# 设置先验概率的参数
prior_mean_mean <- 500
prior_mean_std <- 200       
prior_std_mean <- 100
prior_std_std <- 50

mean_mesh <- seq(0, 1000, length.out = 20)  # 均值网格
std_mesh <- seq(0, 200, length.out = 20)   # 标准差网格

# 计算先验概率（正态分布密度）
prior_mean <- dnorm(mean_mesh, mean = prior_mean_mean, sd = prior_mean_std)
prior_std <- dnorm(std_mesh, mean = prior_std_mean, sd = prior_std_std)

# 创建先验概率网格（外积）
prior_grid <- outer(prior_mean, prior_std, FUN = "*")

# 显示prior_grid的维度
dim(prior_grid)


##---------------------------------------------------------------------------
#                            计算似然函数
#                            1. 先计算一种参数条件下的似然值
#                            2. 通过for循环计算所有参数条件下的似然值，并储存在likelihood_grid中
# ---------------------------------------------------------------------------

# 初始化似然网格
likelihood_grid <- matrix(0, nrow = n_step, ncol = n_step)

# 填充似然网格
for (mean_index in 1:n_step) {
  mean <- mean_grid[mean_index]
  for (std_index in 1:n_step) {
    std <- std_grid[std_index]
    # 计算每个数据点的正态密度并求乘积
    likelihood_i <- prod(stats::dnorm(data, mean = mean, sd = std))
    likelihood_grid[mean_index, std_index] <- likelihood_i
  }
}

# 查看网格维度
dim(likelihood_grid)


##---------------------------------------------------------------------------
#                            计算grid的后验概率
# ---------------------------------------------------------------------------

posterior_grid <- prior_grid * likelihood_grid

# 归一化
posterior_grid <- posterior_grid / sum(posterior_grid)

# 显示 posterior_grid 的形状
dim(posterior_grid)


##---------------------------------------------------------------------------
#                            计算找到后验概率的最大值对应的参数
# ---------------------------------------------------------------------------

# 找到posterior_grid中最大值的索引
max_idx <- which(posterior_grid == max(posterior_grid), arr.ind = TRUE)

# 提取估计的均值和标准差
estimated_mean <- mean_grid[max_idx[1, 1]]
estimated_std <- mean_grid[max_idx[1, 2]]

# 打印结果
cat(sprintf("Estimated Mean: %f\n", estimated_mean))
cat(sprintf("Estimated Standard Deviation: %f\n", estimated_std))

# 绘制后验概率分布图
# 创建数据框，包含三种不同类型的点及其坐标
plot_data <- data.frame(
  mean = c(prior_mean_mean, mean(data), estimated_mean),
  std = c(prior_std_mean, sd(data), estimated_std),
  type = factor(c("prior", "data", "max_posterior"), 
                levels = c("prior", "data", "max_posterior"))
)

# 绘制散点图
ggplot2::ggplot(plot_data, aes(x = mean, y = std, color = type)) +
  ggplot2::geom_point(size = 3) +  # 添加散点
  ggplot2::scale_color_manual(values = c("orange", "black", "red")) +  # 设置颜色
  ggplot2::xlab("Mean") +  # x轴标签
  ggplot2::ylab("Standard Deviation") +  # y轴标签
  ggplot2::ggtitle("Posterior Distribution") +  # 图表标题
  ggplot2::xlim(400, 800) +  # x轴范围
  ggplot2::ylim(20, 200) +  # y轴范围
  papaja::theme_apa() +
  # 设置图例位置
  ggplot2::theme(legend.position = "right") +
  ggplot2::guides(color = guide_legend(title = NULL))  # 移除图例标题

# stan模型示例

# 生成模拟数据
n_trials <- 100
n_successes <- 90

# Stan 模型
stan_model_code <- "
data {
  int<lower=0> n_trials;       // 试验次数
  int<lower=0, upper=n_trials> n_successes;  // 成功次数
}

parameters {
  real<lower=0, upper=1> p;    // 成功概率参数
}

model {
  // 设置先验
  p ~ beta(70, 30);
  
  // 设置似然
  n_successes ~ binomial(n_trials, p);
}
"

# 准备数据
stan_data <- list(
  n_trials = n_trials,
  n_successes = n_successes
)

# 拟合模型
fit <- stan(
  model_code = stan_model_code,   # 模型文件路径
  data = stan_data,                  # 输入数据
  chains = 4,                        # 马尔可夫链数量
  iter = 10000,                      # 总迭代次数（每个链）
  warmup = 5000,                     # 热身迭代次数（不保存）
  cores = 4                          # 使用的CPU核心数
)

# 显示采样结果
print(fit)

# 绘制后验分布
idata <- rstan::extract(fit)
posterior_df <- data.frame(p = idata$p)

#后验分布图
ggplot2::ggplot(posterior_df, aes(x = p)) +
  ggplot2::geom_density(fill = "orange", alpha = 0.5) +
  ggplot2::labs(title = "Posterior", x = " ", y = " ") +
  ggplot2::scale_y_continuous(expand = c(0, 0)) +
  papaja::theme_apa()

# 设置参数
alpha_prior <- 70
beta_prior <- 30

# 计算后验Beta分布参数
alpha_posterior <- alpha_prior + n_successes
beta_posterior <- beta_prior + (n_trials - n_successes)

# 生成x轴数据
x <- seq(0.5, 1, length.out = 100)

# 创建先验和后验分布的数据框
prior_data <- data.frame(
  x = x,
  density = stats::dbeta(x, alpha_prior, beta_prior),
  type = paste0("Prior Beta(", alpha_prior, ",", beta_prior, ")")
)

posterior_conj_data <- data.frame(
  x = x,
  density = stats::dbeta(x, alpha_posterior, beta_posterior),
  type = paste0("Posterior Beta(", alpha_posterior, ",", beta_posterior, ") from conjugated prior")
)

# 合并数据
plot_data <- rbind(prior_data, posterior_conj_data)

# 创建基础绘图
p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = density, color = type, linetype = type)) +
  ggplot2::geom_line(size = 1) +
  ggplot2::scale_color_manual(values = c("green", "red")) +
  ggplot2::scale_linetype_manual(values = c("solid", "dashed")) +
  ggplot2::labs(title = "Posterior results", x = NULL, y = NULL) +
  ggplot2::scale_y_continuous(expand = c(0, 1)) +
  papaja::theme_apa() +
  ggplot2::theme(legend.position = "right") # +
  # ggplot2::guides(color = guide_legend(title = NULL))  # 移除图例标题

# 从模型中提取后验样本并添加到图中
posterior_samples <- posterior_df  # 假设该数据框已在其他地方定义

# 添加stan模型的后验分布
p <- p + ggplot2::stat_density(
  data = posterior_samples, 
  ggplot2::aes(x = p, y = ..density..),
  color = "blue", 
  geom = "line", 
  size = 1, 
  inherit.aes = FALSE
)

# 计算HDI并添加到图中
hdi <- bayestestR::hdi(posterior_samples$p, ci = 0.94)
mean_p <- mean(posterior_samples$p)
hdi_low <- hdi[[1]]  # 下限值
hdi_high <- hdi[[2]]  # 上限值

# 添加HDI区间
p <- p + 
  ggplot2::annotate("segment", x = hdi_low, xend = hdi_high, 
           y = 0, yend = 0, color = "black", size = 2) +
  ggplot2::annotate("text", x = mean(c(hdi_low, hdi_high)), y = 0.5, label = "94% HDI", 
           color = "black", size = 3.5) +
  ggplot2::annotate("text", x = hdi_low, y = -0.3, label = round(hdi_low, 2), 
           color = "black", size = 3.5) +
  ggplot2::annotate("text", x = hdi_high, y = -0.3, label = round(hdi_high, 2), 
           color = "black", size = 3.5) +
  ggplot2::annotate("text", x = mean_p, y = max(stats::dbeta(x, alpha_posterior, beta_posterior)) + 1, 
           label = paste0("mean=", round(mean_p, 2)), 
           color = "blue", size = 4)

print(p)

# 定义正确率范围
x <- seq(0, 1, length.out = 10000)  # 正确率在0到1之间

# 定义先验分布 (基于文献，正确率均值为70%)
prior_mean <- 0.70
prior_std <- 0.05
prior_y <- dnorm(x, mean = prior_mean, sd = prior_std)
prior_y <- prior_y / sum(prior_y)  # 归一化

# 生成似然分布 (基于新实验数据，正确率均值为75%)
likelihood_mean <- 0.75
likelihood_std <- 0.05
likelihood_values <- dnorm(x, mean = likelihood_mean, sd = likelihood_std)
likelihood_values <- likelihood_values / sum(likelihood_values)  # 归一化

# 计算后验分布
posterior_mean <- (prior_mean * likelihood_std^2 + likelihood_mean * prior_std^2) / 
  (prior_std^2 + likelihood_std^2)
posterior_std <- sqrt((prior_std^2 * likelihood_std^2) / (prior_std^2 + likelihood_std^2))
posterior <- dnorm(x, mean = posterior_mean, sd = posterior_std)
posterior <- posterior / sum(posterior)  # 归一化

# 创建数据框用于绘图
plot_data <- data.frame(
  x = rep(x, 3),
  density = c(prior_y, likelihood_values, posterior),
  distribution = factor(rep(c("prior", "likelihood", "posterior"), each = length(x)),
                        levels = c("prior", "likelihood", "posterior"))
)

# 绘制图形
ggplot(plot_data, aes(x = x, y = density, color = distribution, fill = distribution)) +
  geom_line(size = 1) +
  geom_area(alpha = 0.5, position = "identity") +
  scale_color_manual(values = c("#f0e442", "#0071b2", "#009e74")) +
  scale_fill_manual(values = c("#f0e442", "#0071b2", "#009e74")) +
  labs(x = expression(mu ~ "for accuracy (correct response rate)"),y = "density") +
  scale_y_continuous(expand = c(0, 0)) +
  papaja::theme_apa() +
  # 设置图例位置
  theme(legend.position = "right")
  