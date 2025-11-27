# 安装和加载包
options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
if (!requireNamespace('pacman', quietly = TRUE)) {
    install.packages('pacman')
}
pacman::p_load("tidyverse","ggplot2", "dplyr","gridExtra","papaja", "patchwork","bayesplot",
               "rstan","bridgesampling", 'logspline', "easystats", "loo") 
options(warn = -1)  # 抑制警告

# 导入数据
df_raw <- tryCatch({
  read.csv('/home/mw/input/bayes3797/Kolvoort_2020_HBM_Exp1_Clean.csv')
}, error = function(e) {
  read.csv('data/Kolvoort_2020_HBM_Exp1_Clean.csv')
})
# 显示数据前几行
head(df_raw)   

# 数据分组和计算均值
df <- df_raw %>%
  dplyr::group_by(Subject, Label, Matching) %>%
  dplyr::summarize(RT_sec = mean(RT_sec)) %>%
  dplyr::ungroup() %>%
# 将 Label 列的数字编码转为文字标签
  dplyr::mutate(Label = case_when(
    Label == '1' ~ "Self",
    Label == '2' ~ "Friend",
    Label == '3' ~ "Stranger"
  )) %>%
# 替换 Matching 列的值为小写标签
  dplyr::mutate(Matching = ifelse(Matching == "Matching", 'matching','nonmatching')) %>%
# 设置索引
  dplyr::mutate(index = row_number()) %>%
  # 设置索引为新创建的命名行
  tibble::column_to_rownames(var = "index")  %>%
  # 将 Label 列转换为有序的分类变量
  dplyr::mutate(Label = factor(Label, levels = c('Self', 'Friend', 'Stranger'), ordered = TRUE))

# 将分类变量转换为哑变量
X1 <- as.integer(df$Label == 'Friend')
X2 <- as.integer(df$Label == 'Stranger')

head(df)

# 定义模型1
model_code1 <- "
data {
  int<lower=0> N;            // 数据点数量
  vector[N] y;               // 观测数据
  vector[N] X1;              // Friend 
  vector[N] X2;              // Stranger 
}

parameters {
  real beta_0;                   // 截距
  real beta_1;                   // Friend 的斜率
  real beta_2;                   // Stranger 的斜率
  real<lower=0> sigma;           // 误差标准差
}

model {
  beta_0 ~ normal(5, 2);         // 截距的先验分布
  beta_1 ~ normal(0, 1);         // Friend 的斜率的先验分布
  beta_2 ~ normal(0, 1);         // Stranger 的斜率的先验分布
  sigma ~ exponential(0.3);      // 误差标准差的先验分布
  // 似然函数
  y ~ normal(beta_0 + beta_1 * X1 + beta_2 * X2, sigma);
}

generated quantities {
  real y_rep[N];             // 后验预测
  vector[N] log_lik;         // 逐点对数似然
 
  for (n in 1:N) {
    // 后验预测值
    y_rep[n] = normal_rng(beta_0 + beta_1 * X1[n] + beta_2 * X2[n], sigma);
    // 逐点对数似然
    log_lik[n] = normal_lpdf(y[n] | beta_0 + beta_1 * X1[n] + beta_2 * X2[n], sigma);
  }
}
"

# 准备数据列表
data_list1 <- list(
  N = nrow(df),
  y = df$RT_sec,
  X1 = X1,
  X2 = X2
)

# 转换分类变量为哑变量
Matching <- as.integer(df$Matching == 'matching')

# 定义模型2（
model_code2 <- "
data {
  int<lower=0> N;             // 样本数量
  vector[N] y;                // 响应变量
  vector[N] X1;               // 哑变量1（Friend）
  vector[N] X2;               // 哑变量2（Stranger）
  vector[N] Matching;         // 哑变量（Matching条件）
}

parameters {
  real beta_0;                // 截距
  real beta_1;                // Friend的主效应
  real beta_2;                // Stranger的主效应
  real beta_3;                // Matching的主效应
  real<lower=0> sigma;        // 误差项的标准差
}

model {
  // 先验分布
  beta_0 ~ normal(5, 2);
  beta_1 ~ normal(0, 1);
  beta_2 ~ normal(0, 1);
  beta_3 ~ normal(0, 1);
  sigma ~ exponential(0.3);
  
  // 似然函数
  y ~ normal(beta_0 + beta_1 * X1 + beta_2 * X2 + beta_3 * Matching, sigma);
}
generated quantities {
  real y_rep[N];              // 后验预测值
  vector[N] log_lik;          // 逐点对数似然

  for (n in 1:N) {
    // 后验预测值
    y_rep[n] = normal_rng(beta_0 + beta_1 * X1[n] + beta_2 * X2[n] + beta_3 * Matching[n], sigma);
    // 逐点对数似然
    log_lik[n] = normal_lpdf(y[n] | beta_0 + beta_1 * X1[n] + beta_2 * X2[n] + beta_3 * Matching[n], sigma);
  }
}
"

# 准备数据列表
data_list2 <- list(
  N = nrow(df),
  y = df$RT_sec,
  X1 = X1,
  X2 = X2,
  Matching = Matching
)

# 准备数据并转换分类变量为哑变量
Matching <- as.integer(df$Matching == 'matching')
Interaction_1 <- X1 * Matching
Interaction_2 <- X2 * Matching

# 定义模型3（
model_code3 <- 
"
data {
  int<lower=0> N;                 // 样本数量
  vector[N] y;                    // 响应变量
  vector[N] X1;                   // 哑变量1（Friend）
  vector[N] X2;                   // 哑变量2（Stranger）
  vector[N] Matching;             // 哑变量（Matching条件）
  vector[N] Interaction_1;        // Friend 和 Matching 的交互
  vector[N] Interaction_2;        // Stranger 和 Matching 的交互
}

parameters {
  real beta_0;                    // 截距
  real beta_1;                    // Friend的主效应
  real beta_2;                    // Stranger的主效应
  real beta_3;                    // Matching的主效应
  real beta_4;                    // Friend与Matching的交互效应
  real beta_5;                    // Stranger与Matching的交互效应
  real<lower=0> sigma;            // 误差项的标准差
}

model {
  // 先验分布
  beta_0 ~ normal(5, 2);
  beta_1 ~ normal(0, 1);
  beta_2 ~ normal(0, 1);
  beta_3 ~ normal(0, 1);
  beta_4 ~ normal(0, 1);
  beta_5 ~ normal(0, 1);
  sigma ~ exponential(0.3);
  
  // 似然函数
  y ~ normal(beta_0 + beta_1 * X1 + beta_2 * X2 + beta_3 * Matching + 
  beta_4 * Interaction_1 + beta_5 * Interaction_2, sigma);
}
generated quantities {
  real y_rep[N];                  // 后验预测值
  vector[N] log_lik;              // 逐点对数似然

  for (n in 1:N) {
    // 后验预测值（原有）
    y_rep[n] = normal_rng(beta_0 + beta_1 * X1[n] + beta_2 * X2[n] + beta_3 * Matching[n] +
                          beta_4 * Interaction_1[n] + beta_5 * Interaction_2[n], sigma);
    // 逐点对数似然（原有）
    log_lik[n] = normal_lpdf(y[n] | beta_0 + beta_1 * X1[n] + beta_2 * X2[n] + beta_3 * Matching[n] + 
                            beta_4 * Interaction_1[n] + beta_5 * Interaction_2[n], sigma);
  }
}
"

# 准备数据列表
data_list3 <- list(
  N = nrow(df),
  y = df$RT_sec,
  X1 = X1,
  X2 = X2,
  Matching = Matching,
  Interaction_1 = Interaction_1,
  Interaction_2 = Interaction_2
)

# 定义函数，对模型进行采样
run_stan_sampling <- function(save_name, model_code = NULL, data_list = NULL, 
                              iter = 3000, warmup = 1000, chains = 4, 
                              thin = 1, seed = 84735) {
  # 运行Stan模型采样，存在结果文件时直接加载，否则执行采样并保存。
  # 
  # Parameters:
  # - save_name: 保存/加载结果的文件名（无扩展名）
  # - model_code: Stan模型代码字符串（仅采样时需提供）
  # - data_list: Stan模型数据列表（仅采样时需提供）
  # - iter: 总迭代次数（默认3000，含warmup）
  # - warmup: 预热迭代次数（默认1000，将被丢弃）
  # - chains: 采样链数（默认4）
  # - thin: 采样 thinning 间隔（默认1）
  # - seed: 随机种子（默认84735）
  # 
  # Returns:
  # - fit: Stan采样结果对象
  
  # 定义保存文件路径（.rds格式，R标准二进制格式）
  rds_file <- base::paste0(save_name, ".rds")
  
  # 检查文件是否存在
  if (base::file.exists(rds_file)) {
    base::cat(base::sprintf("加载现有的采样结果：%s\n", rds_file))
    fit <- base::readRDS(rds_file)
  } else {
    # 校验模型代码和数据是否提供
    if (base::is.null(model_code) || base::is.null(data_list)) {
      base::stop("模型未定义或数据缺失，请提供model_code和data_list")
    }
    
    base::cat(base::sprintf("未找到现有结果，正在执行采样：%s\n", save_name))
    # 执行Stan采样
    fit <- rstan::stan(
      model_code = model_code,
      data = data_list,
      iter = iter,
      chains = chains,
      warmup = warmup,
      thin = thin,
      seed = seed
    )
    
    # 保存采样结果到RDS文件
    base::saveRDS(fit, file = rds_file)
    base::cat(base::sprintf("采样结果已保存至：%s\n", rds_file))
  }
  
  return(fit)
}

# 运行模型1采样
model1_fit <- run_stan_sampling(save_name = "lec11_model1", model_code = model_code1, data_list = data_list1)

# 运行模型2采样
model2_fit <- run_stan_sampling(save_name = "lec11_model2", model_code = model_code2, data_list = data_list2)

# 运行模型3采样
model3_fit <- run_stan_sampling(save_name = "lec11_model3", model_code = model_code3, data_list = data_list3)

# 构建设计矩阵
# 先构建自变量数据框
X_df <- data.frame(
  X1 = X1,
  X2 = X2,
  Matching = Matching,
  Interaction_1 = Interaction_1,
  Interaction_2 = Interaction_2
)

# 添加截距项（
X <- stats::model.matrix(~ ., data = X_df)  # ~ . 表示包含所有自变量，自动添加截距列(Intercept)

# 定义因变量
y <- df$RT_sec

# 传统线性回归模型
model0 <- stats::lm(
  formula = y ~ X - 1,  # -1 避免重复添加截距（model.matrix已生成截距列）
  data = data.frame(y = y, X = X)  # 合并因变量和设计矩阵为数据框
)

# 查看回归结果
summary(model0)

head(df)

head(X)

contrasts(df$Matching)

contrasts(df$Label) = contr.treatment(3)
contrasts(df$Label)

model0_alt <- stats::lm(
    formula = RT_sec ~ Label * Matching,
    data = df
)

summary(model0_alt)

# 注意，可以使用bayestestR包中的bayesfactor_parameters函数计算贝叶斯因
summary(model3_fit, par=c("beta_0","beta_1","beta_2","beta_3","beta_4","beta_5","sigma"))$summary 

# 从采样结果中提取后验预测样本
posterior_predictive <- tidybayes::spread_draws(model1_fit, y_rep[i])

# 在chain和draw维度上计算后验均值
posterior_mean <- posterior_predictive %>%
  dplyr::group_by(i) %>%  # 按每个观测值分组
  dplyr::summarise(
    posterior_mean = mean(y_rep, na.rm = TRUE),  # 计算每个观测值的后验预测均值
    .groups = "drop"
  ) %>%
  dplyr::rename(observation = i)  # 重命名索引列为"observation"

head(posterior_mean, n = 5)

# 合并原始观测值与后验均值
combined_data <- dplyr::tibble(
  observed = df$RT_sec,          # 原始观测值（RT_sec）
  predicted = posterior_mean$posterior_mean  # 后验预测均值
)

# 计算MAE（观测值和后验均值的绝对误差的中位数）
mae <- stats::median(
  abs(combined_data$observed - combined_data$predicted),  # 绝对误差
  na.rm = TRUE  # 忽略可能的缺失值
)

base::cat(sprintf("MAE: %.4f\n", mae)) 

calculate_mae <- function(trace, observed_data) {
  # 从stanfit对象中提取所有后验预测样本（适用于任何变量名）
  # 提取后验预测样本矩阵（行=迭代×链，列=观测值）
  posterior_pred <- rstan::extract(trace)$y_rep  
  
  # 计算每个观测值的后验均值（按列求平均）
  posterior_mean <- colMeans(posterior_pred, na.rm = TRUE)
  
  # 校验长度
  if (length(observed_data) != length(posterior_mean)) {
    stop("观测值与后验预测均值长度不匹配")
  }
  
  # 计算MAE
  mae <- stats::median(abs(observed_data - posterior_mean), na.rm = TRUE)
  return(mae)
}

tibble::tibble(
  "Model 1" = calculate_mae(model1_fit, df$RT_sec),
  "Model 2" = calculate_mae(model2_fit, df$RT_sec),
  "Model 3" = calculate_mae(model3_fit, df$RT_sec)
)

# 以 model 3 为例计算elpd_loo

library(loo)

# 提取对数似然
log_lik <- loo::extract_log_lik(model3_fit_new, parameter_name = "log_lik")

# 3. 计算ELPD_{LOO-CV}
elpd_loo_result <- loo::loo(log_lik)

# 查看结果
print(elpd_loo_result)

# 为每个模型计算ELPD_loo结果
# 模型1
log_lik1 <- loo::extract_log_lik(model1_fit, parameter_name = "log_lik")
loo1 <- loo::loo(log_lik1)

# 模型2
log_lik2 <- loo::extract_log_lik(model2_fit, parameter_name = "log_lik")
loo2 <- loo::loo(log_lik2)

# 模型3
log_lik3 <- loo::extract_log_lik(model3_fit, parameter_name = "log_lik")
loo3 <- loo::loo(log_lik3)

# 构建模型比较列表
comparison_list <- list(
  model1 = loo1,
  model2 = loo2,
  model3 = loo3
)

# 比较模型
loo::loo_compare(comparison_list)

calculate_dic <- function(model_fit) {
  # log-likelihood 计算 DIC (Deviance Information Criterion)。 参考 Evans, N. J. (2019). Assessing the practical differences between model selection methods in inferences about choice response time tasks. Psychonomic Bulletin & Review, 26(4), 1070–1098. https://doi.org/10.3758/s13423-018-01563-

  # 从模型中提取log_likelihood矩阵
  log_likelihood <- loo::extract_log_lik(model_fit, parameter_name = "log_lik")
  
  # 计算每个样本的Deviance
  deviance_samples <- -2 * log_likelihood
  
  # 计算平均Deviance
  D_bar <- mean(deviance_samples, na.rm = TRUE)
  
  # 计算有效参数 p_D
  p_D <- max(deviance_samples, na.rm = TRUE) - D_bar
  
  # 计算DIC
  DIC <- -2 * (D_bar - p_D)
  
  # 返回DIC值（单个数值）
  return(DIC)
}

# 调用 compute_dic 函数
model1_dic <- calculate_dic(model1_fit)
cat("模型1 DIC：", round(model1_dic, 2), "\n")

data.frame(
  "Model 1" = calculate_dic(model1_fit),  
  "Model 2" = calculate_dic(model2_fit), 
  "Model 3" = calculate_dic(model3_fit)
)

# 练习部分

# 安装和加载包
options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
if (!requireNamespace('pacman', quietly = TRUE)) {
    install.packages('pacman')
}
pacman::p_load("tidyverse","ggplot2", "dplyr","gridExtra","papaja", "patchwork","bayesplot",
               "rstan","bridgesampling", "logspline", "easystats", "loo") 
options(warn = -1)  # 抑制警告

# 导入数据
df_re <- tryCatch({
  read.csv('/home/mw/input/bayes3797/Data_Sum_HPP_Multi_Site_Share.csv')
}, error = function(e) {
  read.csv('data/Data_Sum_HPP_Multi_Site_Share.csv')
})

# 筛选站点为"Tsinghua"的数据
df <- df_re %>%
  dplyr::filter(Site == "Tsinghua") %>%  # 筛选条件
  dplyr::select(stress, scontrol, smoke)  # 选择需要的列

# 1 表示吸烟，2表示不吸烟
df <- df %>%
  dplyr::mutate(smoke = ifelse(smoke == 2, 0, 1),  # 将 'smoke' 列重新编码 
                smoke = ifelse(smoke == 1, "yes", "no"))  # 添加新的 'smoke_recode' 列

# 设置索引
df <- df %>%
  dplyr::mutate(index = row_number()) %>%  # 创建索引列
  column_to_rownames("index")        # 将 'index' 设置为行名

# 查看处理后的数据框
head(df)

# 定义模型4 （压力预测自我控制）
stan_model4 <- 
"
data {
  int<lower=0> N;     
  vector[N] y;                  // scontrol
  vector[N] X;                  // 连续变量: stress
}

parameters {
  ... beta_0;                   // 截距
  ... beta_1;                   // stress的斜率
  ... sigma;                    // 误差标准差       
}

model {
  ...    
  
  // 似然函数
  ... ~ ...
}

generated quantities {
  real y_rep[N];             // 后验预测
  vector[N] log_lik;         // 逐点对数似然
 
  for (n in 1:N) {
    // 后验预测值  (计算 MAE 需要)
    y_rep[n] = normal_rng(...);
    // 逐点对数似然 (计算 loo 和 DIC 需要)
    log_lik[n] = normal_lpdf(y[n] | ...);
  }
}
"
# 准备数据列表
data_list4 <- list(
  ...
)

# 定义模型5
# 提示：在模型4的基础上增加第二个预测变量：是否吸烟（需要用哑变量编码）

# 将分类变量转换为哑变量（以'no'为基线）
smoke <- as.integer(df$smoke == 'yes')

stan_model5 <- 
"
data {
  int<lower=0> N;           
  vector[N] y;            
  vector[N] X1;                   // 连续变量: stress
  vector[N] X2;                // 二分类变量: smoke 
}

parameters {
  ... beta_0;                   // 截距
  ... beta_1;                   // stress的斜率
  ... beta_2;                   // smoke的斜率
  ... sigma;           // 误差标准差      
}

model {
  ...    
  
  // 似然函数
  ... ~ ...
}

generated quantities {
  real y_rep[N];             // 后验预测
  vector[N] log_lik;         // 逐点对数似然
 
  for (n in 1:N) {
    // 后验预测值
    y_rep[n] = normal_rng(...);
    // 逐点对数似然
    log_lik[n] = normal_lpdf(y[n] | ...);
  }
}
"
# 准备数据列表
data_list5 <- list(
  ...
)

# 定义模型6（交互效应模型：压力×吸烟状态）
# 提示：基于模型5，新增“压力×吸烟”的交互效应

# 将分类变量转换为哑变量（以'no'为基线）
smoke <- as.integer(df$smoke == 'yes')
# 准备交互效应的数据 （外部计算需要交互项的数据，并在后面的stan模型中添加这个变量）
Interaction <- smoke * df$stress

stan_model6 <- 
"
data {
  int<lower=0> N;          
  vector[N] y;              // scontrol
  vector[N] X1;             // 连续变量: stress
  vector[N] X2;             // 二分类变量: smoke 
}

parameters {
  ... beta_0;                   // 截距
  ... beta_1;                   // stress的斜率
  ... beta_2;                   // smoke的斜率
  ... sigma;                    // 误差标准差            
}

model {
  ...    
  
  // 似然函数
  ... ~ ...
}

generated quantities {
  real y_rep[N];             // 后验预测
  vector[N] log_lik;         // 逐点对数似然
 
  for (n in 1:N) {
    // 后验预测值  (计算 MAE 需要)
    y_rep[n] = normal_rng(...);
    // 逐点对数似然 (计算 loo 和 DIC 需要)
    log_lik[n] = normal_lpdf(y[n] | ...);
  }
}
"
# 准备数据列表
data_list6 <- list(
  ...
)

#========================================
#     注意！！！以下代码可能需要运行 5 分钟左右,直接运行即可
#     直接运行即可，无需修改
#========================================

run_stan_sampling <- function(save_name, model_code = NULL, data_list = NULL, 
                              iter = 3000, warmup = 1000, chains = 4, 
                              thin = 1, seed = 84735) {
  # 运行Stan模型采样，存在结果文件时直接加载，否则执行采样并保存。
  # 
  # Parameters:
  # - save_name: 保存/加载结果的文件名（无扩展名）
  # - model_code: Stan模型代码字符串（仅采样时需提供）
  # - data_list: Stan模型数据列表（仅采样时需提供）
  # - iter: 总迭代次数（默认3000，含warmup）
  # - warmup: 预热迭代次数（默认1000，将被丢弃）
  # - chains: 采样链数（默认4）
  # - thin: 采样 thinning 间隔（默认1）
  # - seed: 随机种子（默认84735）
  # 
  # Returns:
  # - fit: Stan采样结果对象
  
  # 定义保存文件路径（.rds格式，R标准二进制格式）
  rds_file <- base::paste0(save_name, ".rds")
  
  # 检查文件是否存在
  if (base::file.exists(rds_file)) {
    base::cat(base::sprintf("加载现有的采样结果：%s\n", rds_file))
    fit <- base::readRDS(rds_file)
  } else {
    # 校验模型代码和数据是否提供
    if (base::is.null(model_code) || base::is.null(data_list)) {
      base::stop("模型未定义或数据缺失，请提供model_code和data_list")
    }
    
    base::cat(base::sprintf("未找到现有结果，正在执行采样：%s\n", save_name))
    # 执行Stan采样
    fit <- rstan::stan(
      model_code = model_code,
      data = data_list,
      iter = iter,
      chains = chains,
      warmup = warmup,
      thin = thin,
      seed = seed
    )
    
    # 保存采样结果到RDS文件
    base::saveRDS(fit, file = rds_file)
    base::cat(base::sprintf("采样结果已保存至：%s\n", rds_file))
  }
  
  return(fit)
}

# 运行模型4采样
model4_fit <- run_stan_sampling(save_name = "lec11_model4", model_code = stan_model4, data_list = data_list4)

# 运行模型5采样
model5_fit <- run_stan_sampling(save_name = "lec11_model5", model_code = stan_model6, data_list = data_list5)

# 运行模型6采样
model6_fit <- run_stan_sampling(save_name = "lec11_model6", model_code = stan_model6, data_list = data_list6)

calculate_mae <- function(trace, observed_data) {
  # 从stanfit对象中提取所有后验预测样本（适用于任何变量名）
  # 提取后验预测样本矩阵（行=迭代×链，列=观测值）
  posterior_pred <- rstan::extract(trace)$y_rep  
  
  # 计算每个观测值的后验均值（按列求平均）
  posterior_mean <- colMeans(posterior_pred, na.rm = TRUE)
  
  # 校验长度
  if (length(observed_data) != length(posterior_mean)) {
    stop("观测值与后验预测均值长度不匹配")
  }
  
  # 计算MAE
  mae <- stats::median(abs(observed_data - posterior_mean), na.rm = TRUE)
  return(mae)
}

##================================================
#                练习，修改... 部分
#                
#================================================

tibble::tibble(
  "Model 4" = calculate_mae(model4_fit, ...),
  "Model 5" = calculate_mae(model5_fit, ...),
  "Model 6" = calculate_mae(model6_fit, ...)
)

##================================================
#                练习，修改... 部分       
#================================================

# 为每个模型计算ELPD_loo结果
# 模型4
log_lik4 <- loo::extract_log_lik(model4_fit, parameter_name = "log_lik")
loo4 <- loo::loo(log_lik4)

# 模型5
log_lik5 <- loo::extract_log_lik(model5_fit, parameter_name = "log_lik")
loo5 <- loo::loo(log_lik5)

# 模型6
log_lik6 <- loo::extract_log_lik(model6_fit, parameter_name = "log_lik")
loo6 <- loo::loo(log_lik6)

# 构建模型比较列表
comparison_list <- list(
  model4 = ...,
  model5 = ...,
  model6 = ...
)

# 比较模型
loo::loo_compare(comparison_list)

calculate_dic <- function(model_fit) {
  # log-likelihood 计算 DIC (Deviance Information Criterion)。 参考 Evans, N. J. (2019). Assessing the practical differences between model selection methods in inferences about choice response time tasks. Psychonomic Bulletin & Review, 26(4), 1070–1098. https://doi.org/10.3758/s13423-018-01563-

  # 从模型中提取log_likelihood矩阵
  log_likelihood <- loo::extract_log_lik(model_fit, parameter_name = "log_lik")
  
  # 计算每个样本的Deviance
  deviance_samples <- -2 * log_likelihood
  
  # 计算平均Deviance
  D_bar <- mean(deviance_samples, na.rm = TRUE)
  
  # 计算有效自由度 p_D
  p_D <- max(deviance_samples, na.rm = TRUE) - D_bar
  
  # 计算DIC
  DIC <- -2 * (D_bar - p_D)
  
  # 返回DIC值（单个数值）
  return(DIC)
}

##================================================
#                练习，修改... 部分
#                
#================================================

data.frame(
  "Model 4" = calculate_dic(...),
  "Model 5" = calculate_dic(...),
  "Model 6" = calculate_dic(...),
)