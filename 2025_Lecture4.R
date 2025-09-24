# 安装和加载包
options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
if (!requireNamespace('pacman', quietly = TRUE)) {
  install.packages('pacman')
}
pacman::p_load("tidyverse","ggplot2", "dplyr","gridExtra","papaja","patchwork")
options(warn = -1)  # 抑制警告

# 数据导入
data <- tryCatch({
  read.csv('/home/mw/input/bayes3797/evans2020JExpPsycholLearn_exp1_full_data.csv') #平台路径
}, error = function(e) {
  read.csv('/Users/liumingyu/Desktop/贝叶斯2025/data 2/evans2020JExpPsycholLearn_exp1_full_data.csv') #本地路径
})

cat("被试数量：", length(unique(data$subject)), "\n")
data %>% 
  dplyr::group_by(subject) %>% 
  dplyr::summarise(mean_correct = mean(correct, na.rm = TRUE)) %>% 
  head(10) %>%
  as.data.frame()

# 筛选符合条件的数据
data_subj1 <- data %>%
  dplyr::select(subject, percentCoherence, correct, RT) %>%
  dplyr::filter(subject == 82111, percentCoherence == 5)

# 打印该被试在该条件下的平均正确率
cat("被试 82111 在 5% 一致性正确率数据：", mean(data_subj1$correct, na.rm = TRUE), "\n")
head(data_subj1, 5)

#统计 'binary' 列中各个值的出现次数
data_subj1 %>%
  dplyr::count(correct)

#创建贝叶斯绘图函数
bayesian_analysis_plot <- function(
    alpha, beta, y, n,
    plot_prior      = TRUE,
    plot_likelihood = TRUE,
    plot_posterior  = TRUE,
    xlabel          = expression("ACC" ~ pi),
    show_legend     = TRUE
) {
  
  # 先验分布
  if (plot_prior) {
    x_prior <- seq(qbeta(1e-4, alpha, beta), qbeta(1 - 1e-4, alpha, beta), length.out = 100)
    y_prior <- dbeta(x_prior, alpha, beta)
    prior_df <- data.frame(x = x_prior, y = y_prior, dist = "Prior")
  }
  
  # 似然分布
  if (plot_likelihood) {
    x_like <- seq(0, 1, length.out = 1000)
    y_like <- dbeta(x_like, y + 1, n - y + 1)
    like_df <- data.frame(x = x_like, y = y_like, dist = "Likelihood")
  }
  
  # 后验分布
  if (plot_posterior) {
    a_post <- alpha + y
    b_post <- beta + n - y
    x_post <- seq(qbeta(1e-4, a_post, b_post), qbeta(1 - 1e-4, a_post, b_post), length.out = 100)
    y_post <- dbeta(x_post, a_post, b_post)
    post_df <- data.frame(x = x_post, y = y_post, dist = "Posterior")
  }
  
  # 合并数据 
  plot_data <- do.call(rbind, Filter(Negate(is.null), list(
    if (plot_prior)      transform(prior_df, dist = "prior")      else NULL,
    if (plot_likelihood) transform(like_df,  dist = "likelihood") else NULL,
    if (plot_posterior)  transform(post_df,  dist = "posterior")  else NULL
  )))
  plot_data$dist <- factor(plot_data$dist, c("prior","likelihood","posterior"))
  
  # 作图
  cols_fill <- c(prior="#f0e442", likelihood="#0071b2", posterior="#009e74")
  present   <- intersect(c("prior","likelihood","posterior"), unique(plot_data$dist))
  legend_levels <- as.vector(rbind(paste0(present, "_line"),
                                   paste0(present, "_fill")))
  
  labs_all <- setNames(c(
    sprintf("Beta(alpha=%d, beta=%d)", alpha, beta),          "Prior",
    sprintf("Binomial(n=%d, p=%.2g)", n, y/n),                "Likelihood",
    sprintf("Beta(alpha=%d, beta=%d)", alpha+y, beta+n-y),    "Posterior"
  ), c("prior_line","prior_fill","likelihood_line","likelihood_fill",
       "posterior_line","posterior_fill"))[legend_levels]
  
  plot_data$legend_line <- factor(paste0(plot_data$dist, "_line"), levels = legend_levels)
  plot_data$legend_fill <- factor(paste0(plot_data$dist, "_fill"), levels = legend_levels)
  
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x, y)) +
    ggplot2::geom_area(ggplot2::aes(fill  = legend_fill),
                       alpha = 0.7, position = "identity", colour = NA) +
    ggplot2::geom_line(ggplot2::aes(color = legend_line), linewidth = 1, lineend = "round", linejoin = "round") +
    ggplot2::labs(x = xlabel, y = "Density") +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::scale_fill_manual(
      name   = NULL,
      values = setNames(ifelse(grepl("_fill$", legend_levels),
                               cols_fill[sub("_fill$", "", legend_levels)], NA),
                        legend_levels),
      breaks = legend_levels,
      labels = labs_all,
      drop   = FALSE
    ) +
    ggplot2::scale_color_manual(
      name   = NULL,
      values = setNames(ifelse(grepl("_line$", legend_levels), "black", NA),
                        legend_levels),
      breaks = legend_levels,
      labels = labs_all,
      drop   = FALSE
    ) +
    papaja::theme_apa() +
    theme(legend.background = element_rect(fill = "transparent"))
  return(p)
}

#利用函数，带入参数绘图
options(repr.plot.width=14,repr.plot.height=6)
fig <- bayesian_analysis_plot(70, 30, 152, 253) +
  ggplot2::coord_cartesian(xlim = c(0.4, 0.9), ylim = c(0, 16)) +
  theme(legend.text = element_text(size = 14),
        legend.position = c(0.15, 0.85))
print(fig)

# 定义先验分布的 alpha 和 beta
alpha <- 70
beta  <- 30
plots <- list()
y_values <- c(77, 152, 231)
n_values <- c(128, 254, 385)

for (i in 1:3) {
  plots[[i]] <- bayesian_analysis_plot(alpha, beta, y = y_values[i],  n = n_values[i], plot_posterior = FALSE) +
    ggplot2::coord_cartesian(xlim = c(0.4, 0.9)) +
    theme(legend.text = element_text(size = 8.3),
          legend.position = c(0.87, 0.85))
}

options(repr.plot.width=18, repr.plot.height=6) #自定义画布大小
plots[[1]] + plots[[2]] + plots[[3]]

# 分别创建三个图（包含后验）
plots <- list()
for (i in 1:3) {
  plots[[i]] <- bayesian_analysis_plot(alpha, beta, y = y_values[i],  n = n_values[i], plot_posterior = TRUE) +
    ggplot2::coord_cartesian(xlim = c(0.4, 0.9)) +
    theme(legend.text = element_text(size = 9),
          legend.position = c(0.86, 0.85))
}

options(repr.plot.width=18, repr.plot.height=6) #自定义画布大小
plots[[1]] + plots[[2]] + plots[[3]]


##——————————————————————
library(shiny)

data_correct <- data_subj1$correct

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .update-btn {
        width: 200px; height: 30px; border-radius: 10px; 
        background-color: #1E90FF; color: white; font-size: 18px;
        border: none;
      }
    "))
  ),
  titlePanel("Bayesian Updating (Beta-Binomial)"),
  
  fluidRow(
    column(
      width = 4,
      actionButton("update", "Update with more data", class = "update-btn"),
      br(), br(),
      sliderInput("prior_alpha", "Prior alpha", min = 1, max = 200, value = 1, step = 1),
      sliderInput("prior_beta",  "Prior beta",  min = 1, max = 200, value = 1, step = 1),
      sliderInput("init_trial",  "Initial trial (start index)", min = 1, max = 100, value = 1, step = 1),
      sliderInput("step",        "Step size (per update)",      min = 1, max = 20,  value = 1,  step = 1),
      checkboxInput("show_prior",      "Show prior", TRUE),
      checkboxInput("show_last_post",  "Show posterior (t-1)", TRUE)
    ),
    column(
      width = 8,
      plotOutput("plt", height = 480)
    )
  )
)

server <- function(input, output, session) {
  
  count <- reactiveVal(-1)
  
  observeEvent(input$update, {
    count(count() + 1)
  })
  
  make_plot <- reactive({
    prior_alpha   <- input$prior_alpha
    prior_beta    <- input$prior_beta
    init_trial    <- input$init_trial
    step          <- input$step
    show_prior    <- isTRUE(input$show_prior)
    show_last_post<- isTRUE(input$show_last_post)
    
    cnt <- count()
    
    x <- seq(0, 1, length.out = 1000)
    
    trial_number_last    <- init_trial + (cnt - 1) * step
    trial_number_current <- init_trial + cnt * step
    
    p <- ggplot() + theme_classic() +
      theme(legend.title = element_blank(),
            axis.title.y = ggplot2::element_blank(),
            axis.title.x = ggplot2::element_blank(),
            axis.text.x  = ggplot2::element_text(size = 12),
            axis.text.y  = ggplot2::element_text(size = 12),
            legend.text = element_text(size = 16))
    
    if (show_prior) {
      y_prior <- dbeta(x, prior_alpha, prior_beta)
      p <- p + geom_line(aes(x = x, y = y_prior, color = "prior"),
                         linetype = "dotdash", linewidth = 0.9)
    }
    
    n_total <- length(data_correct)
    
    if (cnt < 0) {
      p <- p + ggtitle(sprintf("Prior Beta: alpha=%d, beta=%d", prior_alpha, prior_beta)) +
        theme(plot.title = element_text(hjust = 0.5))
      return(
        p + scale_color_manual(name = NULL,values = c("prior" = "navy"))+
          papaja::theme_apa()+
          ggplot2::theme(
            legend.margin = margin(t = 2, r = 4, b = 2, l = 4, unit = "pt"),
            legend.background    = element_rect(fill = "transparent", colour = "NA", linewidth = 0.6),
            legend.box.background= element_rect(fill = "transparent", colour = "grey", linewidth = 0.6),
            legend.key           = element_rect(fill = "transparent", colour = NA),
            axis.title.x = ggplot2::element_blank(),
            axis.title.y = ggplot2::element_blank(),
            legend.box = "vertical",
          )
      )
    } else if (trial_number_current > n_total) {
      p <- p + ggtitle(sprintf("All Trials %d with %d corrects", n_total, sum(data_correct))) +
        theme(plot.title = element_text(hjust = 0.5))
      return(
        p + scale_color_manual(name = NULL,values = c("prior" = "navy",
                                                      "posterior (t-1)" = "olivedrab",
                                                      "posterior" = "orangered"))+
          papaja::theme_apa()+
          ggplot2::theme(
            legend.margin = margin(t = 2, r = 4, b = 2, l = 4, unit = "pt"),
            legend.background    = element_rect(fill = "transparent", colour = "NA", linewidth = 0.6),
            legend.box.background= element_rect(fill = "transparent", colour = "grey", linewidth = 0.6),
            legend.key           = element_rect(fill = "transparent", colour = NA),
            axis.title.x = ggplot2::element_blank(),
            axis.title.y = ggplot2::element_blank(),
            legend.box = "vertical",
          )
      )
    } else if (cnt == 0) {
      tmp_data <- data_correct[seq_len(trial_number_current)]
      p <- p + ggtitle(sprintf("Trial %d with %d trials and %d corrects",
                               trial_number_current - init_trial,
                               length(tmp_data),
                               sum(tmp_data))) +
        theme(plot.title = element_text(hjust = 0.5))
    } else if (cnt > 0) {
      start_idx <- trial_number_last + 1
      end_idx   <- trial_number_current
      
      if (start_idx <= end_idx && start_idx >= 1) {
        tmp_data <- data_correct[start_idx:end_idx]
      } else {
        tmp_data <- integer(0)
      }
      
      p <- p + ggtitle(sprintf("Trial %d with %d trials and %d corrects",
                               trial_number_last,
                               length(tmp_data),
                               sum(tmp_data))) +
        theme(plot.title = element_text(hjust = 0.5))
      
      if (show_last_post && trial_number_last > 0) {
        n_correct_last <- sum(data_correct[seq_len(trial_number_last)])
        n_false_last   <- trial_number_last - n_correct_last
        post_a_last    <- prior_alpha + n_correct_last
        post_b_last    <- prior_beta  + n_false_last
        y_last <- dbeta(x, post_a_last, post_b_last)
        p <- p + geom_line(aes(x = x, y = y_last, color = "posterior (t-1)"),
                           alpha = 0.4, linewidth = 0.9)
      }
    }
    
    n_correct_cur <- sum(data_correct[seq_len(trial_number_current)])
    n_false_cur   <- trial_number_current - n_correct_cur
    post_a_cur    <- prior_alpha + n_correct_cur
    post_b_cur    <- prior_beta  + n_false_cur
    y_cur <- dbeta(x, post_a_cur, post_b_cur)
    p <- p + geom_line(aes(x = x, y = y_cur, color = "posterior"), linewidth = 1.1)
    
    p + scale_color_manual(name = NULL,values = c("prior" = "navy",
                                                  "posterior (t-1)" = "olivedrab",
                                                  "posterior" = "orangered")) +
      
      papaja::theme_apa()+
      ggplot2::theme(
        legend.margin = margin(t = 2, r = 4, b = 2, l = 4, unit = "pt"),
        legend.background    = element_rect(fill = "transparent", colour = "NA", linewidth = 0.6),
        legend.box.background= element_rect(fill = "transparent", colour = "grey", linewidth = 0.6),
        legend.key           = element_rect(fill = "transparent", colour = NA),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        legend.box = "vertical",
      )
  })
  
  output$plt <- renderPlot({
    make_plot()
  })
}

shinyApp(ui, server)

##——————————————————————

#设置绘图函数
plot_pdf <- function(alpha, beta, level = 0.95,
                     line_col = "#008b92", title = NULL,
                     baseline = 0) {
  x  <- seq(0, 1, length.out = 1000)
  y  <- dbeta(x, alpha, beta)
  df <- data.frame(x, y)
  
  lo_outer <- qbeta((1 - level)/2, alpha, beta)
  hi_outer <- qbeta(1 - (1 - level)/2, alpha, beta)
  lo_inner <- qbeta(0.25, alpha, beta)
  hi_inner <- qbeta(0.75, alpha, beta)
  
  mu <- alpha/(alpha + beta)
  
  df$y_cut <- if (baseline > 0) ifelse(df$y < baseline, NA, df$y) else df$y
  
  ggplot(df, aes(x, y_cut)) +
    geom_line(linewidth = 1.2, colour = line_col, lineend = "round", na.rm = TRUE) +
    annotate("segment", x = 0, xend = 1, y = baseline, yend = baseline,
             linetype = "dashed", colour = "grey60") +
    annotate("segment", x = lo_outer, xend = hi_outer, y = baseline, yend = baseline,
             linewidth = 1.2, colour = "black", lineend = "round") +
    annotate("segment", x = lo_inner, xend = hi_inner, y = baseline, yend = baseline,
             linewidth = 2.5, colour = "black", lineend = "round") +
    annotate("point", x = mu, y = baseline, shape = 21, size = 4.2,
             stroke = 1.1, fill = "white", colour = "black") +
    coord_cartesian(xlim = c(0, 1), ylim = c(baseline, max(df$y) * 1.08)) +
    labs(title = sprintf("Beta(alpha=%d, beta=%d)", alpha, beta),
         x = "x", y = "Density") +
    papaja::theme_apa() 
}

#输入不同参数作图
options(repr.plot.width=18, repr.plot.height=6) #自定义画布大小
p1 <- plot_pdf(70, 30)
p2 <- plot_pdf(10, 1)
p3 <- plot_pdf(1, 1)
p1 + p2 + p3

data <- tryCatch({
  read.csv('/home/mw/input/bayes3797/evans2020JExpPsycholLearn_exp1_full_data.csv') #平台路径
}, error = function(e) {
  read.csv('C:/Users/A/Desktop/evans2020JExpPsycholLearn_exp1_full_data.csv') #本地路径
})

data %>% 
  group_by(subject) %>% 
  summarise(mean_correct = mean(correct, na.rm = TRUE)) %>% 
  head(10) %>%
  as.data.frame()

# 选取需要的列
data <- data %>%
  select(subject, percentCoherence, correct, RT)

# 筛选符合条件的数据
data_subj1 <- data %>%
  filter(subject == 82111, percentCoherence == 5)
head(data_subj1, 5)

# 分别创建三个图（不包含后验）
plots <- list()
alpha_list <- c(70, 10, 1)
beta_list <- c(30, 1, 1)

for (i in 1:3) {
  plots[[i]] <- bayesian_analysis_plot(alpha_list[i], beta_list[i], y = 152,  n = 254, plot_posterior = FALSE) +
    ggtitle(sprintf("prior:Beta(alpha=%d, beta=%d)", alpha_list[i], beta_list[i])) +
    ggplot2::coord_cartesian(xlim = c(0.4, 0.9)) +
    theme(legend.text = element_text(size = 10),
          legend.position = c(0.8, 0.85))
}

options(repr.plot.width=18, repr.plot.height=6) #自定义画布大小
plots[[1]] + plots[[2]] + plots[[3]]

# 分别创建三个图（包含后验）
plots <- list()
alpha_list <- c(70, 10, 1)
beta_list <- c(30, 1, 1)

for (i in 1:3) {
  plots[[i]] <- bayesian_analysis_plot(alpha_list[i], beta_list[i], y = 152,  n = 254, plot_posterior = TRUE) +
    ggtitle(sprintf("prior:Beta(alpha=%d, beta=%d)", alpha_list[i], beta_list[i])) +
    ggplot2::coord_cartesian(xlim = c(0.4, 0.9)) +
    theme(legend.text = element_text(size = 9),
          legend.position = c(0.8, 0.85))
}

options(repr.plot.width=18, repr.plot.height=6) #自定义画布大小
plots[[1]] + plots[[2]] + plots[[3]]

# 分别创建三个图（包含后验）
plots <- list()
alpha_list2 <- c(70, 700, 7000)
beta_list2 <- c(30, 300, 3000)

for (i in 1:3) {
  plots[[i]] <- bayesian_analysis_plot(alpha_list2[i], beta_list2[i], y = 152,  n = 254, plot_posterior = TRUE) +
    ggtitle(sprintf("prior:Beta(alpha=%d, beta=%d)", alpha_list2[i], beta_list2[i])) +
    ggplot2::coord_cartesian(xlim = c(0.4, 0.9)) +
    theme(legend.text = element_text(size = 8),
          legend.position = c(0.2, 0.85))
}

options(repr.plot.width=18, repr.plot.height=6) #自定义画布大小
plots[[1]] + plots[[2]] + plots[[3]]

# 分别创建九个图（包含后验）
plots <- list()
alpha_val <- c(70, 70, 70, 10, 10, 10, 1, 1, 1)
beta_val <- c(30, 30, 30, 1, 1, 1, 1, 1, 1)
y_val <- c(77,152,231,77,152,231,77,152,231)
n_val <- c(128,254,385,128,254,385,128,254,385)

for (i in 1:9) {
  plots[[i]] <- bayesian_analysis_plot(alpha_val[i], beta_val[i], y = y_val[i],  n = n_val[i], plot_posterior = TRUE) +
    ggtitle(sprintf("prior:Beta(alpha=%d, beta=%d)", alpha_val[i], beta_val[i])) +
    ggplot2::coord_cartesian(xlim = c(0.4, 0.9)) +
    theme(legend.text = element_text(size = 9),
          legend.position = c(0.8, 0.85))
}

options(repr.plot.width=18, repr.plot.height=18) #自定义画布大小
gridExtra::grid.arrange(grobs = plots, ncol = 3)






##课后代码练习
bayesian_analysis_plot <- function(
    alpha, beta, y, n,
    plot_prior      = TRUE,
    plot_likelihood = TRUE,
    plot_posterior  = TRUE,
    xlabel          = expression("ACC" ~ pi),
    show_legend     = TRUE
) {
  
  # 先验分布
  if (plot_prior) {
    x_prior <- seq(qbeta(1e-4, alpha, beta), qbeta(1 - 1e-4, alpha, beta), length.out = 100)
    y_prior <- dbeta(x_prior, alpha, beta)
    prior_df <- data.frame(x = x_prior, y = y_prior, dist = "Prior")
  }
  
  # 似然分布
  if (plot_likelihood) {
    x_like <- seq(0, 1, length.out = 1000)
    y_like <- dbeta(x_like, y + 1, n - y + 1)
    like_df <- data.frame(x = x_like, y = y_like, dist = "Likelihood")
  }
  
  # 后验分布
  if (plot_posterior) {
    a_post <- alpha + y
    b_post <- beta + n - y
    x_post <- seq(qbeta(1e-4, a_post, b_post), qbeta(1 - 1e-4, a_post, b_post), length.out = 100)
    y_post <- dbeta(x_post, a_post, b_post)
    post_df <- data.frame(x = x_post, y = y_post, dist = "Posterior")
  }
  
  # 合并数据 
  plot_data <- do.call(rbind, Filter(Negate(is.null), list(
    if (plot_prior)      transform(prior_df, dist = "prior")      else NULL,
    if (plot_likelihood) transform(like_df,  dist = "likelihood") else NULL,
    if (plot_posterior)  transform(post_df,  dist = "posterior")  else NULL
  )))
  plot_data$dist <- factor(plot_data$dist, c("prior","likelihood","posterior"))
  
  # 作图
  cols_fill <- c(prior="#f0e442", likelihood="#0071b2", posterior="#009e74")
  present   <- intersect(c("prior","likelihood","posterior"), unique(plot_data$dist))
  legend_levels <- as.vector(rbind(paste0(present, "_line"),
                                   paste0(present, "_fill")))
  
  labs_all <- setNames(c(
    sprintf("Beta(alpha=%d, beta=%d)", alpha, beta),          "Prior",
    sprintf("Binomial(n=%d, p=%.2g)", n, y/n),                "Likelihood",
    sprintf("Beta(alpha=%d, beta=%d)", alpha+y, beta+n-y),    "Posterior"
  ), c("prior_line","prior_fill","likelihood_line","likelihood_fill",
       "posterior_line","posterior_fill"))[legend_levels]
  
  plot_data$legend_line <- factor(paste0(plot_data$dist, "_line"), levels = legend_levels)
  plot_data$legend_fill <- factor(paste0(plot_data$dist, "_fill"), levels = legend_levels)
  
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x, y)) +
    ggplot2::geom_area(ggplot2::aes(fill  = legend_fill),
                       alpha = 0.7, position = "identity", colour = NA) +
    ggplot2::geom_line(ggplot2::aes(color = legend_line), linewidth = 1, lineend = "round", linejoin = "round") +
    ggplot2::labs(x = xlabel, y = "Density") +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::scale_fill_manual(
      name   = NULL,
      values = setNames(ifelse(grepl("_fill$", legend_levels),
                               cols_fill[sub("_fill$", "", legend_levels)], NA),
                        legend_levels),
      breaks = legend_levels,
      labels = labs_all,
      drop   = FALSE
    ) +
    ggplot2::scale_color_manual(
      name   = NULL,
      values = setNames(ifelse(grepl("_line$", legend_levels), "black", NA),
                        legend_levels),
      breaks = legend_levels,
      labels = labs_all,
      drop   = FALSE
    ) +
    papaja::theme_apa() +
    theme(legend.background = element_rect(fill = "transparent"))
  return(p)
}

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
n = 180    # n 代表总人数

options(repr.plot.width=12, repr.plot.height=6) #自定义画布大小
bayesian_analysis_plot(alpha, beta, y, n)
