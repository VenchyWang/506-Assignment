set.seed(2025)
generate_data <- function(n, dist) {
  if (dist == "symmetric_mixture") {
    sample_indicator <- runif(n) < 0.5
    data <- ifelse(sample_indicator, 
                   rnorm(n, mean = -3), 
                   rnorm(n, mean = 3))
    return(data)
  } else if (dist == "asymmetric_mixture") {
    sample_indicator <- runif(n) < 0.7
    data <- ifelse(sample_indicator, 
                   rnorm(n, mean = -1), 
                   rnorm(n, mean = 2.3333))
    return(data)
  } else if (dist == "exponential") {
    return(rexp(n, rate = 1) - 1)
  } else if (dist == "lognormal") {
    return(rlnorm(n, meanlog = 0, sdlog = 1) - exp(0.5))
  } else {
    stop("Unknown distribution")
  }
}


distributions <- c("exponential", "lognormal", "symmetric_mixture", "asymmetric_mixture")
sample_sizes <- c(2:20, seq(22, 52, by = 5),seq(60, 500, by = 10))
sample_sizes <- c(2:50)

alpha <- 0.05
n_simulations <- 1000


results <- list()
for (dist in distributions) {
  results[[dist]] <- list()
  for (n in sample_sizes) {
    p_values <- replicate(n_simulations, {
      data <- generate_data(n, dist)
      t.test(data, mu = 0)$p.value
    })
    type1_error <- mean(p_values < alpha)
    results[[dist]][[as.character(n)]] <- list(
      p_values = p_values,
      type1_error = type1_error
    )
  }
}


library(ggplot2)

par(mfrow = c(2, length(distributions)))  
for (dist in distributions) {
  data <- generate_data(100, dist)
  hist(data, main = paste(dist), xlab = "Values", col = "cornflowerblue", border = "black")
}
for (dist in distributions) {
  data <- generate_data(100, dist)
  
  qqnorm(data, main = paste(dist))
  qqline(data, col = "red")
}

type1_error_df <- do.call(rbind, lapply(names(results), function(dist) {
  do.call(rbind, lapply(names(results[[dist]]), function(n) {
    error_rate <- results[[dist]][[n]]$type1_error
    ci <- compute_ci(error_rate, n_simulations)
    data.frame(
      Distribution = dist,
      Sample_Size = as.numeric(n),
      Type1_Error = error_rate,
      Lower_CI = ci["lower"],
      Upper_CI = ci["upper"]
    )
  }))
}))


custom_colors <- c("exponential" = "dodgerblue", 
                   "lognormal" = "indianred", 
                   "symmetric_mixture" = "cyan4", 
                   "asymmetric_mixture" = "lightsteelblue")
ggplot(type1_error_df, aes(x = Sample_Size, y = Type1_Error, color = Distribution, fill = Distribution)) +
  geom_line(size = 1) +  
  geom_point(size = 2) +  
  geom_hline(yintercept = alpha, linetype = "dashed", color = "black", size = 0.75) +  # 显著性水平线
  scale_x_continuous(breaks = seq(0, 
                                  max(type1_error_df$Sample_Size, na.rm = TRUE), 
                                  by = 50)) +
  scale_color_manual(values = custom_colors) +  # 设置折线颜色
  labs(
    title = "Type I Error Rate with Confidence Intervals",
    x = "Sample Size",
    y = "Type I Error Rate"
  ) +
  theme(
    legend.title=element_blank(),
    legend.position = c(1, 1),  
    legend.justification = c(1, 1),  
    # panel.grid.major = element_line(size = 0.5, color = "grey80"),
    # panel.grid.minor = element_line(size = 0.25, color = "grey90")
panel.grid=element_blank(),
 axis.line = element_line())







# library(tidyverse)
# threshold <- 0.01  # 允许的误差范围
# 
# stable_n <- type1_error_df %>%
#   filter(abs(Type1_Error - alpha) < threshold & Lower_CI <= alpha & Upper_CI >= alpha) %>%
#   group_by(Distribution) %>%
#   summarize(Min_Sample_Size = min(Sample_Size))
# 
# print(stable_n)
# 
# 
# library(dplyr)

# # 假设检验辅助判断
# stable_test <- type1_error_df %>%
#   mutate(
#     # 计算 t 检验统计量
#     t_statistic = (Type1_Error - alpha) / sqrt((Type1_Error * (1 - Type1_Error)) / n_simulations),
#     # 计算双尾 p 值
#     p_value = 2 * pt(-abs(t_statistic), df = n_simulations - 1)
#   ) %>%
#   group_by(Distribution) %>%
#   filter(p_value > 0.05) %>%  # 找到无法拒绝 H0 的点
#   summarize(Min_Sample_Size = min(Sample_Size))  # 最小样本量
# 
# print(stable_test)


tolerance_range <- c(0.045, 0.055)  
window_size <- 3

find_stable_sample_size <- function(results, tolerance_range, window_size) {
  stable_sizes <- lapply(names(results), function(dist) {
    sample_sizes <- as.numeric(names(results[[dist]]))
    errors <- sapply(results[[dist]], function(x) x$type1_error)
    
    for (i in seq_along(sample_sizes)) {
      if (i + window_size - 1 > length(sample_sizes)) break
      window <- errors[i:(i + window_size - 1)]
      if (all(window >= tolerance_range[1] & window <= tolerance_range[2])) {
        return(data.frame(
          Distribution = dist,
          Stable_Sample_Size = sample_sizes[i],
          Window_Start = i,
          Window_End = i + window_size - 1
        ))
      }
    }
    return(data.frame(
      Distribution = dist,
      Stable_Sample_Size = NA,  
      Window_Start = NA,
      Window_End = NA
    ))
  })
  do.call(rbind, stable_sizes)
}

stable_sample_sizes <- find_stable_sample_size(results, tolerance_range, window_size)
stable_sample_sizes



