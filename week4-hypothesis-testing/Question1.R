# QA-------------------------------------------------------------
# clear the console area
cat("\014")
# clear the environment var area
rm(list = ls())


# Data
walk_lengths <- c(45.6, 41.8, 46.4, 41.7, 44.0, 44.6, 46.8, 42.9, 45.1, 45.7, 45.1, 41.6)

# Draw Q-Q Plot
qqnorm(walk_lengths)
qqline(walk_lengths)

# Shapiro-Wilk normal distribution test
shapiro.test(walk_lengths)



# QA-------------------------------------------------------------
# calculate mean data
mu <- 45

# 计算样本均值
sample_mean <- mean(walk_lengths)

# 计算样本标准差
sample_sd <- sd(walk_lengths)

# 计算检验统计量（Z分数）
test_statistic <- (sample_mean - mu) / (sample_sd / sqrt(length(walk_lengths)))
test_statistic



# QA-------------------------------------------------------------
# 显著性水平
alpha <- 0.05


critical_value <- qnorm(1 - alpha/2)
critical_value


# 进行假设检验
if (abs(test_statistic) > critical_value) {
  cat("Reject H0: The average walk length is not equal to 45 minutes.\n")
} else {
  cat("Fail to reject H0: The average walk length is equal to 45 minutes.\n")
}


