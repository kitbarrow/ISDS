# QA-------------------------------------------------------------
# 载入数据
data(faithful)

# 提取数据
eruptions <- faithful$eruptions

# 假设的平均喷发持续时间
mu <- 3.5

# 样本均值
sample_mean <- mean(eruptions)

# 样本标准差
sample_sd <- sd(eruptions)

# 样本容量
n <- length(eruptions)

# 计算检验统计量（t 分数）
test_statistic <- (sample_mean - mu) / (sample_sd / sqrt(n))
test_statistic

# QB-------------------------------------------------------------

# 显著性水平
alpha <- 0.05

# 自由度（样本容量减去 1）
df <- n - 1

# 计算 p-值
p_value <- pt(test_statistic, df)
p_value

# QC-------------------------------------------------------------


# QE-------------------------------------------------------------

# 使用标准正态分布计算 p-值
p_value_std_normal <- pnorm(test_statistic)
p_value_std_normal
