# a---------------------------------------------------

# 创建奇数编号的等待时间向量
faithfulodd <- faithful$waiting[seq(1, 271, by = 2)]

# 创建偶数编号的等待时间向量
faithfuleven <- faithful$waiting[seq(2, 272, by = 2)]


# c---------------------------------------------------
# 显著性水平
alpha <- 0.01

# 进行双侧 t-检验
t_test_result <- t.test(faithfulodd, faithfuleven, alternative = "two.sided")

# 输出 t-检验结果
t_test_result



