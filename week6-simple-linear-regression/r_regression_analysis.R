# clear the console area
cat("\014")
# clear the environment var area
rm(list = ls())
# clear all plots
graphics.off()
# -----------------------------------------------------------------------------

# 加载faithful数据集
data(faithful) 

# use boxplot to see if exists error data
par(mfrow = c(1,2))
boxplot(faithful$eruptions, main="Eruptions")
boxplot(faithful$waiting, main="Waiting")

# -----------------------------------------------------------------------------
# 创建等待时间向量 w 和喷发持续时间向量 d
w <- faithful$waiting
d <- faithful$eruptions
# -----------------------------------------------------------------------------
# 绘制散点图
plot(d, w, main="Waiting Time vs Eruption Duration", 
     xlab="Eruption Duration (minutes)", 
     ylab="Waiting Time", pch=1)
# -----------------------------------------------------------------------------
# 使用cor函数评估线性关系的强度
correlation <- cor(d, w)
cat("Correlation between Waiting Time and Eruption Duration:",
    correlation, "\n")
# -----------------------------------------------------------------------------

# Check normality of waiting times
shapiro.test(w)
# Check normality of eruption durations
shapiro.test(d)
# W and D do not conform to the assumption of normal distribution.
# -----------------------------------------------------------------------------
# 使用Spearman等级相关系数
spearman_cor <- cor(d, w, method="spearman")
cat("Spearman's Rank Correlation Coefficient:", spearman_cor, "\n")
# -----------------------------------------------------------------------------
# 使用Kendall's tau相关系数
kendall_cor <- cor(d, w, method="kendall")
cat("Kendall's Tau Correlation Coefficient:", kendall_cor, "\n")
# -----------------------------------------------------------------------------
# Fit linear regression model
model <- lm(w ~ d)
# -----------------------------------------------------------------------------
# Extract coefficients
intercept <- coef(model)[1]
slope <- coef(model)[2]

cat("Intercept (beta_0):", intercept, "\n")
cat("Slope (beta_1):", slope, "\n")

# -----------------------------------------------------------------------------
# Extract coefficients as a vector
coefficients_vector <- coef(model)
cat("Coefficients Vector:", coefficients_vector, "\n")

# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------


