# clear the console area
cat("\014")
# clear the environment var area
rm(list = ls())
# set work directory
setwd("C:/Users/QianZ/Documents/Project/ISDS/personal-assignment-2/RCode")


# Import data using base R function
sugar_data <- read.csv("./data/Sugar.csv")

# Generate a Q-Q plot using base R functions
qqnorm(sugar_data$x)
qqline(sugar_data$x, col = "red")
# 
# 
# 
# 
# 
# Perform the Shapiro-Wilk test on the sugar data
shapiro_test <- shapiro.test(sugar_data$x)

# Output the p-value from the test
shapiro_p_value <- shapiro_test$p.value

# Print the p-value
print(shapiro_p_value)

# Comment on the p-value
if (shapiro_p_value > 0.05) {
  cat("The p-value is greater than 0.05, suggesting that the null hypothesis of normality cannot be rejected.")
} else {
  cat("The p-value is less than or equal to 0.05, suggesting that the null hypothesis of normality can be rejected.")
}


# 
# 

# Define the significance level
alpha <- 0.05

# The null hypothesis (H0): The mean amount of sugar added is 5 ml
# The alternative hypothesis (H1): The mean amount of sugar added is not 5 ml

# Calculate the test statistic (t-value)
t_test <- t.test(sugar_data$x, mu = 5, alternative = "two.sided")

# Output the test statistic
t_value <- t_test$statistic

# Print the value of the test statistic
print(t_value)

# Find the critical t-value for a two-tailed test at the 5% significance level
critical_t_value <- qt(alpha/2, df=length(sugar_data$x)-1, lower.tail=FALSE)

# Print the critical t-value
print(critical_t_value)

# Conclusion based on the t-test
if (abs(t_value) > critical_t_value) {
  cat("Conclusion: Reject the null hypothesis. There is a significant difference between the amount of sugar I am adding and the 5 ml I am supposed to be adding.")
} else {
  cat("Conclusion: Fail to reject the null hypothesis. There is no significant difference between the amount of sugar I am adding and the 5 ml I am supposed to be adding.")
}
# 
# 
# 
