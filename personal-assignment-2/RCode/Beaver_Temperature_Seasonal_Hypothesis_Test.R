# clear the console area
cat("\014")
# clear the environment var area
rm(list = ls())

# Load the datasets
beaver1
beaver2

# Define the significance level
alpha <- 0.05

# The null hypothesis (H0): There is no difference in mean internal body temperature of beavers between November and December.
# The alternative hypothesis (H1): Beavers have a lower mean internal body temperature in December than in November.

# We will apply a one-tailed t-test since we are specifically interested in whether the mean temperature in December is lower.

# Perform the t-test for independent samples
t_test <- t.test(beaver1$temp, beaver2$temp, alternative = "less")

# The value of the test statistic
t_value <- t_test$statistic

# A bound on the p-value for the test
p_value_bound <- t_test$p.value

# Print the test statistic and the bound on the p-value
print(t_value)
print(p_value_bound)

# Conclusion
if (p_value_bound < alpha) {
  cat("Conclusion: Reject the null hypothesis. There is evidence to suggest that beavers have a lower mean internal body temperature in December than in November.")
} else {
  cat("Conclusion: Fail to reject the null hypothesis. There is not sufficient evidence to suggest that beavers have a lower mean internal body temperature in December than in November.")
}
