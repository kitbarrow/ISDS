# clear the console area
cat("\014")
# clear the environment var area
rm(list = ls())
# clear all plots
graphics.off()
# -----------------------------------------------------------------------------
library(MASS)
data("Boston")
attach(Boston)
# -----------------------------------------------------------------------------
names(Boston)
# Look at the first few rows
head(Boston)
# Summary statistics
summary(Boston)
# -----------------------------------------------------------------------------
# Check for missing values
sum(is.na(Boston))
# -----------------------------------------------------------------------------
#  506 observations and 14 variables
# (b)  
pairs(Boston)
# -----------------------------------------------------------------------------
# (c)
# Add correlation coefficients 
# Correlation panel
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("r = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
# Customize upper panel
upper.panel<-function(x, y){
  points(x,y, pch = 19)
}
# Create the plots
pairs(Boston, 
      lower.panel = panel.cor,
      upper.panel = upper.panel)
# -----------------------------------------------------------------------------
# A nicer plot  using the corrplot package.
#install.packages("corrplot")
library(corrplot)
#checking correlation between variables
corrplot(cor(Boston), method = "number", type = "upper", diag = FALSE)
# -----------------------------------------------------------------------------
# Simple Linear Regression
# Linear regression model fitting
# (a) Use the lm() function to fit a simple linear regression model, with medv as 
# the response variable and lstat as the predictor variable. Save the result 
# of the regression function to reg1.
# (b)Use summary(reg1) command to get information about the model. This gives
# us the estimated coefficients, t-tests, p-values and standard errors as well
# as the R-square and F-statistic for the model.
# (c) Use names(reg1) function to find what types of information are stored in 
# reg1. For example, we can get or extract the estimated coefficients using the
# function coef(reg1) or reg1$coefficients.
# SOLUTION
# (a)
reg1 <- lm(medv~lstat,data=Boston)
# (b)
summary(reg1)
coef(reg1)
# -----------------------------------------------------------------------------
# 2.2 Confidence and prediction intervals
# (a) Obtain a 95% confidence interval for the coefficient estimates. 
# Hint: use the confint() command.
# (b) Obtain a 95% confidence and prediction intervals of medv for a given
# value of lstat. Assume this given value of lstat is equal to 10.
## SOLUTION
# (a)
confint(reg1, level = 0.95)
# (b)
predict(reg1,data.frame(lstat=10), interval="prediction",  level = 0.95)
# -----------------------------------------------------------------------------
# 2.3 Regression diagnostics
# (a) Create a scatterplot of lstat and medv. Is there a relationship between the
# two variables?
# (b) Plot medv and lstat along with the least squares regression line using the
# plot() and abline() functions.
# (c) Use residual plots to check the assumptions of the model.
## SOLUTION
# (a) & (b)
plot(lstat,medv, pch=2, col="cornflowerblue")
abline(reg1,lwd=3,col="red")
# (c)
par(mfrow=c(1,2)) # to have two plots side-by-side
plot(reg1, which=1,  pch=16, col="cornflowerblue")
plot(reg1, which=2,  pch=16, col="cornflowerblue")
# 
par(mfrow=c(1,1)) # to return to one plot per window 
par(mfrow=c(2,2))
plot(reg1, pch=16, col="green")
# -----------------------------------------------------------------------------
# Multiple Linear Regression
# Linear regression model fitting
# (a) Use the lm() function again to fit a linear regression model, with medv as 
# the response variable and both lstat and age as predictors. Save the result 
# of the regression function to reg2.
# (b) Now use the lm() function again to fit a linear regression model, with
# medv as the response variable and all other 13 variables as predictors. Save
# the result of the regression function to reg3. 
# Hint: Use lm(medv~., data=Boston).
# (c) Fit the same linear regression as in (b) but now exclude the age.
# Save the result of the regression function to reg4.
# Hint: Use lm(medv~.-age, data=Boston).

# -----------------------------------------------------------------------------
