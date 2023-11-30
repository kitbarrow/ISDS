library(mice)
# -----------------------------------------------------------------------------
# clear the console area
cat("\014")
# clear the environment var area
rm(list = ls())
# clear all plots
graphics.off()
#sets the working directory

# install.packages("plyr")

## Load the durhamSLR package
library(durhamSLR)
# -----------------------------------------------------------------------------
## Load original excel file
# current_directory
current_directory <- getwd()
# read_csv
# joint file path
file_path <- file.path(current_directory, "Happyedt.csv")
Happy <- read.csv(file_path)
# -----------------------------------------------------------------------------

#check if the continents have any inherent ordering
Happy$Continent

#Check if we have any missing values
sum(is.na(Happy))
#Palestine has no HLE

# Create a mapping of continents to numeric values
Continent_mapping <-
  c(
    "North America" = 1,
    "South America" = 2,
    "Europe" = 3,
    "Asia" = 4,
    "Africa" = 5,
    "Oceania" = 6
  )
Continent_mapping

#Assign this mapping to the dataset
Happy$numeric_continent <- Continent_mapping[Happy$Continent]
print(Happy)

#create a new database to be modified
Happy2 <- Happy
library(dplyr)

#remove Palestine, will use MICE in actual report
Happy2 <- slice(Happy2, -40)
Happy2

#Remove country names and continent names as these will confuse R
Happy2$Continent <- NULL
Happy2$Country.name <- NULL

Happy2


## Check size
dim(Happy2)

## Store dimensions, n and p:
(n = nrow(Happy2))
(p = ncol(Happy2) - 1)
## Print first few rows
head(Happy2)

## Function to compute the sum of squares
sumofsquares = function(x)
  sum(x ^ 2)

## Loop over columns
ss = numeric(7)
for (i in 1:7) {
  ss[i] = sumofsquares(Happy2[, i])
}
ss

#Use pairs and cor for initial idea of correlation
pairs(Happy2)
cor(Happy2)

## Fit model using least squares
lsq_fit = lm(Ladder_score ~ ., data = Happy2)

## Summarise fitted model
(lsq_summary = summary(lsq_fit))

## Compute the fitted values:
yhat = predict(lsq_fit, Happy2)
## Print first few elements:
head(yhat)

## Compare with actual values:
head(Happy2$Ladder_score)

## Compute the fitted values: did this above and they are stored in yhat
## Compute training error:
(training_error = mean((Happy2$Ladder_score - yhat) ^ 2))

#k-fold cross-validation
reg_fold_error = function(X, y, test_data) {
  Xy = data.frame(X, y = y)
  ## Fit the model to the training data
  if (ncol(Xy) > 1)
    tmp_fit = lm(y ~ ., data = Xy[!test_data, ])
  else
    tmp_fit = lm(y ~ 1, data = Xy[!test_data, , drop = FALSE])
  ## Generate predictions over the test data
  yhat = predict(tmp_fit, Xy[test_data, , drop = FALSE])
  yobs = y[test_data]
  ## Compute the test MSE
  test_error = mean((yobs - yhat) ^ 2)
  return(test_error)
}

df = data.frame(y = rnorm(3))
df
df[1:3, , drop = FALSE]
## Set the seed to make the analysis reproducible
set.seed(1)

## 10-fold cross validation
k = 7
## Sample fold-assignment index
fold_index = sample(k, n, replace = TRUE)
## Print first few fold-assignments
head(fold_index)
c(3, 4, 2, 1, 4, 3, 3, 1) == 1
## We can also assign the output to a variable so it can be stored:
demo = c(3, 4, 2, 1, 4, 3, 3, 1) == 1
demo

regression_cv = function(X, y, fold_ind) {
  p = ncol(X)
  Xy = cbind(X, y = y)
  nfolds = max(fold_ind)
  if (!all.equal(sort(unique(fold_ind)), 1:nfolds))
    stop("Invalid fold partition.")
  fold_errors = numeric(nfolds)
  # Compute the test MSE for each fold
  for (fold in 1:nfolds) {
    test_data = fold_ind == fold
    fold_errors[fold] = reg_fold_error(X, y, test_data)
  }
  # Find the fold sizes
  fold_sizes = numeric(nfolds)
  for (fold in 1:nfolds)
    fold_sizes[fold] = length(which(fold_ind == fold))
  # Compute the average test MSE across folds
  test_error = weighted.mean(fold_errors, w = fold_sizes)
  # Return the test error
  return(test_error)
}

(test_MSE = regression_cv(Happy2[, 1:p], Happy2[, p + 1], fold_index))

general_cv = function(X, y, fold_ind, fold_error_function) {
  p = ncol(X)
  Xy = cbind(X, y = y)
  nfolds = max(fold_ind)
  if (!all.equal(sort(unique(fold_ind)), 1:nfolds))
    stop("Invalid fold partition.")
  fold_errors = numeric(nfolds)
  # Compute the test error for each fold
  for (fold in 1:nfolds) {
    test_data = fold_ind == fold
    fold_errors[fold] = fold_error_function(X, y, test_data)
  }
  # Find the fold sizes
  fold_sizes = numeric(nfolds)
  for (fold in 1:nfolds)
    fold_sizes[fold] = length(which(fold_ind == fold))
  # Compute the average test error across folds
  test_error = weighted.mean(fold_errors, w = fold_sizes)
  # Return the test error
  return(test_error)
}

(test_MSE = general_cv(Happy2[, 1:p], Happy2[, p + 1], fold_index, reg_fold_error))


# install.packages('leaps')
library(leaps)

## Apply the best subset selection algorithm
bss_fit = regsubsets(Ladder_score ~ .,
                     data = Happy2,
                     method = "exhaustive",
                     nvmax = p)

## Summarise the results
(bss_summary = summary(bss_fit))
str(bss_summary)
bss_summary$which
(predictor_indicator = bss_summary$which[, 2:(p + 1)])

## Pick out the first few observations on the p predictor variables
(demo = Happy2[1:3, 1:p])

## Select only the predictor variables that appear in, say, M_2
demo[1:3, predictor_indicator[2, ]]

## Select only the predictor variables that appear in, say, M_5
demo[1:3, predictor_indicator[5, ]]
bss_summary$adjr2
bss_summary$cp
bss_summary$bic
(best_adjr2 = which.max(bss_summary$adjr2))
(best_cp = which.min(bss_summary$cp))
(best_bic = which.min(bss_summary$bic))

## Create multi-panel plotting device
par(mfrow = c(2, 2))

## Produce plots, highlighting optimal value of k
plot(
  1:p,
  bss_summary$adjr2,
  xlab = "Number of predictors",
  ylab = "Adjusted Rsq",
  type = "b"
)
points(best_adjr2,
       bss_summary$adjr2[best_adjr2],
       col = "red",
       pch = 16)
plot(1:p,
     bss_summary$cp,
     xlab = "Number of predictors",
     ylab = "Cp",
     type = "b")
points(best_cp, bss_summary$cp[best_cp], col = "red", pch = 16)
plot(1:p,
     bss_summary$bic,
     xlab = "Number of predictors",
     ylab = "BIC",
     type = "b")
points(best_bic,
       bss_summary$bic[best_bic],
       col = "red",
       pch = 16)

coef(bss_fit, 3)

reg_bss_cv = function(X, y, fold_ind) {
  p = ncol(X)
  Xy = cbind(X, y = y)
  nfolds = max(fold_ind)
  if (!all.equal(sort(unique(fold_ind)), 1:nfolds))
    stop("Invalid fold partition.")
  fold_errors = matrix(NA, nfolds, p)
  for (fold in 1:nfolds) {
    # Using all *but* the fold as training data, find the best-fitting models with 1, ..., p
    # predictors, i.e. M_1, ..., M_p
    tmp_fit = regsubsets(y ~ .,
                         data = Xy[fold_ind != fold, ],
                         method = "exhaustive",
                         nvmax = p)
    best_models = summary(tmp_fit)$which[, 2:(1 + p)]
    # Using the fold as test data, find the test error associated with each of M_1,..., M_p
    for (k in 1:p) {
      fold_errors[fold, k] = reg_fold_error(X[, best_models[k, ]], y, fold_ind ==
                                              fold)
    }
  }
  # Find the fold sizes
  fold_sizes = numeric(nfolds)
  for (fold in 1:nfolds)
    fold_sizes[fold] = length(which(fold_ind == fold))
  # For each of M_0, M_1, ..., M_p, compute the average test error across folds
  test_errors = numeric(p)
  for (k in 1:p) {
    test_errors[k] = weighted.mean(fold_errors[, k], w = fold_sizes)
  }
  # Return the test error for models M_1, ..., M_p
  return(test_errors)
}



## Apply the function to the Happy data
bss_mse = reg_bss_cv(Happy2[, 1:6], Happy2[, 7], fold_index)
## Identify model with the lowest error
(best_cv = which.min(bss_mse))


## Create multi-panel plotting device
par(mfrow = c(2, 2))
## Produce plots, highlighting optimal value of k
plot(
  1:p,
  bss_summary$adjr2,
  xlab = "Number of predictors",
  ylab = "Adjusted Rsq",
  type = "b"
)
points(best_adjr2,
       bss_summary$adjr2[best_adjr2],
       col = "red",
       pch = 16)
plot(1:p,
     bss_summary$cp,
     xlab = "Number of predictors",
     ylab = "Cp",
     type = "b")
points(best_cp, bss_summary$cp[best_cp], col = "red", pch = 16)
plot(1:p,
     bss_summary$bic,
     xlab = "Number of predictors",
     ylab = "BIC",
     type = "b")
points(best_bic,
       bss_summary$bic[best_bic],
       col = "red",
       pch = 16)
plot(1:p,
     bss_mse,
     xlab = "Number of predictors",
     ylab = "10-fold CV Error",
     type = "b")
points(best_cv, bss_mse[best_cv], col = "red", pch = 16)

##Sample 8-fold matrices

## Create matrix to store the fold assignments:
fold_indices = matrix(NA, 8, n)
## Sample the fold assignments:
for (i in 1:8)
  fold_indices[i, ] = sample(k, n, replace = TRUE)

## Create a matrix to store the test errors:
bss_mses = matrix(NA, 8, p)
## Calculate the test errors for the p models for each fold assignment:
for (i in 1:8)
  bss_mses[i, ] = reg_bss_cv(Happy2[, 1:6], Happy2[, 7], fold_indices[i, ])
## Identify the best model in each case:
best_cvs = apply(bss_mses, 1, which.min)

par(mfrow = c(1, 1))
plot(1:p,
     bss_mses[1, ],
     xlab = "Number of predictors",
     ylab = "10-fold CV Error",
     type = "l")
points(best_cvs[1], bss_mses[1, best_cvs[1]], pch = 16)
for (i in 2:8) {
  lines(1:p, bss_mses[i, ], col = i)
  points(best_cvs[i], bss_mses[i, best_cvs[i]], pch = 16, col = i)
}

