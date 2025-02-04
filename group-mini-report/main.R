# -----------------------------------------------------------------------------
#   ___ ____  ____  ____
#  |_ _/ ___||  _ \/ ___|
#   | |\___ \| | | \___ \
#   | | ___) | |_| |___) |
#  |___|____/|____/|____/
#
#    ____                         _  ___
#   / ___|_ __ ___  _   _ _ __   / |/ _ \
#  | |  _| '__/ _ \| | | | '_ \  | | | | |
#  | |_| | | | (_) | |_| | |_) | | | |_| |
#   \____|_|  \___/ \__,_| .__/  |_|\___/
#
# -----------------------------------------------------------------------------
# install necessary R packages
# install.packages('leaps')
# install.packages("plyr")
# install.packages("bestglm")
# -----------------------------------------------------------------------------
# clear the environment var area
rm(list = ls())
# clear all plots
graphics.off()
# clear the console area
cat("\014")
# -----------------------------------------------------------------------------
# current_directory
current_directory = getwd()
# read_csv
# joint file path
file_path = file.path(current_directory, "Happy.csv")
Happy_origin = read.csv(file_path)
# -----------------------------------------------------------------------------
# show data
head(Happy_origin)
# Check if we have any missing values
sum(is.na(Happy_origin))
# -----------------------------------------------------------------------------
# Missing Value Handling
# Using mice to insert empty data of State of Palestine
library(mice)
Happy_complete = complete(mice(Happy_origin))
head(Happy_complete)
# -----------------------------------------------------------------------------
Happy_general_continent = Happy_complete
Continent_mapping =
  c(
    "North America" = 1,
    "South America" = 2,
    "Europe" = 3,
    "Asia" = 4,
    "Africa" = 5,
    "Oceania" = 6
  )
Continent_mapping
# -----------------------------------------------------------------------------
# Happy_origin: Data Set download from ultra
# Happy_complete: Complete empty data with mice
# Happy_general_continent: All the data with numeric continent
# Happy_general: All the data with ignored continent
# Africa: Area data
# Asia: Area data
# Europe: Area data
# North_America: Area data
# Oceania: Area data
# South_America: Area data
# -----------------------------------------------------------------------------
Africa = Happy_general_continent[Happy_general_continent$Continent == "Africa",]
Africa$Country_name = NULL
Africa$Continent = NULL
# -----------------------------------------------------------------------------
Asia = Happy_general_continent[Happy_general_continent$Continent == "Asia",]
Asia$Country_name = NULL
Asia$Continent = NULL
# -----------------------------------------------------------------------------
Europe = Happy_general_continent[Happy_general_continent$Continent == "Europe",]
Europe$Country_name = NULL
Europe$Continent = NULL
# -----------------------------------------------------------------------------
North_America = Happy_general_continent[Happy_general_continent$Continent == "North America",]
North_America$Country_name = NULL
North_America$Continent = NULL
# -----------------------------------------------------------------------------
Oceania = Happy_general_continent[Happy_general_continent$Continent == "Oceania",]
Oceania$Country_name = NULL
Oceania$Continent = NULL
# -----------------------------------------------------------------------------
South_America = Happy_general_continent[Happy_general_continent$Continent == "South America",]
South_America$Country_name = NULL
South_America$Continent = NULL
# -----------------------------------------------------------------------------
Happy_general_continent$Numeric_continent = Continent_mapping[Happy_general_continent$Continent]
Happy_general_continent$Continent = NULL
Happy_general_continent$Country_name = NULL
Happy_general_continent = Happy_general_continent[, c(1, 2, 3, 4, 5, 7, 6)]
# -----------------------------------------------------------------------------
Happy_general = Happy_general_continent
Happy_general$Numeric_continent = NULL
# -----------------------------------------------------------------------------
# create scatter plot matrix with pairs()
pairs(Happy_general_continent, main = "General Data with Numeric Continent")
pairs(Happy_general, main = "General Data Scatterplot")
pairs(Africa, main = "Africa Data Scatterplot")
pairs(Asia, main = "Asia Data Scatterplot")
pairs(Europe, main = "Europe Data Scatterplot")
pairs(North_America, main = "North America Data Scatterplot")
pairs(Oceania, main = "Oceania Data Scatterplot")
pairs(South_America, main = "South America Data Scatterplot")
# -----------------------------------------------------------------------------
# calculate the correlation coefficient with pearson method
cor_Happy_general = cor(Happy_general)
# correlation among one continent
cor_Africa = cor(Africa)
cor_Asia = cor(Asia)
cor_Europe = cor(Europe)
cor_North_America = cor(North_America)
cor_Oceania = cor(Oceania)
cor_South_America = cor(South_America)
# -----------------------------------------------------------------------------
# Simple Linear Regression
reg_LGDP = lm(Ladder_score ~ LGDP, data = Happy_general)
summary(reg_LGDP)
# Confidence and prediction intervals
# Regression Diagnostics
par(mfrow = c(2, 2))
plot(reg_LGDP, pch = 16, col = "cornflowerblue")
par(mfrow = c(1, 1))
# -----------------------------------------------------------------------------
# Multiple Linear Regression
lsq_fit = lm(Ladder_score ~ ., data = Happy_general)
lsq_summary = summary(lsq_fit)
fitted_values = predict(lsq_fit, Happy_general)
# -----------------------------------------------------------------------------
# Best Subset Selection
library(leaps)
p =  5
bss_fit = regsubsets(Ladder_score ~ .,
                     data = Happy_general,
                     method = "exhaustive",
                     nvmax = p)
(bss_summary = summary(bss_fit))
# -----------------------------------------------------------------------------
#
#
#
#
#
# -----------------------------------------------------------------------------
reg_fold_error = function(X, y, test_data) {
  Xy = data.frame(X, y=y)
  ## Fit the model to the training data
  if(ncol(Xy)>1) tmp_fit = lm(y ~ ., data=Xy[!test_data,])
  else tmp_fit = lm(y ~ 1, data=Xy[!test_data,,drop=FALSE])
  ## Generate predictions over the test data
  yhat = predict(tmp_fit, Xy[test_data,,drop=FALSE])
  yobs = y[test_data]
  ## Compute the test MSE
  test_error = mean((yobs - yhat)^2)
  return(test_error)
}
# -----------------------------------------------------------------------------
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
                         data = Xy[fold_ind != fold,],
                         method = "exhaustive",
                         nvmax = p)
    best_models = summary(tmp_fit)$which[, 2:(1 + p)]
    # Using the fold as test data, find the test error associated with each of M_1,..., M_p
    for (k in 1:p) {
      fold_errors[fold, k] = reg_fold_error(X[, best_models[k,]], y, fold_ind ==
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
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# Load the bestglm package
library(bestglm)
# Create multi-panel plotting device
par(mfrow = c(2, 2))
# Produce plots, highlighting optimal value of k
(best_adjr2 = which.max(bss_summary$adjr2))
(best_cp = which.min(bss_summary$cp))
(best_bic = which.min(bss_summary$bic))
k = 5
n = nrow(Happy_general)
fold_index = sample(k,n,replace=TRUE)
## Apply the function to the Happy data
bss_mse = reg_bss_cv(Happy_general[, 1:5], Happy_general[, 6], fold_index)
## Identify model with the lowest error
(best_cv = which.min(bss_mse))

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
