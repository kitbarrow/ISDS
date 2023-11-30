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
bss_fit = regsubsets(Ladder_score ~ .,
                     data = Happy_general,
                     method = "exhaustive",
                     nvmax = 5)
(bss_summary = summary(bss_fit))
