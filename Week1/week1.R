# clear the console area
cat("\014")
# clear the environment var area
rm(list = ls())
# set work directory
setwd("C:/Users/QianZ/Desktop/ISDS/Week1")
# when read csv file whose delimiter is a comma
# read these three data set
hair = read.csv("./data/hair.csv")
uspop = read.csv("./data/uspop.csv")
stop = read.csv("./data/stop.csv")
# -----------------------------------------------------------------------
# us pop data set processing
# getting mean data
uspop_mean = mean(uspop$Population)
# getting median data
uspop_median = median(uspop$Population)
# summary function
uspop_sum = summary(uspop$Population)
# Calculate the range
uspop_data_range = max(uspop$Population) - min(uspop$Population)
# Calculate the interquartile range (IQR)
uspop_Q1 = quantile(uspop$Population, 0.25)
uspop_Q3 = quantile(uspop$Population, 0.75)
uspop_IQR_value = uspop_Q3 - uspop_Q1
# Create a histogram of the us pop data
# from the hist plot, the us pop data is positive skew
hist(uspop$Population, main = "Histogram of uspop Data", xlab = "Population")
# using var and standard deviation function
uspop_var = var(uspop$Population)
uspop_sd = sd(uspop$Population)
# -----------------------------------------------------------------------
# hair data set processing
hair_colors = c("black","brown","red","gold")
barplot(height = hair$Frequency, names.arg = hair$Color, xlab = "Color", 
        ylab = "Frequency", main = "Hair Color Frequency using Barplot", 
        col=hair_colors)
# Pie Chart Rendering
pie(x = hair$Frequency, labels = hair$Color, col = hair_colors, 
    main = "Hair Color Frequency using Pie Chart")
# -----------------------------------------------------------------------
# stop data set processing
stem(stop$dist)
# define a function to find mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
dist_mode = Mode(stop$dist)
# Calculate quintile values (20th, 40th, 60th, and 80th percentiles)
quintiles = quantile(stop$dist, probs = c(0.2, 0.4, 0.6, 0.8))

