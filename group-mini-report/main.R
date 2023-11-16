# -----------------------------------------------------------------------------
# clear the console area
cat("\014")
# clear the environment var area
rm(list = ls())
# clear all plots
graphics.off()
# -----------------------------------------------------------------------------
# current_directory
current_directory <- getwd()
# read_csv
# 拼接文件路径
file_path <- file.path(current_directory, "Happy.csv")
data <- read.csv(file_path)
# show data
head(data)
