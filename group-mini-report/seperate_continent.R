library(mice)
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
file_path <- file.path(current_directory, "Happyedt.csv")
Happy <- read.csv(file_path)

data_asia <- Happy[Happy$Continent == "Asia", ]
