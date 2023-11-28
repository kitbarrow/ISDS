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
# read_csv
# joint file path
file_path <- file.path(current_directory, "Happyedt.csv")
Happy <- read.csv(file_path)
# -----------------------------------------------------------------------------
# show data0
head(Happy)
# -----------------------------------------------------------------------------
Happy <- mice(Happy)
Happy <- complete(Happy)
head(Happy)
# -----------------------------------------------------------------------------
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
Happy$numeric_continent <- Continent_mapping[Happy$Continent]
# -----------------------------------------------------------------------------
Happy$Continent <- NULL
Happy$Country.name <- NULL
# -----------------------------------------------------------------------------
