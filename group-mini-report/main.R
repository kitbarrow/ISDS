# -----------------------------------------------------------------------------
# Data Set we use
# Happy_origin
# Happy_complete
# Happy_general_continent
# Happy_general
# Africa
# Asia
# Europe
# North_America
# Oceania
# South_America
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
# 
# 
# 
# 
# 
# 
# 
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
# -----------------------------------------------------------------------------
Happy_general = Happy_general_continent
Happy_general$Numeric_continent = NULL
# -----------------------------------------------------------------------------
