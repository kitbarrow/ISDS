getwd()
setwd("C:\\Users\\kitba\\OneDrive - Durham University\\Documents\\University\\Data Science\\Introduction to Statistics for Data Science\\Mini_Project")
Happy<-read.csv("./Happyedt.csv")
reg<-lm(Ladder_score~.-Country.name,data=Happy)
Happy$Continent
sum(is.na(Happy))
#Palestine has no HLE
#Happy$Continent<-as.numeric(Happy$Continent)
# Create a mapping of continents to numeric values
Continent_mapping <- c("North America" = 1, "South America" = 2, "Europe" = 3, "Asia" = 4, "Africa" = 5, "Oceania" = 6)
Continent_mapping
# Use the mapping to create a new column with numeric values
Happy$numeric_continent <- Continent_mapping[Happy$Continent]
pairs(Happy2)
# Print the result
print(Happy)
Happy2<-Happy
Happy2$Continent<-NULL
Happy2$Country.name<-NULL
Happy2
MultReg<-lm(Ladder_score~.-Country.name-Continent,data=Happy)
plot(MultReg)
reg2<-lm(Ladder_score~numeric_continent,data=Happy2)
plot(Happy2$numeric_continent,Happy2$Ladder_score)
abline(reg2)

#k-means clustering for Palestine
install.packages('factoextra')
install.packages('cluster')
library(factoextra)
library(cluster)
Happy3<-Happy
Happy3$HLE<-NULL
Happy3$Country.name<-NULL
head(Happy3)
kmeans(Happy3,)