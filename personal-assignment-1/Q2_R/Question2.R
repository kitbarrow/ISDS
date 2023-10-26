# clear the console area
cat("\014")
# clear the environment var area
rm(list = ls())
# Load the Tooth Growth data set
data(ToothGrowth)

# Find the modal tooth length
modal_tooth_length = as.numeric(names(sort(
  table(ToothGrowth$len), decreasing = TRUE)[1]))


# Calculate the mean tooth length for guinea pigs 
# given vitamins via orange juice
mean_tooth_length_orange = mean(
  ToothGrowth$len[ToothGrowth$supp == "OJ"])
mean_tooth_length_orange

# Create a side-by-side box-and-whisker plot
# for tooth length by supplement type
boxplot(len ~ supp, data = ToothGrowth, 
        col = c("lightblue", "lightgreen"),
        xlab = "Supplement Type", ylab = "Tooth Length",
        main = "Tooth Length by Supplement Type")

