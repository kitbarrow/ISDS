# -----------------------------------------------------------------------------
## Load the  durhamSLR package
library(durhamSLR)
## Load the data
data(diabetes)
## Check size
dim(diabetes)
# -----------------------------------------------------------------------------
head(diabetes)
# -----------------------------------------------------------------------------
## Function to compute the sum of squares
sumofsquares = function(x) sum(x^2)
## Loop over columns
ss = numeric(10)
for(i in 1:10) {
  ss[i] = sumofsquares(diabetes[,i])
}
ss
# -----------------------------------------------------------------------------
pairs(diabetes)