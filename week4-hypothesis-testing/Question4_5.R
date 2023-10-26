# clear the console area
cat("\014")
# clear the environment var area
rm(list = ls())

data(Guyer)

# (a) Test for gender differences in cooperation
# H0: Men and women are equally likely to cooperate
# H1: Men and women are not equally likely to cooperate
gender_cooperation_test <-
  t.test(cooperation ~ sex, data = Guyer, alternative = "two.sided")
#
# Perform a normality test for each group
normality_test_male <-
  shapiro.test(Guyer$cooperation[which(Guyer$sex == "male")])
normality_test_female <-
  shapiro.test(Guyer$cooperation[which(Guyer$sex == "female")])
#
# Print the normality test results
print("Normality Test for Men:")
print(normality_test_male)
print("Normality Test for Women:")
print(normality_test_female)
#
#
#
#
#
# (b) Test for anonymity effect on cooperation
# H0: Cooperation is equally likely under anonymity and public conditions
# H1: Cooperation is less likely under anonymity than public conditions
anonymity_cooperation_test <-
  t.test(cooperation ~ condition,
         data = Guyer,
         alternative = "less")

# Perform a normality test for each group
normality_test_anonymous <-
  shapiro.test(Guyer$cooperation[which(Guyer$condition == "anonymous")])
normality_test_public <-
  shapiro.test(Guyer$cooperation[which(Guyer$condition == "public")])

# Print the normality test results
print("Normality Test for Anonymous Conditions:")
print(normality_test_anonymous)
print("Normality Test for Public Conditions:")
print(normality_test_public)