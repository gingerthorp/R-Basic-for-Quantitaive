# Basic for Quantitaive
# homework 02
# name : 손승한
# student number : 2021711370

# Install & Import package : readxl
install.packages("readxl")
library("readxl")

# change directory
setwd("/Users/thorp/R_script/R-Basic-for-Quantitaive/practice/homework2")

cols_name = c('e401k', 'inc', 'marr', 'male', 'age', 'fsize', 'nettfa', 'p401k', 'pira', 'incsq', 'agesq')

data_set <- read_excel("401ksubs.xls", col_names = cols_name)
# For this quest ion, use only the data for married people without children living at home ( marr =1, fsize =2)
data_set <- subset(data_set, marr==1 & fsize==2)

# Problem 1: How many married couples without children at home are in the data set?
couples_without_children_count = nrow(data_set)
sprintf("couples without children : %d", couples_without_children_count)
# couples without children : 1494

# Problem 2: Use OLS to estimate the model
ols = lm(nettfa ~ inc + age, data = data_set)
summary(ols)
# Intercept: -104.40455
# Estimate-inc: 1.30783
# Estimate-age: 1.66470
# Multiple R-squared: 0.2021
# Adjusted R-squared: 0.201

# Problem 3: 
confint(ols, 'age', 0.95)
#        2.5 %   97.5 %
# age 1.334314 1.995085

# Problem 5:
p_value_two = 2 * (1 - pt((1.66470-1)/0.16843, couples_without_children_count - 2))
p_value_two # 8.300755e-05

# Problem 6:
p_value_one = (1 - pt((1.66470-1)/0.16843, couples_without_children_count - 2))
p_value_one # 4.150377e-05
