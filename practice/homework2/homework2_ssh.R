# Basic for Quantitaive
# homework 02
# name : 손승한
# student number : 2021711370

# Question 1 : 

# --- Data Setting start ---

# change directory
getwd()
setwd("/Users/thorp/R_script/R-Basic-for-Quantitaive/practice/homework2")

# Input file : 401ksubs_description.txt
desc = readLines("401ksubs_description.txt")

# extracts column names from _description.txt
count = 0
column_name = NULL

for(row in desc){
  
  if(row == ""){
    count = count + 1
  }
  if(count == 1){
    column_name <- append(column_name, unlist(strsplit(row, " ")))
  }else if(count == 2){
    break
  }
}

column_name = column_name[!(column_name %in% c(""))] # vector에서 "" 값 삭제.

# Install & Import package : readxl
install.packages("readxl")
library("readxl")

# Input file : 401ksubs.xls
data_set <- read_excel(
  "401ksubs.xls", sheet = "401ksubs",
  col_names = column_name, skip = 0 # (skip = 0)
)

# --- Data Setting end ---

# Problem 1: How many married couples without children at home are in the data set?

couples_without_children = subset(data_set, marr==1 & fsize==2)
couples_without_children_count = nrow(couples_without_children)
sprintf("couples without children : %d", couples_without_children_count)


# Problem 2: Use OLS to estimate the model

ols = lm(nettfa ~ I(inc + age), data = data_set)
ols2 = lm(nettfa ~ inc, data = data_set)
ols3 = lm(nettfa ~ age, data = data_set)

summary(ols)
summary(ols2)
summary(ols3)
par(mfrow = c(3,1))

plot(nettfa ~ I(inc + age), data = data_set)
abline(ols)
plot(nettfa ~ inc, data = data_set)
abline(ols2)
plot(nettfa ~ age, data = data_set)
abline(ols3)
# coefficient estimates: 0.96722
# Multiple R-squared: 0.1689
# Interpret the slope coefficients: 
# Are there any surprises in the slope estimates?

# Problem 3: 
# Is age statistically significant?
# inc+age Multiple R-squared : 0.1689
# inc     Multiple R-squared : 0.1418
# 연령은 통계적으로 유의합니다. 왜냐하면, 나이를 더했을 경우 R-squared가 0.0271 증가하기 때문입니다.

# Obtain the 95% confidence interval of 𝛽2

# Problem 4: 
# Does the intercept from the regression in part (2) have an interesting meaning? Explain


# Problem 5: Find the p value for the test H0: 𝛽2=1 against H1: 𝛽2≠1. Do you reject the null hypothesis at the 1% significant level?


# Problem 6: Find the p value for the test H0: 𝛽2=1 against H1: 𝛽2>1. Do you reject the null hypothesis at the 1% significant level?


intercept <- ols$coefficients[[1]]
sprintf("Intercept of the equation : %f", intercept)
coefficient <- ols$coefficients[[2]]
sprintf("Coefficient of the equation : %f", coefficient)
