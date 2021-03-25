# Basic for Quantitaive
# homework 01
# name : 손승한
# student number : 2021711370

# Question 1 : U.S. CEO Annual Salary Analysis

# change directory
getwd()
setwd("/Users/thorp/R_script/R-Basic-for-Quantitaive/practice/homework1")

# Input file : CEOSAL2_description.txt
ceosal2_desc = readLines("CEOSAL2_description.txt")

# extracts column names from _description.txt
count = 0
column_name = NULL

for(row in ceosal2_desc){
  
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

# Input file : ceosal2.xls
ceosal2 <- read_excel(
  "ceosal2.xls", sheet = "CEOSAL2",
  col_names = column_name, skip = 0 # (skip = 0)
)

# Problem 1: Find the average salary and the average tenure in the sample.
# colMeans(ceosal2)
average_salary <- mean(ceosal2$salary)
sprintf("average salary of salary: %f($1000)", average_salary)
average_tenure <- mean(ceosal2$ceoten)
sprintf("average tenure of ceo: %f(years)", average_tenure)


# Problem 2: 
# How many CEOs are in their first year as CEO (that is,ceoten =0)? 
# What is the longest tenure as a CEO?
ceo_first_tenure <- ceosal2[ceosal2$ceoten == 0,]
ceo_first_tenure_count <- nrow(ceo_first_tenure)
sprintf("first year as CEO: %d(people)", ceo_first_tenure_count)

ceosal2[order(-ceosal2$ceoten),][1,]["ceoten"][,1] #정렬 후 첫행에서 추출.
# 데이터에서 최대를 갖는 색인의 위치찾아 추출.
ceo_longest_tenure <- ceosal2[which.max(ceosal2$ceoten),]["ceoten"][,1]$ceoten
ceo_longest_tenure
sprintf("ceo's longest tunure is : %d(year)", ceo_longest_tenure)


# Problem 3:
# Estimate the simple regression model 
# log(salary) = \beta_0 + \beta_1ceoten + u
ols = lm(log(salary) ~ ceoten, data=ceosal2)
summary(ols)
par(mfrow = c(1,1))
plot(log(salary) ~ ceoten, data=ceosal2)
abline(ols)

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.15314 -0.38319 -0.02251  0.44439  1.94337 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 6.505498   0.067991  95.682   <2e-16 ***
#   ceoten      0.009724   0.006364   1.528    0.128    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.6038 on 175 degrees of freedom
# Multiple R-squared:  0.01316,	Adjusted R-squared:  0.007523 
# F-statistic: 2.334 on 1 and 175 DF,  p-value: 0.1284

# Question 1 : U.S. CEO Annual Salary Analysis

# Problem 1: Find the average salary and the average tenure in the sample.
# - average salary of salary: 865.864407($1000)
# - average tenure of ceo: 7.954802(years)

# Problem 2: How many CEOs are in their first year as CEO (that is,ceoten =0)? 
# - first year as CEO: 5(people)
# - ceo's longest tunure is : 37(year)

# Problem 3: Estimate the simple regression model 
# - coefficient estimates: 6.505498
# - standard errors: 0.6038
# - Multiple R-squared:  0.01316
# - predicted percentage increase: 0.9724%

#####

# Question 2 : Analyze the relationship between participation and generosity in 401(k) pension plans.

# ## 데이터에 대한 이해를 위한 궁금점?
# 
# 개요 : 401k는 미국의 확정 기여형 퇴직 연금으로 회사가 직원을 위해 추가로 저축을 해 줄수 있다.
# 
# 직원이 저축한 금액은 저축한 년도에 소득 공제를 받고, 59.5세 이후엔 패널티 없이 인출 가능함.
# 
# ### 데이터 해석
# 
# 1. prate(참여율) : totpart/totelg*100(%) : 참가자/적용대상자
# 2. mrate(회사의 최대 기여도(율)) : 연봉 100,000$, mrate :5%인 경우, 최대 5000$까지 저축 시 회사에서 5000$ 받을 수 있음.
# 3. totpart : 참가한 총 인원
# 4. totelg : 401k 혜택을 받을 수 있는 인원
# 5. age : 401k 플랜이 제공된 기간.
# 6. totemp : 회사 직원의 총 인원수
# 7. sole : 401k 플랜 외 다른 플랜이 있는가?
# 8. ltotemp : 직원 총 수의 자연로그 ln(totemp)

# Input file : 401K_description.txt
k401_desc = readLines("401K_description.txt")

# extracts column names from _description.txt
count = 0
column_name_401k = NULL

for(row in k401_desc){
  
  if(row == ""){
    count = count + 1
  }
  if(count == 1){
    column_name_401k <- append(column_name_401k, unlist(strsplit(row, " ")))
  }else if(count == 2){
    break
  }
}

# vector에서 "" 값 삭제.
column_name_401k = column_name_401k[!(column_name_401k %in% c(""))] 

# Install & Import package : readxl
install.packages("readxl")
library("readxl")

# Input file : 401k.xls
k401 <- read_excel(
  "401K.xls", sheet = "401K",
  col_names = column_name_401k, skip = 0 # (skip = 0)
)


# Problem 1: 
# Find the average participation rate and the average match rate in the sample of plans.

average_prate = mean(k401$prate)
average_mrate = mean(k401$mrate)
sprintf("average participation rate : %f(%%)", average_prate)
sprintf("average match rate : %f(%%)", average_mrate)


# Problem 2:
# Estimate the simple regression model 
# prate = \beta_0 + \beta_1mrate + u
ols2 = lm((prate) ~ (mrate), data=k401)
summary(ols2)
par(mfrow = c(1,1))
plot((prate) ~ (mrate), data=k401)
abline(ols2)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -82.303  -8.184   5.178  12.712  16.807 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  83.0755     0.5633  147.48   <2e-16 ***
#   mrate         5.8611     0.5270   11.12   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 16.09 on 1532 degrees of freedom
# Multiple R-squared:  0.0747,	Adjusted R-squared:  0.0741 
# F-statistic: 123.7 on 1 and 1532 DF,  p-value: < 2.2e-16


# - coefficient estimates: 83.0755
# - standard errors: 16.09
# - Multiple R-squared:  0.0747

# Problem 3:
intercept <- ols2$coefficients[[1]]
sprintf("Intercept of the equation : %f", intercept)
coefficient <- ols2$coefficients[[2]]
sprintf("Coefficient of the equation : %f", coefficient)

# Problem 4:
mrate35 <- coefficient * (3.5) + intercept
sprintf("predicted prate when mrate=3.5 : %f",mrate35)
# p-value가 2.2e^16으로 0.05보다 낮기 때문에 귀무가설 기각하기 때문에 예측력이 떨어집니다.


# Problem 5:
# Multiple R-squared가 0.0747으로 7.47% 데이터만 모델에 의해 설명되고,  92.53% 데이터는 회귀식으로 설명할 수 없는 error로 추정치의 신뢰도가 낮습니다.


