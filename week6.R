
library(SDSFoundations) 

gbr <- WorldBankData[WorldBankData$Country.Code == 'GBR', ]

gbr2000 <- gbr[gbr$year >= 2000 & gbr$year < 2010, ]

time <- gbr2000$year - 2000

mv <- gbr2000$motor.vehicles

plot(time, mv)

# Exponential and Logistic Models

expFit(time, mv)

logisticFit(time, mv)

tripleFit(time, mv)

expFitPred(time, mv, 12)

logisticFitPred(time, mv, 12)

## Week Pre-Lab

world <- WorldBankData 

# What is the first "Low Income" country in the dataset?
head(world[world$IncomeGroup == 'Low income', ], 10)
# Afghanistan

# What was the rural population of Aruba in 1970 (report without commas)?
world[world$Country == 'Aruba' & world$year == 1970, ]
# 29164

# When was the first year Australia had data on the number of mobile device subscriptions? (Subscriptions more than 0)
yr <- which(world$Country == 'Australia' & world$mobile.users > 0) 
world[yr[1], ]
# 1987


# Subset data for just the United States and name the new data frame "us"
us <- world[world$Country.Code == "USA", ]

# Select the years from 1990 and name the new data frame "us_select"
us_select <- us[us$year >= 1990, ]

# Make the number of users more interpretable (into millions)
us_select$internet.mil <- us_select$internet.users / 1e6

# Create a new variable that is "years since 1990"
us_select$time <- us_select$year - 1990

# Select the first 10 years (from 1990 to 1999) and name the new data frame "us_select_10"
us_select_10 <- us_select[us_select$time < 10, ]

# Use a function to fit an exponential and logistic model for 1990-1999
expFit(us_select_10$time, us_select_10$internet.mil,xlab='Time (yr)',ylab='Internet User per Million')

logisticFit(us_select_10$time, us_select_10$internet.mil,xlab='Time (yr)',ylab='Internet User per Million')

# Based on the prior model parameters, predict the number of internet users in 2006
e <- expFitPred(us_select_10$time, us_select_10$internet.mil, 16)

l <- logisticFitPred(us_select_10$time, us_select_10$internet.mil, 16)

# Show how many internet users the US actually had in 2006
us_select[us_select$time == 16, c("Country", "year", "internet.mil")]

# Calculate the residuals for each model
us_select$internet.mil[us_select$time == 16] - e

us_select$internet.mil[us_select$time == 16] - l

# Look at the model fits for all available data (1990 to 2012)
expFit(us_select$time, us_select$internet.mil,xlab='Time (yr)',ylab='Internet User per Million')

logisticFit(us_select$time, us_select$internet.mil,xlab='Time (yr)',ylab='Internet User per Million')

# Which model fits the best?
tripleFit(us_select$time, us_select$internet.mil,xlab='Time (yr)',ylab='Internet User per Million')

# How many internet users would the US have had in 2012 if you had used the original exponential model?
expFitPred(us_select_10$time, us_select_10$internet.mil, 22)

# Week 6 Lab

# Create a subset of the data that only contains data from 1990 onward.
subset <- world[world$year >= 1990, ]

# Create two new data frames --- one for each country of interest.
Denmark <- subset[subset$Country == 'Denmark', ]

Belarus <- subset[subset$Country == 'Belarus', ]

# Create a new variable that is "years since 1990". 
time <- Denmark$year - 1990

# Determine the best-fitting model (exponential or logistic) for internet usage in each country from 1990 onward.
dmk <- Denmark$internet.users / Denmark$population 

expFit(time, dmk, xlab='Time (yr)', ylab='Internet User per Population')

logisticFit(time, dmk, xlab='Time (yr)', ylab='Internet User per Population')

brs <- Belarus$internet.users / Belarus$population 

expFit(time, brs, xlab='Time (yr)', ylab='Internet User per Population')

logisticFit(time, brs, xlab='Time (yr)', ylab='Internet User per Population')

# Using the logistic model equations from your analysis, calculate the YEAR that 10% of the population in each country would be using the internet.
t1 <- log(308.8345/(0.89668*10 - 1))/log(1.73124)
t1 + 1990

t2 <- log(308.8345/(0.89668/0.8 - 1))/log(1.73124)
t2 + 1990

# Using the logistic model equations from your analysis, calculate the YEAR that 80% of the population in each country would be using the internet.
t3 <- log(422.4322/(0.8987*10 - 1))/log(1.31884)
t3 + 1990

t4 <- log(422.4322/(0.8987/0.8 - 1))/log(1.31884)
t4 + 1990

# Week 6 Problem Set Q1

# Create a subset of the world bank data that contains records from Brazil 1995 and later.
Brazil <- world[world$Country == 'Brazil' & world$year >= 1995, ]

# Change the year variable to be "years since 1995" and update the units of the "mobile.users" variable to millions of users.
tyr <- Brazil$year - 1995

users <- Brazil$mobile.users / 1e6

# Find the number of mobile users in Brazil (in millions) in 2000
users[1]
users[2000 - 1995 + 1]
# 23.19

# In what year did Brazil first record more than 100 million mobile users?
tyr[users > 100] + 1995

# Which model best describes the increase in mobile users in Brazil since 1995?
tripleFit(tyr, users,xlab='Time (yr)',ylab='Mobile User per Million')

# What proportion of the variation in mobile users is explained by years since 1995 in the best‐fitting model?
logisticFit(tyr, users,xlab='Time (yr)',ylab='Mobile User per Million')
# R-squared =  0.99785

# Using the best-fitting model, predict the number of mobile users (in millions) in Brazil in 2025. 
logisticFitPred(tyr, users,xlab='Time (yr)',ylab='Mobile User per Million', 2025-1995)
# 345.427

# Q2

Day <- c(0:9)
Flu <- c(73, 105, 137, 257, 367, 658, 898, 1085, 1490, 1893)

# rate of change in flu cases from April 30 to May 1
(367 - 257)/257

# growth rate for the flu, according to the exponential model? 
expFit(Day, Flu, xlab='Days', ylab='Flue Cases')
1.46 - 1   # b =  1.46096

# Predict the number of cases of flu on Day 14 (when "Day" is equal to 14), using the exponential model.
expFitPred(Day, Flu,xlab='Days',ylab='Flu Cases', 14)
# 15466.356
logisticFitPred(Day, Flu,xlab='Days',ylab='Flu Cases', 14)
# 3036.525

# Use the provided approximate model constants in the table, not to calculate the precise model constants using the raw data.
a <- 76.64
b <- 1.46
t <- 14
a*b^t
# [1] 15324.82

# Using the logistic model, predict the total number of flu cases on Day 14.
C <- 3273.31
a <- 43.59
b <- 1.57
C/( 1 + a*b^(-t) )
# [1] 3034.078

# The actual number of flu cases on Day 14 was 4,379. Find the residual of the exponential model prediction.
4379 - 15325 

# What is the residual of the logistic model prediction for Day 14?
4379 - 3034

# Q3

# how many wolves were being added to the park each year?

yr <- c(1,3)
Nw <- c(25,45)
linFit(yr, Nw, xlab='Yr', ylab='Numbers')
(45-25)/(3-1)
# Slope =  10

# According to their linear model, what was the size of the original wolf population when the project began?
25 - 10
# Intercept =  15

# Fit an exponential model to this data. What is the growth factor for his model?
expFit(yr, Nw, xlab='Yr', ylab='Numbers')
# b =  1.34164   growth factor = 1 +  growth rate

# What is the annual growth rate of these wolves each year, according to this model?
1.34164 - 1

# Assuming exponential growth, find the initial number of wolves when the project began. 
18.6339

# By 2002, there were 147 wolves in Yellowstone Park. Which model was determined to fit the data better?
linFitPred(yr, Nw, xlab='Yr', ylab='Numbers', 2002-1995)
# Predicted value 85

# Residual of linFit
147 - 85

expFitPred(yr, Nw, xlab='Yr', ylab='Numbers', 2002-1995)
# Predicted value 145.8

# Residual of expFit
147 - 145.8

yr <- c(1,3,7)
Nw <- c(25,45,147)
linFit(yr, Nw, xlab='Yr', ylab='Numbers')
# R-squared =  0.96793

expFit(yr, Nw, xlab='Yr', ylab='Numbers')
# R-squared =  1

# Using the best fitting model, how many years must pass before there are more than 325 wolves in Yellowstone? 
a <- 18.58487
b <- 1.34361
t <- log10(325/a)/log10(b)  # a*b^t
# [1] 9.6881

expFitPred(yr, Nw, xlab='Yr', ylab='Numbers', 10)
# Predicted : 356.352

expFitPred(yr, Nw, xlab='Yr', ylab='Numbers', 9)
# Predicted : 265.22

# Q4

C <- 2000
a <- 152.10
b <- 2.17

# What was the size of the hedgehog population when the growth rate began to slow down?
C/2

# How many years had passed when the population growth rate began to slow down?
log10(a)/log10(b)

# The hedgehogs were released in South Austin in 2001. How many hedgehogs were living in South Austin by 2010, according to
# the model?
t <- 2010-2001
C/(1 + a*b^(-t))
# [1] 1750.458

