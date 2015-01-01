
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
world[world$IncomeGroup == 'Low income', ]
# Afghanistan

# What was the rural population of Aruba in 1970 (report without commas)?
world[world$Country == 'Aruba' & world$year == 1970, ]
# 29164

# When was the first year Australia had data on the number of mobile device subscriptions? (Subscriptions more than 0)
which(world$Country == 'Australia' & world$mobile.users > 0)
# 1987


# Subset data for just the United States and name the new data frame "us"
us <- world[world$Country.Code == "USA", ]

# Select the years from 1990 and name the new data frame "us_select"
us_select <- us[us$year >= 1990, ]

# Make the number of users more interpretable (into millions)
us_select$internet.mil <- us_select$internet.users / 1000000

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
tyr <- Brazil$year -1995

users <- Brazil$mobile.users / 1000000

# Find the number of mobile users in Brazil (in millions) in 2000
users
users[1]
users[6]
# 23.19

# In what year did Brazil first record more than 100 million mobile users?
tyr[users > 100] + 1995

# Which model best describes the increase in mobile users in Brazil since 1995?
tripleFit(tyr, users,xlab='Time (yr)',ylab='Mobile User per Million')

#
logisticFit(tyr, users,xlab='Time (yr)',ylab='Mobile User per Million')

# Using the best‐fitting model, predict the number of mobile users (in millions) in Brazil in 2025. 
logisticFitPred(tyr, users,xlab='Time (yr)',ylab='Mobile User per Million', 2025-1995)

# Q2

# rate of change in flu cases from April 30 to May 1
110/257

# growth rate for the flu, according to the exponential model? 
1.46 - 1

# Predict the number of cases of flu on Day 14 (when "Day" is equal to 14), using the exponential model.
Day <- c(0:9)
Flu <- c(73, 105, 137, 257, 367, 658, 898, 1085, 1490, 1893)
expFitPred(Day, Flu,xlab='Days',ylab='Flu Cases', 14)
logisticFitPred(Day, Flu,xlab='Days',ylab='Flu Cases', 14)
