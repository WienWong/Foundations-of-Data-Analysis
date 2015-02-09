
# Week 8 ONE-SAMPLE T TESTS Tutorial 

# In this lab you will be working with data from the Professional Bull Riders Association.
# 
# 1. Open RStudio. Make sure you've installed the SDSFoundations package.
# 2. Type library(SDSFoundations) This will automatically load the data for the labs. 
# 3.Type bull <- BullRiders This will assign the data to your Workspace.
# 
# Alternatively, you can use follow the steps in the "Importing a Data Frame" R tutorial video, and use the BullRiders.csv file. (Right-click and "Save As.") Make sure to name the dataframe "bull" when importing.
# 
# 1. Open RStudio.
# 2. Click on "Import Dataset" button at the top of the workspace window. Choose "from text file."
# 3. Click on the location of the BullRiders.csv file you just downloaded.
# 4. Click on the BullRiders.csv file. Then, click Upload.

library(SDSFoundations) 

bull <- BullRiders 

# So suppose you want to test a claim that the mean age of bull riders in this season was 30 years old. This is the 2012 
# season, and because we have the year that they were born, we can calculate pretty quickly their current age at that season.

age <- 2012 - bull$YearBorn

age

# An important assumption of a one-sample t-test that we're going to have to check is that the distribution of our variable
# age is approximately normally distributed. And we can get a quick look at the distribution of our variable by just creating # a histogram.
hist(age， breaks=6)
# Approximately normally distributed. There's no outliers, no serious skewness going on. So we are good to go with our
# normality assumption.

# So to do a one-sample t-test in R, we just use the t.test function. The first argument we need to give it is our vector of
# numbers, that is, our sample, which is age. And then as another option, we give it the value in our null hypothesis, which
# is going to be 30 years old.
t.test(age, mu = 30)

# One Sample t-test
# 
# data:  age
# t = -5.5446, df = 37, p-value = 2.599e-06
# alternative hypothesis: true mean is not equal to 30
# 95 percent confidence interval:
#     25.32877 27.82912
# sample estimates:
#     mean of x 
# 26.57895 

# Our p-value is definitely less than 0.05. So we would reject the null hypothesis here that the population mean is equal to
# 30 years old. Notice that it also gives us a 95% confidence interval of the true mean age based off our sample mean, which
# is also given, 26.57 years. 

# One other line that it gives you here is the alternative hypothesis that the true mean is not equal to 30. So this is
# corresponding to a two-sided alternative hypothesis. And that's because we haven't specified one, and the two-sided is just
# the default alternative that we use.

# If we wanted to go into this t-test saying there's no way that the mean age is greater than 30, it's got to be less than if
# it's anything, we could have a one-sided alternative hypothesis. And do that in R, we just need to add one more
# option, which is called alternative. Then we give it the word either "less" or "greater" in quotes, and that defines the
# direction of our alternative.

# So if we think that the mean age of these bull riders is actually less than 30, then we'll say alternative equals "less,"
# re-run it.
t.test(age, mu = 30, alternative = "less")

# One Sample t-test
# 
# data:  age
# t = -5.5446, df = 37, p-value = 1.299e-06
# alternative hypothesis: true mean is less than 30
# 95 percent confidence interval:
#     -Inf 27.6199
# sample estimates:
#     mean of x 
# 26.57895 

# Notice our t and degrees of freedom are exactly the same. Our p-value does change, though, when we switch from a two-sided
# to one-sided. And also notice when we have a one-sided, less than 30 alternative hypothesis, our confidence interval gives
# us a bound that is either negative infinity or positive infinity. And that's because we usually don't talk about confidence
# intervals when we go into a t-test knowing that the mean is either less than or greater than the claimed value.

# Lecture

# ALPHA LEVELS, CRITICAL VALUES, AND P-VALUES  2c
qnorm(0.95)
# 1.644854

# 2d
xbar = 8.12
mu = 8
n = 81
std = 0.72
z <- (xbar-mu)/(std/sqrt(n))
# 1.50

# SINGLE SAMPLE T-TEST 2a
mu = 2000
n = 25
xbar = 1891
std = 251
t <- (xbar-mu)/(std/sqrt(n))
# -2.17

# 2b What is the absolute critical t value, assuming alpha=0.05 ??

t <- abs(xbar-mu)/(std/sqrt(n))
# 2.064

# 3c
n = 7
std = 59
xbar = 861
mu = 900
std/sqrt(n)

# 3d
t <- (xbar-mu)/(std/sqrt(n))
# -1.749

# Week 8 Pre-Lab

library(SDSFoundations) 
bull <- BullRiders 

# Calculate the sample mean and standard deviation for the weight of the bull-riders.
# Summarize the bull rider weights
mean(bull$Weight) 
# 152.0263
sd(bull$Weight)
# 14.36115

# Create a histogram to visualize the distribution of bull-riders' weights.  
hist(bull$Weight, main='Histogram of Bull Rider Weights',xlab='Weight (lbs)')

# Confirm the assumptions of a one-sample t-test

# Run the t-test and interpret the results.
# Run the single sample t-test
t.test(bull$Weight, mu=190)
# One Sample t-test
# 
# data:  bull$Weight
# t = -16.2999, df = 37, p-value < 2.2e-16
# alternative hypothesis: true mean is not equal to 190
# 95 percent confidence interval:
#     147.3059 156.7467
# sample estimates:
#     mean of x 
# 152.0263 

# Calculate the standard error for this sample of 38 riders
sd(bull$Weight)/sqrt(38)
# 2.329686

# Week 8 Lab

# Primary Research Question. Do professional bull riders stay on their bulls at least 50% of the time? Test the hypothesis
# that the mean ride percentage is 50%.  (Hint: Two-sided test.)

# Calculate the sample mean and standard deviation of ride percentage.
mean(bull$RidePer)
# 0.3747368
sd(bull$RidePer)
# 0.121516

# The distribution of the percentage of time a professional bull rider stays on the bull for this sample
hist(bull$RidePer)

# What is the value of the t-statistic?
t.test(bull$RidePer, alternative = "two.sided", mu=0.50)
# One Sample t-test
# 
# data:  bull$RidePer
# t = -6.3545, df = 37, p-value = 2.082e-07
# alternative hypothesis: true mean is not equal to 0.5
# 95 percent confidence interval:
#     0.3347955 0.4146782
# sample estimates:
#     mean of x 
# 0.3747368 

# Week 8  Problem-Set 
# How much money do professional bull riders earn by participating in an event?

# Create a new variable that equals the "average earnings per event" in the 2012 season for each bull rider in the dataset.
earned <- bull$Earnings / bull$Events

# Make a histogram of your "earnings per event" variable.
hist(earned)

# Make a histogram of this log-transformed variable.
hist(log(earned))

# Q1 1c
mean(log(earned))
# 8.766166

# 1d
t.test(log(earned), mu=8.77)
# One Sample t-test
# 
# data:  log(earned)
# t = -0.0334, df = 37, p-value = 0.9736
# alternative hypothesis: true mean is not equal to 8.77
# 95 percent confidence interval:
#     8.533344 8.998989
# sample estimates:
#     mean of x 
# 8.766166 

# 1e
exp(8.53)
# 5064.446
exp(9.00)
# 8103.084

# Q2
wgt = c( 29.4,29.0,28.4,28.8,28.9,29.3,28.5,28.2)
n = 8
mu = 28.5
# Sample mean
mean(wgt)
# 28.8125
# Standard Deviation
sd(wgt)
# 0.4257347

# What is the test statistic for this hypothesis test? Remember: this is the t-statistic for the sample mean.

t <- (mean(wgt) - mu) / (sd(wgt)/sqrt(n))
t
# 2.076137

t.test(wgt,mu=28.5)
# One Sample t-test
# 
# data:  wgt
# t = 2.0761, df = 7, p-value = 0.07652
# alternative hypothesis: true mean is not equal to 28.5
# 95 percent confidence interval:
#     28.45658 29.16842
# sample estimates:
#     mean of x 
# 28.8125 

# 2d What is t-critical for this test, assuming an alpha level of 0.05?
2.365  # from the t-critical table with df=7

# Q3
n = 25
mn = 93.6
std = 7.8
mu = 91
(mn - mu) / (std/sqrt(n))
# 1.666667
# t-critical value
1.711
# 1.667 < 1.711 thus, 'No' sufficient evidence to suggest that the calcium concentration in the river is more than 91 mg/L.
mn_L = 95
(mn_L - mu) / (std/sqrt(n))
# 2.564103

# We should reject the Null hypothesis, but we didn't, thus type II error.

# Q4
n = 12
df = 12 - 1
mn = 42.6
std = 5.3

# What is t-critical for a 90% confidence interval? 
tcri <- round(qt(1 - 0.10/2,df),3) 
# 1.796

# Calculate a 90% confidence interval for the mean wingspan for the population of male peregrine falcons.
mn - tcri * std/sqrt(n)  
# 39.85216
mn + tcri * std/sqrt(n)  
# 45.34784

mn + tcri * std/sqrt(n)  - ( mn - tcri * std/sqrt(n) )
# 5.495682

# What is t-critical for a 95% confidence interval? 
tc95 <- round(qt(1 - 0.05/2,df),3) 
# 2.201

# Calculate a 90% confidence interval for the mean wingspan for the population of male peregrine falcons.
mn - tc95 * std/sqrt(n)  
# 39.23252
mn + tc95 * std/sqrt(n)  
# 45.96748

mn + tc95 * std/sqrt(n)  - ( mn - tc95 * std/sqrt(n) )
# 6.734964
