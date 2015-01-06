
library(SDSFoundations)

survey <- StudentSurvey

# week 7  R Tutorial

mean(survey$age)

sd(survey$age)

hist(survey$age) #not normally distributed

sample(survey$age, size=30)
# a list of 30 randomly-selected ages.

# Create an empty vector to store all of our sample means. Basically draw 1,000 samples and store their means into this
# vector that's now empty called My X Bar.
myxbar <- rep(NA,1000)

# Now, to randomly sample 1,000 times
for(i in 1:1000){
    mysamp <- sample(survey$age, size=30)
    # So My X Bar, on the Ith index, is going to hold the mean of My Samp.
    myxbar[i] <- mean(mysamp)
}

# the sampling distribution of means that we just ran.
hist(myxbar)
# Here the sample means are much more constricted.

mean(myxbar)
# it's really, really close to the true population mean, 19.25594.

sd(myxbar)

sd(survey$age) / sqrt(30)
# the standard deviation of our sampling distribution, to be close to the population standard deviation divided by the square
# root of our sample size, which was 30.

# Week 7 Pre-Lab 

# Visualize the shape of the population data by making a histogram.  
hist(survey$name_letters)

# Calculate the “true” mean and standard deviation of the population.
fivenum(survey$name_letters)
mean(survey$name_letters)
sd(survey$name_letters)

# Draw 1,000 samples of size n=5 from the population data. Calculate the mean of each sample. 
xbar5 <- rep(NA, 1000)

for(i in 1:1000){
    x <- sample(survey$name_letters, size = 5)
    xbar5[i] <- mean(x)
}

# Graph these 1,000 sample means in a histogram and examine the shape.
hist(xbar5,xlim = c(2, 10))

# Calculate the mean and standard deviation of the sampling distribution.
mean(xbar5)
sd(xbar5)
sd(survey$name_letters)/sqrt(5)

# Repeat this process for samples of size n=15 and n=25.
# Repeat for samples of size n=15
xbar15 <-rep(NA, 1000)

for (i in 1:1000){
    x <- sample(survey$name_letters, size = 15)
    xbar15[i] <- mean(x)
}
hist(xbar15, xlim = c(2, 10))
mean(xbar15)
sd(xbar15)
sd(survey$name_letters)/sqrt(15)

# Repeat for samples of size n=25
xbar25 <- rep(NA, 1000)
for (i in 1:1000){
    x <- sample(survey$name_letters, size = 25)
    xbar25[i] <- mean(x)
}
hist(xbar25,xlim = c(2,10))
mean(xbar25)
sd(xbar25)
sd(survey$name_letters)/sqrt(25)

# THE CONFIDENCE INTERVAL  Q1

# How much variation should we expect in sample means given a sample size of 36? In other words, what is the value of the
# standard error?
13/sqrt(36)

# What is the z-score of our sample mean in the sampling distribution given sample sizes of 36? 
(77 - 74)/ (13/sqrt(36)) 

# What is the probability of observing our sample mean, or one that is larger, when the true population mean is 74 beats per
# minute?
1 - pnorm(77,74,(13/sqrt(36)))

# Construct a 95% confidence interval around your sample mean of 77 beats per minute using this formula Mean = 1.96*(sigma
# /sqrt(n))
# What are the bounds of the 95% confidence interval?
77 + 1.96*(13/sqrt(36))
77 - 1.96*(13/sqrt(36))

# THE CONFIDENCE INTERVAL Q2

# What is the sample mean? 
s1 <- c(180, 200, 190, 230, 80, 160, 170)
s2 <- c(130, 140, 220, 110, 120, 100, 170)
sum(s1,s2)
sum(s1+s2)
sum(s1,s2)/14

# If the standard deviation reported by the yogurt industry is 48.5 calories, how much variation should we expect between
# sample means for samples of size 14? 
48.5/sqrt(14)

# What is the margin of error if the standard deviation is 48.5 and the samples are of size 14? (Assume 95% confidence)
1.96*48.5/sqrt(14) 

# The margin of error is found by multiplying the standard error of the mean by the z-score of the percent confidence level.
# the Z-score critical value for a 95% confidence level, which is 1.96.  This comes from the standard normal z-table. 

# What is the 95% confidence interval for the average calorie content of vanilla yogurt? 
sum(s1,s2)/14 + 1.96*48.5/sqrt(14) 
sum(s1,s2)/14 - 1.96*48.5/sqrt(14) 


# Week 7 Lab


survey <- StudentSurvey

# Visualize the shape of the population happiness scores data. 
hist(survey$happy)

# Calculate the “true” mean and standard deviation of the population.
mean(survey$happy)

sd(survey$happy)

# Draw 1,000 samples of size n=5 from the population data. Calculate the mean of each sample. 
hapsamp <- rep(NA, 1000)

for(idx in 1:1000){
    vect <- sample(survey$happy, size = 5)
    hapsamp[idx] <- mean(vect)
}

# Graph these 1,000 sample means in a histogram and examine the shape.
hist(hapsamp, xlim = c(0,100))

# Calculate the mean and standard deviation of the sampling distribution.
mean(hapsamp)

sd(hapsamp)

sd(survey$happy)/sqrt(5)

# Repeat this process for samples of size n=15.
hapsamp15 <- rep(NA, 1000)

for(idx in 1:1000){
    vect15 <- sample(survey$happy, size = 15)
    hapsamp15[idx] <- mean(vect15)
}

hist(hapsamp15, xlim = c(0,100))

mean(hapsamp15)

sd(hapsamp15)

sd(survey$happy)/sqrt(15)

# Repeat this process for samples of size n=25.
hapsamp25 <- rep(NA, 1000)

for(idx in 1:1000){
    vect25 <- sample(survey$happy, size = 25)
    hapsamp25[idx] <- mean(vect25)
}

hist(hapsamp25, xlim = c(0,100))

mean(hapsamp25)

sd(hapsamp25)

sd(survey$happy)/sqrt(25)

