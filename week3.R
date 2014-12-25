
# automatically load the data for the labs. 
library(SDSFoundations)

# assign the data to your Workspace.
bull <- BullRiders 

# Scatter Plots

plot(bull$YearsPro, bull$BuckOuts, xlab='Years Pro', ylab='Buck Outs')

# the linear relationship between two variables -- LM function. The argument to lm is a model formula, which has the response
# on the left of the tilde ~ (read "is modeled as") 
# abline {graphics} -- adds one or more straight lines through the current plot.
abline(lm(bull$BuckOuts ~ bull$YearsPro))

plot(bull$Events, bull$BuckOuts, xlab='No of Events', ylab='Buck Outs')

abline(lm(bull$BuckOuts ~ bull$Events))

# Correlation

cor(bull$YearsPro, bull$BuckOuts)
# [1] -0.1670275   It makes sense because a slightly negative sloping line in 1st plot.

cor(bull$Events, bull$BuckOuts)
# [1] 0.9803737   correlation close to negative 1 or 1 indicates a strong relationship. close to 0, it indicates that there's
# no real relationship at all.

# make a correlation matrix
myvars <- c('YearsPro','Events','BuckOuts')

cor(bull[, myvars])
#            YearsPro     Events   BuckOuts
# YearsPro  1.0000000 -0.1597916 -0.1670275
# Events   -0.1597916  1.0000000  0.9803737
# BuckOuts -0.1670275  0.9803737  1.0000000

# Week 3 Pre Lab

# How many rides were completed by the rider with the fewest buck-outs?
bull[bull$BuckOuts == min(bull$BuckOuts), ]

# Rides value of bull dataframe for row where fewest attempts occur
bull[ which(bull$BuckOuts == min(bull$BuckOuts)), "Rides" ]

#gg <- c(10,2,6,12,14,15,15,24,15,25,3,12) - c(12.75)                     <----need to be modified
#sqrt(sum(gg^2)/12)

# Visualize and describe the first variable of interest 
# histogram of the successful ride percentages of all the riders.
hist(bull$RidePer)

fivenum(bull$RidePer)
# [1] 0.19 0.29 0.35 0.49 0.61

mean(bull$RidePer)
# [1] 0.3747368

sd(bull$RidePer)
# [1] 0.121516

# How many riders stayed on their bull more than 60% of the time
bull[bull$RidePer > 0.6, ]

# Visualize and describe the second variable of interest 
hist(bull$Top10)

fivenum(bull$Top10)
# [1]  0.0  2.0  6.5  9.0 18.0

mean(bull$Top10)
# [1] 6.236842

sd(bull$Top10)
# [1] 4.611585

# Create a scatterplot
plot(bull$RidePer,bull$Top10)

# Add line of best fit
abline(lm(bull$Top10~bull$RidePer))

# Calculate the correlation coefficient
cor(bull$RidePer,bull$Top10)
# [1] 0.8554679

# Create a correlation matrix  
vars <- c("Top10", "RidePer")

cor(bull[, vars])
#           Top10   RidePer
# Top10   1.0000000 0.8554679
# RidePer 0.8554679 1.0000000

# identify a specific record
bull[which(bull$Top10==5 & bull$RidePer==.53), ]

# Week 3 Lab

# Make a histogram to visualize the distribution of Earnings.
hist(bull$Earnings)

fivenum(bull$Earnings)
# [1]   21343.28   55617.33  111147.63  208724.52 1464475.61

# Scatterplot of Earnings and Ride Percentage
plot(bull$RidePer,bull$Earnings,xlab='RidePer',ylab='Earnings')

cor(bull$RidePer,bull$Earnings)
# [1] 0.6191194

abline(lm(bull$Earnings ~ bull$RidePer),xlab='RidePer',ylab='Earnings')

# Scatterplot of Earnings and Cup Points
plot(bull$CupPoints,bull$Earnings,xlab='CupPoints',ylab='Earnings')

abline(lm(bull$Earnings ~ bull$CupPoints),xlab='CupPoints',ylab='Earnings')

cor(bull$Earnings,bull$CupPoints)
# [1] 0.6741746

# identify specific case
which(bull$Earnings == max(bull$Earnings))

#remove this data point from the dataset to assess what kind of impact, if any, it had on our correlation analysis. 
#Subset the data
nooutlier <-bull[-1,]

plot(nooutlier$RidePer,nooutlier$Earnings,xlab='RidePer',ylab='Earnings')

abline(lm(bull$Earnings ~ bull$RidePer),xlab='RidePer',ylab='Earnings')

# After removing the outlier, what was the new correlation of Earnings and Ride Percentage?
cor(nooutlier$RidePer,nooutlier$Earnings)
# [1] 0.8144455

plot(nooutlier$CupPoints,nooutlier$Earnings,xlab='CupPoints',ylab='Earnings')

abline(lm(nooutlier$Earnings ~ nooutlier$CupPoints),xlab='CupPoints',ylab='Earnings')

# After removing the outlier, what was the new correlation of Earnings and Cup Points? 
cor(nooutlier$Earnings,nooutlier$CupPoints)
# [1] 0.9036631
# this outlier data was an influential point because it masked the strength of the relationships between Earnings and the
# other variables.

# Week 3 Problem Set

# Create a new variable for the average number of rides per event for each bull rider in the dataset:
bull$RidesPerEvent <- bull$Rides/bull$Events

# histogram of your "rides per event" variable 
hist(bull$RidesPerEvent)

fivenum(bull$RidesPerEvent)

# Create a scatterplot of "rides per event" and yearly ranking (defined by the "Place" variable) and add a line of best fit.
plot(bull$RidesPerEvent, bull$Place)

abline(lm(bull$Place ~ bull$RidesPerEvent),xlab='Rides per Event',ylab='Yearly Ranking')

# the correlation coefficient for rides per event and yearly ranking
cor(bull$RidesPerEvent, bull$Place)
# [1] -0.782405

Mints <- c(30,45,180,95,130,140,30,80,60,110,0,80)
fivenum(Mints)
# [1]   0.0  37.5  80.0 120.0 180.0
Grade <- c(74,68,87,90,94,84,92,88,82,93,65,90)

cor(Mints,Grade)
# [1] 0.5967026

80/180
# [1] 0.4444444                                                   <-------- need to be modified

# Plot of exam grades and time spent studying
plot(Mints, Grade, xlab='Minutes', ylab='Grade')

Mints_nooutlier <- c(30,45,180,95,130,140,80,60,110,0,80)

Grade_nooutlier <- c(74,68,87,90,94,84,88,82,93,65,90)

cor(Mints_nooutlier, Grade_nooutlier)
# [1] 0.7374439
