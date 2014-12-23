
# automatically load the data for the labs. 
library(SDSFoundations) 

# assign the data to your Workspace.
animaldata <- AnimalData

# VISUALIZING UNIVARIATE DATA

head(animaldata)

table(animaldata$Sex)
# Female   Male 
#   220    253

# barchart plot of animal genders
plot(animaldata$Sex, main = 'Bar Chart of Animal Genders', xlab = 'Animal Genders', ylab = 'Frequency')

# to visualize the distribution of the Age at Intake variable
hist(animaldata$Age.Intake)
# this distribution is what we would call a right skew or positive skew   distribution. 

# modify the notation of the histogram
hist(animaldata$Age.Intake, main = 'Histogram of Intake Ages', xlab='Age at Intake')

# HISTOGRAMS BY GROUPS

# histogram of female and male animals by ages
femaleage <- animaldata$Age.Intake[animaldata$Sex == 'Female']
maleage <- animaldata$Age.Intake[animaldata$Sex == 'Male']
hist(femaleage, main = 'Histogram of Female Ages', xlab = 'Ages at Intake of Female Animals')
hist(maleage, main = 'Histogram of Male Ages', xlab = 'Ages at Intake of Male Animals')

# changes the bins by setting 'breaks'
hist(maleage, main = 'Histogram of Male Ages', xlab = 'Ages at Intake of Male Animals', breaks = 15)

# 'which' will pull out a case from your data frame that follows a certain condition
max(maleage)
# [1] 17

max(femaleage)
# [1] 15

which(animaldata$Age.Intake == 17)
# [1] 415     It just gives the row number 415.

# detail information of this numbered 415.
animaldata[415, ]

# UNIVARIATE DESCRIPTIVE STATISTICS

mean(animaldata$Age.Intake)
# [1] 2.348837

# the halfway point, or where 50% of the data fall below it and 50% fall above it.
median(animaldata$Age.Intake)
# [1] 1

# standard deviation
sd(animaldata$Age.Intake)
# [1] 3.099837

# five-num summary: min, first quartile, median, third quartile, max age
fivenum(animaldata$Age.Intake)
# [1]  0  0  1  3 17

# Week 2 R Tutorial Videos -- Pre-Lab

# Find the number of animals that were adopted
table(animaldata$Outcome.Type)

# Pull out only adopted animals
adopted <- animaldata[animaldata$Outcome.Type == 'Adoption', ]

# Pull out just the days in shelter for the adopted animals
daystoadopt <- adopted$Days.Shelter

# Visualize and describe this variable
hist(daystoadopt)
fivenum(daystoadopt)
mean(daystoadopt)
sd(daystoadopt)

# the row that contains the animal that took the longest to be adopted. 
which(animaldata$Days.Shelter == max(daystoadopt))
# [1] 425

# IQR (Interquartile range) spread
38 - 8

# One adopted animal spent much more time in the shelter than the others.
# The longest period of time an animal was in the shelter was ? days before being adopted. 
max(daystoadopt)
# [1] 211

# z-score for this particular animal? Round to the nearest ONE decimal places.
round((max(daystoadopt) - mean(daystoadopt)) / sd(daystoadopt), digits = 1)
# [1] 5.1

animaldata[425, ]
# This animal was a 2-year-old dog that entered the shelter injured.

# Week 2 Lab 2

table(animaldata$Animal.Type)
# Cat Dog 
# 182 291 

# num of adult animals 
sum(animaldata$Age.Intake >= 1)
# [1] 282

AgeLargerOne <- animaldata[animaldata$Age.Intake >= 1, ]
AdultDogs <- AgeLargerOne[AgeLargerOne$Animal.Type == 'Dog', ] #226 adult dogs
AdultCats <- AgeLargerOne[AgeLargerOne$Animal.Type == 'Cat', ] #56 adult cats

# shape of the distribution of weight for adult dogs
hist(AdultDogs$Weight, main='Adult Dogs Weight', xlab='Weight')

# shape of the distribution of weight for adult cats
hist(AdultCats$Weight, main='Adult Cats Weight', xlab='Weight')

# Average adult cat weight in pounds 
mean(AdultCats$Weight)
# [1] 8.603571

#  standard deviation for the weight of the adult cats
sd(AdultCats$Weight)
# [1] 1.911517

#  z-score of a 13 pound adult cat
zcat <- (13 - mean(AdultCats$Weight)) / sd(AdultCats$Weight)
# [1] 2.299969

# What proportion of adult cats weigh more than 13 pounds
1 - pnorm(zcat)
# [1] 0.010725

# What quartile would contain a 13-pound adult dog?
fivenum(AdultDogs$Weight)
# [1]   3.30  13.50  35.25  54.00 131.00

fivenum(AdultCats$Weight)
# [1]  4.75  7.40  8.50  9.75 13.50

# 98% of adult cats at the shelter weigh between ?
qnorm(0.01, mean(AdultCats$Weight), sd(AdultCats$Weight))
# [1] 4.156719

# and ? pounds.
qnorm(0.99, mean(AdultCats$Weight), sd(AdultCats$Weight))
# [1] 13.05042

mean(AdultCats$Weight) + qnorm(0.99) * sd(AdultCats$Weight)
# [1] 13.05042

mean(AdultCats$Weight) - qnorm(0.99) * sd(AdultCats$Weight)
# [1] 4.156719
