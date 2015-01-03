
# automatically load the data for the labs. 
library(SDSFoundations) 

# assign the data to your Workspace.
animaldata <- AnimalData

# extract the dogs
Dogs <- animaldata[animaldata$Animal.Type == "Dog", ]
# Dogs <- animaldata[which(animaldata$Animal.Type == "Dog"), ]

# What was the most common way that dogs arrived in the shelter?
IntakeDog <- Dogs$Intake.Type
table(IntakeDog)

# What proportion of dogs were brought to the shelter as an owner surrender?
DogOwnSur <- table(IntakeDog)[2]
DogTotal <- sum(table(IntakeDog))
DogOwnSur / DogTotal
# Owner Surrender 
# 0.2783505 

# Of the dogs that were brought to the shelter as an owner surrender, how many were returned to their owner?
# extract the Owner Surrender dogs only
OwnSurDogs <- Dogs[Dogs$Intake.Type == "Owner Surrender", ] 

# extract the dogs returned to owner only
Re2OwnDogs <- OwnSurDogs[OwnSurDogs$Outcome.Type == "Return to Owner", ] 

# Mean number of days that these dogs spent at the shelter before being returned to their owner? 
mean(Re2OwnDogs$Days.Shelter) 
# [1] 3.5

# to show the bar chart of dog intake types
hist(table(IntakeDog))

####

# 4b. What proportion of adults sleep longer than 4.5 hours per night? 
1 - pnorm(4.5,6.7,1.1)
# [1] 0.9772499

# 4c. What proportion of adults sleep between 5.38 and 8.79 hours of sleep? 
pnorm(8.79,6.7,1.1) - pnorm(5.38,6.7,1.1)
# [1] 0.8562138

# Week 2 Comprehension Check
# Twelve coworkers log their hours worked overtime in the past month:
y <- c(10,2,6,12,14,15,15,24,15,25,3,12)
mean <- sum(y)/length(y)
sqrt ( sum((y - mean)^2)/length(y) ) # <-- Discrete random variable of Wikipedia
# [1] 6.820618
sd(y)
# [1] 7.123903
