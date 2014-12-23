
# Automatically load the data 
library(SDSFoundations)

# Assign the data to the Workspace
BikeData <- BikeData

# Week 1 R Tutorial Videos - IMPORTING A DATA FRAME

# Find Distance column and calculate the mean.
mean(BikeData$distance)
# 5.990661

# To see the counts of how many people rode daily, how many rode several times per week, and all the various categories in
# this cyc_freq variable. And in order to get a frequency table, we can just use the Table command.
table(BikeData$cyc_freq)

# Week 1 R Tutorial Videos - INDEXING DATA FRAMES

# Find every single case in column 5 from bike data.
BikeData[, 5]

# The number of males and females in BikeData set.
table(BikeData$gender)
#  F  M 
# 31 90 

# Only wanted to consider cases that were female, thus combine the idea of indexing and then logical statements
BikeData$gender == 'F'  # F stands for Female

# Build a data frame of all females information
female <- BikeData[BikeData$gender == 'F', ]

# Speed of the female bicyclists?
femalespeed <- BikeData$speed[BikeData$gender == 'F']

# Get the mean of female speeds using the mean function
mean(femalespeed)

# Week 1 R Tutorial Videos Pre-Lab 3

# 1. What type of dataset file extension is most easily imported in R?
# .csv

# 2. In R terminology, what is the name for a matrix with cases in rows and variables in columns?
# data frame

# 3. Number of cyclists who are employed and number of cyclists who are unemployed?
table(BikeData$employed)

# 4. For BikeData[8,7], what value will result?
6.21

# 5. A vector of distance values for all cases
BikeData[,7]

# 6. Create a new data frame from BikeData which contains only employed cyclists.
employed <- BikeData[BikeData$employed == '1', ]

# 7. Create a vector of distances for employed cyclists.
employed_distance <- BikeData$distance[BikeData$employed == '1']

# Lab 1

# Show number of students
table(BikeData$student)

# Pull out student data into a new data frame
student <- BikeData[BikeData$student == 1, ]

# Find how often the students ride
table(student$cyc_freq)

# Create vector for the variable distance
distance <- student$distance
distance

# Find average distance ridden by students
mean(distance)
