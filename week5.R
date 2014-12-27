
library(SDSFoundations) 

WR <- WorldRecords

## LINEAR MODELS

mens800 <- WR[WR$Event =='Mens 800m', ]

linFit(mens800$Year, mens800$Record, xlab='Year', ylab='Record')

## Week 5 Pre Lab

# How many different types of events (e.g. "Mens 100m," "Womens shotput," etc.) are represented in the dataset?
table(WR$Event)

# In what year did Usain Bolt first break the world record for the men's 100m dash?
WR[WR$Athlete == 'Usain Bolt' ,]

# 3) Who was the first woman to break the women’s 1 mile world record with a time of less than 260 seconds?
WR[WR$Record < 260 & WR$Event == 'Womens Mile' ,]

#Subset the data
menshot <- WR[WR$Event=='Mens Shotput', ]

womenshot <- WR[WR$Event=='Womens Shotput', ] 

# Create a scatterplot of year and record shotput distance: one for men and one for women.
plot(menshot$Year,menshot$Record,main='Mens Shotput World Records',xlab='Year',ylab='World Record Distance (m)',pch=16)

plot(womenshot$Year,womenshot$Record,main='Womens Shotput World Records',xlab='Year',ylab='World Record Distance (m)',pch=16)

#Run linear models
linFit(menshot$Year, menshot$Record,xlab='Year',ylab='World Record Distance (m)')

linFit(womenshot$Year,womenshot$Record,xlab='Year',ylab='World Record Distance (m)')

# How many records are in the menshots data frame?
menshot[0]
# data frame with 0 columns and 39 rows

# How many records in the womenshot data frame?
womenshot[0]
# data frame with 0 columns and 41 rows

## Week 5 Lab

#Subset the data
mensmile <- WR[WR$Event=='Mens Mile', ]

womensmile <- WR[WR$Event=='Womens Mile', ] 

# Create a scatterplot for each relationship of Mile time and year: one for men and one for women.  
plot(mensmile$Year,mensmile$Record,main='Mens Mile World Records',xlab='Year',ylab='World Record Time (s)',pch=16)

plot(womensmile$Year,womensmile$Record,main='Womens Mile World Records',xlab='Year',ylab='World Record Time (s)',pch=16)

# Run a linear model for each event and then interpret the results. Be sure to calculate R-squared values for each model. 
linFit(mensmile$Year, mensmile$Record,xlab='Year',ylab='World Record Time (s)')

linFit(womensmile$Year,womensmile$Record,xlab='Year',ylab='World Record Time (s)')

## Week 5  Problem Set

# What is the standing world record height (in meters) for men's pole vault?
menspole <- WR[WR$Event=='Mens Polevault', ]
# 6.14

# In what year did the pole vault record first exceed 6 meters? 
menspole[menspole$Record > 6.00, ]

# Create a scatterplot showing the men's pole vault records since 1970 as a function of year. Fit a linear model to the data.
menspole2 <- WR[WR$Event=='Mens Polevault' & WR$Year >= 1970, ]

plot(menspole2$Year, menspole2$Record, main='Mens Pole World Records', xlab='Year', ylab='World Record Height (m)', pch=16)

linFit(menspole2$Year, menspole2$Record,xlab='Year',ylab='World Record Height (m)')


# Problem 2
h <- c(0,2,4,6)
C <- c(140,280,420,560)
plot(h,C)
linFit(h,C)

((175*4)-140)/70
# 8
