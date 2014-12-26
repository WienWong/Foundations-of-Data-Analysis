
library(SDSFoundations) 

acl <- AustinCityLimits

## TABLE PROPORTIONS

gtab <- table(acl$Grammy)
gtab
# N  Y 
# 67 49 

# Function prop.table can calculate the proportion  across each of these categories
prop.table(gtab)
#         N         Y 
# 0.5775862 0.4224138 

67/116
# [1] 0.5775862

# the marginal distribution of Grammy winners. Look within each gender of artists to see how many Grammy winners there were.
gtab1 <- table(acl$Grammy, acl$Gender)
gtab1
#    F  M
# N 21 46   <==21 female non-Grammy winners
# Y 14 35

# the proportion of the 116 overall observations that # fall within each of 4 cells.
prop.table(gtab1)
#     F         M
# N 0.1810345 0.3965517
# Y 0.1206897 0.3017241

21/116
# [1] 0.1810345

# to see the conditional distribution of gender.
prop.table(gtab1,1)
#    F         M
# N 0.3134328 0.6865672
# Y 0.2857143 0.7142857

# if we look across the rows, we're going to get proportions that add up to 1,
# or 100%. So we can say within the no Grammy row, 31% of observations were
# females, 68% were males.

# now calculate the proportions across each column.
prop.table(gtab1,2)
#    F         M
# N 0.6000000 0.5679012
# Y 0.4000000 0.4320988
# Of all the females in the data set, 60% have not won a Grammy, 40% have.

## GROUPED BAR CHARTS

barplot(gtab,main='ACL Grammy Winners',xlab='Grammy Winner',ylab='Counts')

# Put the gender variable along the x-axis now. So we see males and females.
# And then it colored the bars and kind of stacked them based on the response to Grammy.
barplot(gtab1,legend=T,main='ACL Grammy Winner',xlab='Gender',ylab='Counts')
#    F  M
# N 21 46
# Y 14 35

# switch it to a side by side bar chart instead of a stacked bar chart.
barplot(gtab1,legend=T,main='ACL Grammy Winner',xlab='Gender',ylab='Counts',beside=T)

# a relative frequency stacked bar chart, or the mosaic plot.
barplot(prop.table(gtab1, 2),legend=T,main='ACL Grammy Winner',xlab='Gender',ylab='Counts')
# we can see within each gender what proportion were Grammy winners and non 
# Grammy winners. Although more males in the data set, the proportion among males 
# and females that are Grammy winners is pretty consistent at about 40%.

# Week 4 Pre Lab

#  What genre was played by the first female artist in the dataset who was over 60 years of age?
acl[which(acl$Age > 60 & acl$Gender == 'F'), ]

#  Marginal probabilities alone would help us determine the distribution of a single categorical variable.

# Create tables of marginal distributions
genre <- table(acl$Genre)
genre

gender <- table(acl$Gender)
gender
#  F  M 
# 35 81 
# Create contingency table 
twoway <- table (acl$Gender,acl$Genre)
twoway
#     Country Jazz/Blues Rock/Folk/Indie Singer-Songwriter
# F       7          6              12                10
# M      11          7              56                 7

# Visualize the counts
barplot(twoway, legend=T, beside=T)
# This code produces a bar chart with both a legend and side-by-side bars for each gender

# What would the code look like if we wanted to keep the legend but stack the
# bars (instead of set them side-by-side)?
barplot(twoway, legend=T)

# Calculate P(A): the probability of each genre being played
prop.table(genre)
#   Country         Jazz/Blues      Rock/Folk/Indie    Singer-Songwriter 
# 0.1551724         0.1120690         0.5862069         0.1465517 

# What value should you get if you sum the four values together?
sum(prop.table(genre))

# Calculate P(A|B): the probability of each genre being played, given the artist’s gender
prop.table(twoway,1)
#        Country  Jazz/Blues    Rock/Folk/Indie   Singer-Songwriter
#   F 0.20000000 0.17142857      0.34285714        0.28571429
#   M 0.13580247 0.08641975      0.69135802        0.08641975
# The number 1 references the first variable (gender) listed in the contingency table code. 

# To determine the proportion of jazz performers that were male
table(acl$Genre=='Jazz/Blues', acl$Gender)

gtab2 <- table(acl$Genre, acl$Gender)
gtab2

7/13

# To determine the proportion of males that performed jazz, 
7/81

# Which of these lines of code provides the probability that a randomly selected artist from the dataset performed rock/folk
# /indie music?
prop.table(genre)

# Which of these lines of code provides the probability that a randomly selected female artist performed rock/folk/indie
# music?
prop.table(twoway,1)

# For genre and gender to be independent, which of the following statements must be true?
# P(rock) = P(rock|female)

##  Week 4 Lab

# Which genre had the greatest number of Grammy wins?
twoway2 <- table (acl$Genre, acl$Grammy)
twoway2

# What is the probability that a randomly selected artist was a Grammy winner? 
gtab[2]/sum(gtab)

# To determine the probability of winning a Grammy if the artist was a singer-songwriter
prop.table(twoway2, 1)
5 / 17

# To determine the probability that a randomly-selected Grammy winner was a singer-songwriter
5 / 49
prop.table(twoway2, 2)

# What is the probability that a randomly selected artist from Country won a Grammy? 
prop.table(twoway2, 1)
12 / 18

# What is the probability that a randomly selected artist from Jazz won a Grammy? 
prop.table(twoway2, 1)
6 / 13

# What is the probability that a randomly selected artist from Rock/folk/indie won a Grammy? 
prop.table(twoway2, 1)
26 / 68

# What is the probability that a randomly selected artist from Singer-songwriter won a Grammy? 
prop.table(twoway2, 1)
5 / 17

# Visual examination of the barplot shows the conditional probabilities of winning a Grammy are not equal across Genres
barplot(twoway2,beside=T)

#  Week 4 Problem Set
# Problem 1
# How many artists in the dataset have 100K+ likes on Facebook?
sum(acl$Facebook.100k)
# [1] 85

# Which age group has the highest number of artists that have 100K+ likes on Facebook?
twoway3 <- table(acl$Age.Group, acl$Facebook.100k)
twoway3
#                     0  1
# Fifties or Older   13 19
# Forties            6 24
# Thirties           9 31
# Twenties           3 11

# For each age group, fill in the proportion of artists who have 100K+ likes on Facebook.
prop.table(twoway3,1)

11/14

31/40

24/30

19/32

# Problem 2

F1 <- c(5,10,9,10,6)
S2 <- c(8,10,9,7,4)
J3 <- c(11,5,4,4,2)
S4 <- c(9,9,4,2,0)

# proportion of students in the class received a grade of A
(5+8+11+9)/(sum(c(F1,S2,J3,S4)))

# proportion of the students were upperclassmen (juniors and seniors)
sum(c(J3,S4))/(sum(c(F1,S2,J3,S4)))

6/sum(F1)

# What is the probability that a randomly selected student from the class would be a sophomore that received a grade of B? 
10/sum(S2)* (sum(S2)/(sum(c(F1,S2,J3,S4))) )

# What proportion of juniors passed the course with a grade of D or better? 
rmlastJ3 <- c(11,5,4,4)
sum(rmlastJ3)/sum(J3) #* ( sum(J3)/(sum(c(F1,S2,J3,S4))) )

# What is the probability that a randomly selected student from this class would be a senior? 
sum(S4)/(sum(c(F1,S2,J3,S4))) 

# If a student received a grade of D in the class, what is the probability that the student was a senior?
2/(10+7+4+2)

# the probability that a randomly selected student is a senior & we know that the student received a grade of D 
(10+7+4+2)/(sum(c(F1,S2,J3,S4)))  * (2/sum(S4) )

# Problem 3

0.15/0.35

# Problem 4

W <- 12+34+30+24
M <- 35+25+32.5+7.5

# What is the probability that a randomly chosen person from the survey prefers Action films?
(12+35)/200

# What is P(Action|Women)?
0.12/0.50

