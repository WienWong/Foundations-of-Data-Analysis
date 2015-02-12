
# Week 10  Lecture - CHI-SQUARE GOODNESS-OF-FIT, PART ONE

Brand <- c(38, 28, 24)
# Expected value of Brand A, B and C
sum(Brand) *0.333
# 30

# Chi-square statistic: 
Chi <- (38-30)^2/30 + (28-30)^2/30 + (24-30)^2/30
# 3.467

# Chi-square critical value:
qchisq(.95, df=2)
# 5.991465

# Week 10 Lecture - CHI-SQUARE GOODNESS-OF-FIT, PART TWO
age <- c(12, 36, 32)
sum(age)
# 80
sum(age)/3
# 26.67

sum(age)*0.2
# 16
sum(age)*0.45
# 36
sum(age)*0.35
# 28

# the chi-square statistic:
Chi <- (12-16)^2/16 + (36-36)^2/36 + (32-28)^2/28
Chi
# 1.571429

# the critical Chi-square value:
qchisq(.95, df=2)
# 5.991465

# Week 10 Lecture - CHI-SQUARE TEST-OF-INDEPENDENCE

#                                         Men	Women   total
# Expressed a Fear of Heights	           68	109     177
# Did Not Express a Fear of Heights	       94	89      183
#                                         162   198     360
#                                         0.45  0.55   
68 + 109
# 177
94 + 89
# 183
68 + 94
# 162
109 + 89
# 198
162 + 198
# 360
162/(162+198)
# 0.45
198/(162+198)
# 0.55
# Men who expressed a fear of heights:
177 * 0.45
# 79.65
# Men who did not express a fear of heights: 
183 * 0.45
# 82.35
# Women who expressed a fear of heights: 
177 * 0.55
# 97.35
# Women who did not express a fear of heights: 
183 * 0.55
# 100.65

###89/(2+86+89+204)  ###0.2335958 for video lecture example

# Proportion of the entire sample that has a fear of heights: 
177 / 360
# 0.4916667
# Proportion of the men that have a fear of heights:
68 / 162
# 0.4197531
# Proportion of the women that have a fear of heights:
109 / 198
# 0.5505051

# Week 10 - Tutorial CHI-SQUARED GOODNESS OF FIT TEST

library(SDSFoundations) 

acl <- AustinCityLimits 

# So suppose we hear a claim that 1/3 of all the ACL Live artists have won a Grammy. We can test this claim with a chi-squared goodness of fit test.
gtab <- table(acl$Grammy)
# First, to make a table of the actual counts of our yeses and nos in our Grammy variable.
gtab
# N  Y 
# 67 49 

# Next step we have to take is actually create a vector that holds the claimed proportions of yeses and nos. So if the claim
# was that 1/3 of ACL Live performers have won a Grammy, that means that 2/3 haven't.
claimp <- c(2/3, 1/3)
# Be careful to keep it in the correct order that it appears when you use the table function.

chisq.test(gtab, p=claimp)
# Chi-squared test for given probabilities
# 
# data:  gtab
# X-squared = 4.1422, df = 1, p-value = 0.04183

# So we see here our p-value is less the 0.05, and that allows us to reject the null hypothesis that these were, in fact, the
# actual proportions of Grammy winners and non-Grammy winners.

# To check our assumption of sufficient sample size. If we ask for $expected at the end of that function, we're basically
# saying, go into this test and give us this vector, called expected, which is a component of our chi-squared test that we
# just don't see in the output. If we run that, we're going to get the expected counts for each category.
chisq.test(gtab, p=claimp)$expected
#      N        Y 
# 77.33333 38.66667

# Basically, what R is doing is taking our claim proportions and saying, if this were true, we would expect 77.3 artists to
# not have won a Grammy and 38.6 to have won a Grammy. Because both of these are over 5, we meet our sufficient sample size
# assumption, so we're OK to carry out the chi-squared goodness of fit test.

# Week 10 - Tutorial CHI-SQUARED TEST OF INDEPENDENCE

# We're going to create a contingency table of our two categorical variables
grammyage <- table(acl$Grammy, acl$Age.Group)
grammyage
# Fifties or Older Forties Thirties Twenties
# N               17      13       28        9
# Y               15      17       12        5

# It shows the counts of all the non-Grammy winners and Grammy winners across each of our four age-group categories.

# So similar to what we did in our goodness of fit test, we're going to  actually give the chi-square.test function statement# , and then ask for the expected counts out of that function object. 

# Notice we don't need the p option here. We don't have any claim about the distribution of a single variable. We're simply
# seeing if these two variables are independent from each other.
chisq.test(grammyage)$expected
# Fifties or Older  Forties Thirties Twenties
# N         18.48276 17.32759 23.10345 8.086207
# Y         13.51724 12.67241 16.89655 5.913793

# And we can see that all of these cells have an expected count of greater than 5. So we're good to go on our sample-size assumption.


# Now the only thing left is to carry out the actual test, which, again, we're just going to give it our contingency table
# and then add one other option-- correct equals false.

# Now if we hadn't met our sample-size assumption, there is a non-parametric correction that we could apply this chi-square
# test of independence. And actually, that is the default in R for chi-square.test.
chisq.test(grammyage, correct=F) 
# F means don't correct this test for any violation of assumptions.

# Pearson's Chi-squared test
# 
# data:  grammyage
# X-squared = 5.5415, df = 3, p-value = 0.1362
# Here we see our p-value is greater than 0.05, so we would fail to reject that these two variables are independent.

# Week 10 - PreLab

# Primary Research Questions
# 
# 1. Are there an equal number of male and female performers on Austin City Limits?
# 2. Are male performers just as likely to have had a Top 10 hit as female performers?
acl[acl$Artist =='Allen Toussaint', ] # Allen Toussaint

# Question 1 (Goodness of Fit)
# Create a table of counts for Gender
gender_tab <-table(acl$Gender)
gender_tab
#  F  M 
# 35 81 

# Create vector of expected proportions
ExpGender <- c(.50, .50)

# Check expected counts assumption
chisq.test(gender_tab, p=ExpGender)$expected
#  F  M 
# 58 58

# Run goodness of fit
chisq.test(gender_tab, p=ExpGender)
# Chi-squared test for given probabilities
# 
# data:  gender_tab
# X-squared = 18.2414, df = 1, p-value = 1.946e-05

# We should reject the hypothesis that there were an equal number of male and female performers at ACL Live.

# Question 2 (Test of Independence)
# Create two-way table
gender_top10 <-table(acl$Gender, acl$BB.wk.top10)
gender_top10
#    0  1
# F 15 18
# M 38 32

# Generate expected counts
chisq.test(gender_top10, correct=FALSE)$expected
#          0        1
# F 16.98058 16.01942
# M 36.01942 33.98058

# Run test of independence
chisq.test(gender_top10, correct=FALSE)
# Pearson's Chi-squared test
# 
# data:  gender_top10
# X-squared = 0.7002, df = 1, p-value = 0.4027

# We should fail to reject the hypothesis that gender is associated with having a Top 10 hit.

# Week 10 Lab

acl[acl$Year>=2012 & acl$Gender=='F',]
