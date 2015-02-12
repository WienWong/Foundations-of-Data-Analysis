
# Week 9 Lecture - INDEPENDENT SAMPLES T-TEST
# the t-statistic?
t <- (78-66) / (sqrt((12.56)^2/10 + (12.04)^2/15))
t
# 2.37918

# What is the t-critical value for this test, assuming df = n_smallest - 1 and alpha=0.05?
qt(0.025, 9)
# -2.262157
qt(0.975, 9)
# 2.262157
qt(0.95, 9)
# 1.833113

# Week 9 Lecture - PAIRED SAMPLES T-TEST
ChewingG <- c(79, 95, 85, 82)
NoG <- c(80, 94, 87, 84)
# the average difference score for this sample
mean(ChewingG) - mean(NoG)
# -1
# the standard error for this test?
diff <- ChewingG - NoG
sd(diff)/sqrt(4)
# 0.7071068
# What is the value of the t-statistic and the appropriate conclusion for the test, assuming alpha=0.05
t <- (mean(diff) - 0) / ( sd(diff)/sqrt(4) )
t
# -1.414214

# Week 9 R Tutorial - PAIRED T TESTS



# Week 9 R Tutorial - INDEPENDENT T TEST 

library(SDSFoundations) 

post <- PostSurvey

# Let's first look at the distribution of our first variable, which is exclusive within the post data frame.
hist(post$exclusive)

# Similarly, if we look at our post-exclusive variable,
hist(post$post_exclusive)

# So even though neither of these distributions are normal, the assumption of a paired t-test is that the distribution of the
# differences in measurements is normal. 
diff <- post$exclusive - post$post_exclusive
# So this vector will represent the change in each student's response between the first time we ask them and the second time.

hist(diff)
# It's not perfectly normal, but there's no huge outliers and there's no skewness. So we are good to go on the normality
# assumption of our paired sample t-test.

# Two variables. And tell R that we have paired data, we're going to add one more option and that is paired equals t or
# paired equals true.
t.test(post$exclusive, post$post_exclusive, paired = T)
# So this'll tell R that these two variables that we're giving it within the t dot test function are actually from the same
# subjects. So they are paired.

# Paired t-test
# 
# data:  post$exclusive and post$post_exclusive
# t = 3.2243, df = 213, p-value = 0.001462
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#     0.2342792 0.9713283
# sample estimates:
#     mean of the differences 
# 0.6028037 

# We see that our p is less than alpha, less than 0.05. So we would reject the null hypothesis that the mean response to the
# first exclusive variable is equal to the mean of the second. 

mean(diff)
# 0.6028037

# So similar to a one sample t-test, if we had a one sided alternative hypothesis, we could add the alternative option and
# then give it either the less or greater word in quotes.
t.test(post$exclusive, post$post_exclusive, paired = T, alternative = 'less')
 
# Paired t-test
# 
# data:  post$exclusive and post$post_exclusive
# t = 3.2243, df = 213, p-value = 0.9993
# alternative hypothesis: true difference in means is less than 0
# 95 percent confidence interval:
#     -Inf 0.9116654
# sample estimates:
#     mean of the differences 
# 0.6028037

# Week 9 R Tutorial - INDEPENDENT T TEST

# Instead of looking at two measurements that are paired, we're going to compare two independent groups and see if the means
# are the same.

# So if we want to compare if the mean number of hours of sleep on Tuesday is the same for female students as it is for male
# students,
fsleep <- post$sleep_Tues[post$gender=='Female']
msleep <- post$sleep_Tues[post$gender=='Male']

# We have two independent groups-- the assumptions we have to meet are that both groups are normally distributed.
hist(fsleep)
hist(msleep)
# We don't see any serious skewness or outliers. So we are good to go on the normality assumption for our two sample independent t-test.

# But we no longer have paired data, so we don't need to give it this paired equals true option. The default for t.test is
# that the paired option is false.
t.test(fsleep, msleep)
# If I wanted that one-sided alternative hypothesis, I could use the alternative option.

# Welch Two Sample t-test
# 
# data:  fsleep and msleep
# t = -0.4156, df = 27.979, p-value = 0.6809
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#     -0.7606145  0.5040355
# sample estimates:
#     mean of x mean of y 
# 6.809211  6.937500

# So because our p value here is greater than 0.05, we would fail to reject the null hypothesis that the means of these two
# groups are equal. The mean hours of sleep for females, 6.809, and also for males, 6.938.


# Week Pre-Lab

# Primary Research Questions
# 
# Who is happier at the beginning of the semester:  under-classmen or upper-classmen?
# Does student happiness change from the beginning of the semester to the end?

# Lab Question 1
# Make a vector of happiness scores for each sample
underclass_happy <- post$happy[post$classification=='Freshman'|post$classification=='Sophomore']
upperclass_happy <- post$happy[post$classification=='Junior'|post$classification=='Senior']

# Check the normality assumption
hist(underclass_happy, xlab='Underclassman Happiness', main='Percent of Time Happy')
# left-skewed
hist(upperclass_happy, xlab='Upperclassman Happiness', main='Percent of Time Happy')
# left-skewed

# Run independent t-test
t.test(underclass_happy, upperclass_happy)

# Welch Two Sample t-test
# 
# data:  underclass_happy and upperclass_happy
# t = 0.423, df = 35.358, p-value = 0.6748
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#     -5.210391  7.954653
# sample estimates:
#     mean of x mean of y 
# 79.67213  78.30000 

# Lab Question 2
# Make a vector of difference scores
post$diff_happy <- post$happy - post$post_happy

# Check the normality assumption
hist(post$diff_happy, xlab= 'Difference in Happiness over the Semester', main = 'Happy-Post Happy')

# Run dependent t-test
t.test(post$happy, post$post_happy, paired=T)

# Paired t-test
# 
# data:  post$happy and post$post_happy
# t = 1.6838, df = 213, p-value = 0.09368
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#     -0.2168971  2.7589532
# sample estimates:
#     mean of the differences 
# 1.271028

# What percent of the time, on average, were underclassmen happy? 
mean(underclass_happy)
# 79.67213

# What percent of the time, on average, were upperclassmen happy? 
mean(upperclass_happy)
# 78.3

# Distribution of happiness difference scores was
hist(post$diff_happy)
# nearly Normal but with a slight right/postive-skewed

# Week 9 - Lab

# Primary Research Questions
# 
# 1. Do students at UT spend more time on homework per week in college than they did in high school?
# 2. Do students in fraternities and sororities get less sleep on the weekends than other college students?

# NOTE:  If you are running directional hypotheses tests, remember that you must modify the code to reflect this direction.
# A one-sided test looks like this:   
# t.test(Variable1, Variable2, alternative = 'less'), when you expect Mean1 < Mean2
# t.test(Variable1, Variable2, alternative = 'greater'), when you expect Mean1 > Mean2

# 1a On average, students spent how many hours more on homework each week in college than they did in high school? 
diff_hw_hr <- post$hw_hours_college - post$hw_hours_HS
mean(diff_hw_hr)
# 10.94626

hist(post$hw_hours_college)
hist(post$hw_hours_HS)
# both positive skewed
hist(diff_hw_hr)
# nearly Normal

# 1b What was the t-statistic for this test? 
t.test(post$hw_hours_college, post$hw_hours_HS, paired=T)
# Paired t-test
# 
# data:  post$hw_hours_college and post$hw_hours_HS
# t = 16.8115, df = 213, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#     9.662804 12.229720
# sample estimates:
#     mean of the differences 
# 10.94626

# 2a. On average, students who are not Greek sleep how many hours more than Greek students on Saturday nights? 
nongreek <- post$sleep_Sat[post$greek=='no'] 
greek <- post$sleep_Sat[post$greek=='yes']
mean(nongreek) - mean(greek)
# 0.3153743

# 2b. What is the t-statistic for this test? 
t.test(greek, nongreek, alternative='less')
# Welch Two Sample t-test
# 
# data:  nongreek and greek
# t = 0.9808, df = 62.948, p-value = 0.1652
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#     -0.2214398        Inf
# sample estimates:
#     mean of x mean of y 
# 8.042647  7.727273 

hist(nongreek)
hist(greek)
# both nearly Normal

# Week 9 - Problem Set

# Q1 Is the increase in time spent studying from high school to college the same for nursing majors and biology majors?  

# 1. Create a new variable that equals the difference in hours spent studying per week in college versus high school for each
# student. 
# 
# 2. Create two vectors of those differences, one for nursing majors and one for biology majors.
# 
# 3. Use this data to answer the following questions.

diff_hw_hr <- post$hw_hours_college - post$hw_hours_HS

nurse <- post$hw_hours_college[post$major == 'Nursing']

biology <- post$hw_hours_college[post$major == 'Biology']

diff_hw_nurse <- nurse - post$hw_hours_HS
    
diff_hw_biology <- biology - post$hw_hours_HS

hist(diff_hw_hr)
hist(diff_hw_nurse)
hist(diff_hw_biology)
