
# Week 11 Lecture - ONE-WAY ANOVA

# 4d
dfW = 4*25 -4

# 4i How many post-hoc group comparisons will you need to run?
k=4
C = 4*(4-1)/2

# 4j Using the Bonferroni correction, what significance level should you use for each post-hoc hypothesis test if you want an
# overall significance level of 0.05?
0.05/C
# 0.008333333

# 4k
# The question is easier than you think. Here are some quotes from Wikipedia that should help. "A type I error ... occurs
# when the null hypothesis (H0) is true, but is rejected. ...A Type I error occurs when we believe a falsehood.[4] In terms
# of folk tales, an investigator may be "crying wolf" without a wolf in sight (raising a false alarm) (H0: no wolf)."..."The
# rate of the type I error is ... denoted by the Greek letter α (alpha). It usually equals the significance level of a test.
# In the case of a simple null hypothesis α is the probability of a type I error."

# 4I
C = 6
round(1-(1-.05)^C,2)
# 0.2649081


# Week 11 - Lab

library(SDSFoundations) 

film <- FilmData

film$Rank[film$Film == 'Titanic']
# 2

# Primary Research Questions
# 
# 1. Does a film’s rating (PG, PG-13, or R) impact its cost to produce?
# 2. Does a film’s rating (PG, PG-13, or R) influence its IMDB score?

# Show how many films are in each group
table(film$Rating)
# PG PG13    R 
# 39   94   18 

# Question 1

# Calculate avg film budget of each group
aggregate(Budget~Rating,film,mean)
#    Rating    Budget
# 1     PG  75.07692
# 2   PG13 127.07447
# 3      R  93.02926

# Calculate sd of film budget within each group
aggregate(Budget~Rating,film,sd)
#    Rating   Budget
# 1     PG 46.46002
# 2   PG13 58.02877
# 3      R 56.90324

# Visualize the group means and variability
boxplot(film$Budget~film$Rating, main= "Film Budgets by Rating",
        ylab= "Budget", xlab= "MPAA Rating")

# Run ANOVA
modelbud <- aov(film$Budget~film$Rating)
summary(modelbud)
#               Df Sum Sq Mean Sq  F value  Pr(>F)    
# film$Rating    2  79852   39926   13.13 5.67e-06 ***
# Residuals    147 446994    3041                     
# ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 1 observation deleted due to missingness

# Run post-hoc test if F statistic is significant
TukeyHSD(modelbud)
# Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = film$Budget ~ film$Rating)
# 
# $`film$Rating`
#              diff       lwr        upr     p adj
# PG13-PG  51.99755  27.12917 76.8659209 0.0000060
# R-PG     17.95234 -19.99271 55.8973842 0.5031892
# R-PG13  -34.04521 -68.45570  0.3652895 0.0531681


# Question 2


# Calculate avg IMDB score of each group
aggregate(IMDB~Rating,film,mean)
#    Rating     IMDB
# 1     PG 7.015385
# 2   PG13 7.014894
# 3      R 7.183333

#Calculate sd of IMDB scores within each group
aggregate(IMDB~Rating,film,sd)
#    Rating      IMDB
# 1     PG 1.0139912
# 2   PG13 0.8213492
# 3      R 0.6679644

# Visualize the group means and variability
boxplot(film$IMDB~film$Rating, main= "IMDB Scores by Rating",
        ylab= "IMDB Score", xlab= "MPAA Rating")

# Run ANOVA
modelscore <- aov(film$IMDB~film$Rating)
summary(modelscore)
#               Df Sum Sq Mean Sq F value Pr(>F)
# film$Rating   2   0.45  0.2245   0.304  0.738
# Residuals   148 109.39  0.7392  

# Run post-hod text if F statistic is significant
TukeyHSD(modelscore)
Tukey multiple comparisons of means
# 95% family-wise confidence level
# 
# Fit: aov(formula = film$IMDB ~ film$Rating)
# 
# $`film$Rating`
#                  diff        lwr       upr     p adj
# PG13-PG -0.0004909984 -0.3881883 0.3872063 0.9999950
# R-PG     0.1679487179 -0.4120567 0.7479541 0.7722832
# R-PG13   0.1684397163 -0.3552476 0.6921270 0.7271492

##
2147-782
# 1365

1365/34
# 40.14706
782/2
# 391

391/40.15
# 9.74








