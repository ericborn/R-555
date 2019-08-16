#library(ggplot2)
library(plotly)
library(pROC)

# 1)
# Read the data into R
# 1=males, 2 =females
dat <- read.csv(file='c:/CS_555_Assignment_6.csv', sep = ',')

# creates a new column temp_level
# 0 if below < 98.6, 1 if body temp >= 98.6
dat$temp_level <- ifelse(dat$temp >= 98.6, 1, 0)

# create comparison table between temp level and sex
tab <- table(dat$temp_level, dat$sex)

# Output comparison table
tab

# 2)
# total male less than 98.6 / total males
prop.male   <- tab[2,1]/colSums(tab)[1]

# total female less than 98.6 / total females
prop.female <- tab[2,2]/colSums(tab)[2]

# male risk minus female risk
risk.diff <- prop.male - prop.female

# test proportions
# male/female above 98.6 as x, total male/female as y
# tab 1 = higher body temp across both male and female
# n=colsums(tab) is calculating the sum by column
prop.test(x=tab['1',], n=colSums(tab), conf.level=0.95, correct=FALSE)


# proportion of males/females with high temp at a = 0.05
# -z to z value is +- 1.960
-0.4307693 + 1.960 * sqrt((0.2461538*(1 - 0.2461538) / 65) + (0.6769231*(1 - 0.6769231) / 65))
-0.4307693 - 1.960 * sqrt((0.2461538*(1 - 0.2461538) / 65) + (0.6769231*(1 - 0.6769231) / 65))

# formally test if the proportion of men and women have higher than normal body temp
(0.2461538 - 0.6769231) / sqrt((16+44)/(65+65)*(1-(16+44)/(65+65))*(1/65+1/65))
0.4307693 / sqrt(0.4615385*(0.5384615)*(0.03076923))
(0.6769231 - 0.2461538) / sqrt(0.007646791)


# 3)
# create dummy variables female(2) = 1, male(1) = 0
dat$sex_variable <- ifelse(dat$sex==2, 1, 0)

# What is the regression coefficient for girls in your logistic regression?

# logisitic regression with temp_level as the response
# and sex as the predictor or explanatory variable.
m <- glm(temp_level ~ sex_variable, data = dat, family = 'binomial')

summary(m)

confint(m)

# create probabilities
dat$prob <- predict(m, type=c("response"))

# calculate odds ratio
exp(cbind(OR = coef(m), confint(m, level = 0.95)))

#exp(coef(m))

# What is the c-statistic (Area under ROC curve) of this model?
# ROC curve 
g <- roc(dat$temp_level ~ dat$prob)

# 0.7167
g

# Plot the ROC curve
plot(g)

# 4)
# Multiple logistic regression
m2 <- glm(temp_level ~ sex_variable + heartRate, data = dat, family = 'binomial')

summary(m2)

confint(m2)

exp(summary(m2)$coefficients["DSH",1] + 
qnorm(c(0.025,0.5,0.975)) * summary(m)$coefficients["DSH",2])

# calculate odds ratio
exp(confint.default((m1))*10)

# create probabilities
dat$prob2 <- predict(m2, type=c("response"))

# ROC curve 
g2 <- roc(dat$temp_level ~ dat$prob2)

# 0.7167
g2

# Plot the ROC curve
plot(g2)
