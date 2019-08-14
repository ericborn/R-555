#library(ggplot2)
library(plotly)
install.packages('oddsratio')
library(oddsratio)

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
prop.test(x=tab['1',], n=colSums(tab), conf.level=0.95, correct=FALSE)

# 3)
# create dummy variables female(2) = 1, male(1) = 0
dat$s0 <- ifelse(dat$sex==2, 1, 0)

# What is the regression coefficient for girls in your logistic regression?

# logisitic regression with temp_level as the response
# and sex as the predictor or explanatory variable.
m <- glm(temp_level ~ sex, data = dat, family = 'binomial')

# create probabilities
dat$prob <- predict(m, type=c("response"))

# calculate odds ratio
exp(cbind(OR = coef(m), confint(m, level = 0.95)))

# What is the c-statistic (Area under ROC curve) of this model?

# ROC curve 
g <- roc(dat$temp_level ~ dat$prob)

# 0.7167
g

# Plot the ROC curve
plot(g)

# 4)
# Multiple logistic regression
m2 <- glm(temp_level ~ sex + heartRate, data = dat, family = 'binomial')

summary(m2)

# calculate odds ratio
exp(cbind(OR = coef(m2), confint(m2, level = 0.95))*10)

# create probabilities
dat$prob2 <- predict(m2, type=c("response"))

# ROC curve 
g2 <- roc(dat$temp_level ~ dat$prob2)

# 0.7167
g2

# Plot the ROC curve
plot(g2)
