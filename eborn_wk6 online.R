#library(ggplot2)
library(plotly)
library(aod)

# 1)
#calculate the absolute value and enter a number in percent - between 0 and 100)
# 1=males, 2 =females
dat <- read.csv(file='c:/CS_555_Assignment_6.csv', sep = ',')

# creates a new column temp_level
# 1 if body temp >= 98.6, 0 if below < 98.6
dat$temp_level <- ifelse(dat$temp >= 98.6, 1, 0)


# create comparison table between temp level and sex
tab <- table(dat$temp_level, dat$sex)

# setup proportions for male and above/below body temp
prop.male.dat   <- tab[2,1]/colSums(tab)[1]
prop.female.dat <- tab[2,2]/colSums(tab)[2]

# Take the absoluate value from male minus female high body temp
# 43
abs(prop.male.dat-prop.female.dat)

# 2)
# Formally test (at the alpha = 0.05 level) whether the proportion of people with higher 
# body temperatures (greater than or equal to 98.6) is the same across men and women, 
# based on this effect measure. What is the X-squared of this test?
# 24.267
prop.test(x=tab["1",], n=colSums(tab), correct=FALSE)

# 3)
# create dummy variables female(2) = 1, male(1) = 0
dat$s0 <- ifelse(dat$sex==2, 1, 0)

# logisitic regression with temp_level as the response
# and sex as the predictor or explanatory variable.
m<-glm(temp_level ~ sex, data = dat, family = 'binomial')

summary(m)


