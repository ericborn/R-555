#library(ggplot2)
library(plotly)

# 1)
# Read the data into R
# 1=males, 2 =females
dat <- read.csv(file='c:/CS_555_Assignment_6.csv', sep = ',')

# creates a new column temp_level
# 1 if body temp >= 98.6, 0 if below < 98.6
dat$temp_level <- ifelse(dat$temp >= 98.6, 1, 0)

# create comparison table between temp level and sex
tab <- table(dat$temp_level, dat$sex)

# Output comparison table
tab

# 2)
m <- glm(dat$temp_level ~ dat$sex, family = "binomial")

# predict risk for each patient
risk <-predict(m, type=c("response"))

risk 


# setup proportions for male and above/below body temp
prop.male.dat   <- tab[2,1]/colSums(tab)[1]
prop.female.dat <- tab[2,2]/colSums(tab)[2]

# Take the absoluate value from male minus female high body temp
# 43
abs(prop.male.dat-prop.female.dat)