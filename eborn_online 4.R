# Read the data into R
dat <- read.csv(file='c:/CS_555_Assignment_4.csv', sep = ',')

# 1)
#What is the Correlation Coefficient between prestige score and years of education?
# -0.0604238
cor(dat$Education, dat$Score)

# 2)
# What is the Correlation Coefficient between prestige score and income?
# -0.07710873
cor(dat$Score, dat$Income)

# 3)
# What is r-squared value for least squares regression that predicts prestige score from 
# education? (Enter the Adjusted R-squared)
# -0.006312 
m <- lm(dat$Education ~ dat$Score)
summary(m)