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

# 4)
# Calculate the least squares regression equation that predicts prestige score from education, 
# income and percentage of women. What is the regression coefficient (beta) for the income?
# -0.0003433
mlr.fit <- lm(Score ~ Education + Income + WorkforceWomen)
summary(mlr.fit)

# 5)
# Calculate the least squares regression equation that predicts prestige score 
# from education, income and percentage of women. Formally test whether the set of 
# these predictors are associated with prestige at the alpha= 0.05 level. What is the F-statistic?
# 0.407
summary(mlr.fit)

# 6)
# Calculate the least squares regression equation that predicts prestige score from 
# education, income and percentage of women. What is r-squared value for least squares 
# regression ? (Enter the Adjusted R-squared)
# -0.01793
summary(mlr.fit)

# Calculate the least squares regression equation that predicts prestige score from education, 
# income and percentage of women. Formally test whether income is a significant predictor 
# associated with prestige score at the alpha= 0.05 level. What is the t-statistic for income?
# -0.867
summary(mlr.fit)