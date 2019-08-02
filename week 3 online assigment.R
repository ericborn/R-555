# 1)
# What is the correlation coefficient for the number of meals eaten that contain 
# fish (per week) and mercury levels?

# Read the data into R
dat <- read.csv(file='c:/CS_555_Assignment_3.csv', sep = ',')

cor(dat$NumFishMeals, dat$TotalMercury)

# 2)
# What is the estimate for beta_1?
# slope 0.386759
m <- lm(dat$TotalMercury ~ dat$NumFishMeals)
print(m)

# 3)
# What is the estimate for beta_0 (y-intercept)?
# Intercept 2.4598
print(m)

# 4)
# What is the r-squared value? (Enter here the Multiple R-squared and not the Adjusted R-squared)
# 0.5473
summary(m)

# 5)
# Formally test the hypothesis that beta_1 = 0 using the global F-test. 
# What is the p-value value? (Rounded to 4 decimals, if very small enter 0)
# p = 2.2e-16 = -0.00000000000000022 = 0
aova(m)

# 6)
# What is the lower bound value for the 90% confidence interval of beta_1?
# 0.3277582 rounded up to 0.328
confint(m, level = 0.90)