dat <- read.csv(file='c:/CS_555_Assignment_2.csv', sep = ',')

# Q1 
# Summarize the data by whether children participated in the meal preparation or not. 
# Use an appropriately labeled table to represent data from different groups. 
# What is the different between mean of participant group to non-participant group? 
# (enter absolute value)
# diff = 10.2128
mean(dat$part) - mean(dat$nonpart)

# Q2
# Does the mean calorie consumption for those who participated in the meal preparation 
# differ from 425 Calories? Formally test at the alpha level 0.05 using the 5 steps 
# outlined in the module. Enter the p-value of your test. 
# (Rounded to 4 decimal places only, if very small enter 0)
# p-value = 0.02277

# use t.test with the data you're testing against a mean number provided
t.test(dat$part, mu = 425)

# Q3
# Calculate a 90% confidence interval for the mean calorie intake for participants 
# in the meal preparation. What is the lower bound?

# Use the t.test on desired data with conf.level.
# conf.int will return both lower and upper bounds
# Indexing into conf.int will return lower by using [1] and upper by using [2]
lower.bound <- t.test(dat$part, conf.level = 0.9)$conf.int[1]
upper.bound <- t.test(dat$part, conf.level = 0.9)$conf.int[2]

# Q4
# Formally test whether or not participants consumed more calories than non-participants 
# at the alpha=0.05 level. What is the t-statistics?
# t-statistics = 0.25083
t.test(dat$part, dat$nonpart, alternative="greater")

# Q5
# Formally test whether or not participants consumed more calories than non-participants 
# at the alpha=0.05 level. What is the p-value for this significant test? 
# (Rounded to 4 decimal places only, if very small enter 0)
# p-value = 0.4017
t.test(dat$part, dat$nonpart, alternative="greater")