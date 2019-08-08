# Read the data into R
dat <- read.csv(file='c:/CS_555_Assignment_5.csv', sep = ',')

# how many students are in each group?
# Count of students
# 15 in each group
table(dat$group)


# What is the difference of mean IQ of Chemistry student group with Physics student group? 
# (Enter the absolute value difference)
# 47.46667 - 34.9333 = 12.53337
aggregate(dat$iq, by=list(dat$group), mean)


# What is the difference of standard deviations of IQ of Chemistry students with Physics? 
# (Enter the absolute value difference)
# 0.7432234 - 1.2227993 = 0.4795759,
aggregate(dat$iq, by=list(dat$group), sd)



# What is the difference of standard deviations of age of Chemistry students with Physics? 
# (Enter the absolute value difference)
# 0.7988086 - 0.9904304 = 0.1916218
aggregate(dat$age, by=list(dat$group), sd)


# Perform a one way ANOVA to assess if the IQ scores vary by student group? 
# What is the F Value for this test?
# create fit an analysis of variance model
my_anova <- aov(dat$iq~dat$group)

# display summary of model
# F value = 512
summary(my_anova)

# Perform pairwise comparisons using Tukeys procedure at the 95% family-wise confidence level 
# to adjust for multiple comparisons. What is mean difference upper-level of Math student 
# compared to Physics student?
# -1.869492
TukeyHSD(my_anova)


# Create an appropriate number of dummy variables for student group and re-run the one-way 
# ANOVA using the lm function with the newly created dummy variables. 
# Set chemistry students as the reference group. What is the t-value from the regression model 
# for physics students?

# declare dummy variables for each of the group types
dat$g0 <- ifelse(dat$group=='Chemistry student', 1, 0)
dat$g1 <- ifelse(dat$group=='Math student', 1, 0)
dat$g2 <- ifelse(dat$group=='Physics student', 1, 0)

# Exclude reference group when creating the lm
# Excluded group was Chemistry student g0
m2 <- lm(dat$iq ~ dat$g1 + dat$g2, data = dat)

# print summary
# t value of physics g2 was -30.54
summary(m2)
