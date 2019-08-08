library(ggplot2)

# 1)
# Read the data into R
dat <- read.csv(file='c:/CS_555_Assignment_5.csv', sep = ',')

# Count of students
# 15 in each group
table(dat$group)

#data.frame(table(dat$group))

is.factor(dat$group)

mean(dat$iq, by=list(dat$group))

aggregate(dat$age, by=list(dat$group), summary)
aggregate(dat$iq, by=list(dat$group), mean)

aggregate(dat$age, by=list(dat$group), sd)
aggregate(dat$iq, by=list(dat$group), sd)


anova(dat)
