library(ggplot2)
library(plotly)

# 1)
# Read the data into R
dat <- read.csv(file='c:/CS_555_Assignment_4.csv', sep = ',')

# Generate a scatter plot between education and score.
ggplot(dat, aes(x=Education, y=Score)) +
  geom_point(color='blue') +
  xlab('Years of Education') + ylab ('Prestige Score') +
  ggtitle("Scatterplot between years of education and prestige score") +
  theme(plot.title = element_text(hjust = 0.5))

# Calculate the correlation between the two features.
# -0.0604238
cor(dat$Education, dat$Score)

# 2)
# Predict score from education
# Fit the linear model
fit <- lm(Score ~ Education, data=dat)

# Predicted score values from education
dat$predicted <- predict(fit)

# compute the residual
dat$residual <- resid(fit)

# write new csv file with added columns
#write.csv(dat, file = 'data.csv')

# plot the residual
qplot(dat$Education, dat$residual) +
  geom_point(color='blue') +
  geom_hline(yintercept = 0, color = 'red') +
  ylab('Residuals') + xlab('Education') +
  ggtitle("Residual plot for education and prestige score") +
  theme(plot.title = element_text(hjust = 0.5))

# Using some filtering on residual I found there are 10 rows 
# where the residual is > 35 or less -35
dat[dat$residual > 35 | dat$residual < -35, ]

# Filtering on education there is only 1 row with education
# greater than 17 and 1 row with it less than 4
dat[dat$Education > 17 | dat$Education < 5, ]

# reversing the signs and switching the 'or' symbol to the 'and' symbol
# to create a new dataset to plot and evaluate
new.dat <- dat[dat$residual < 35 & dat$residual > -35, ]
new.dat <- new.dat[new.dat$Education < 17 & new.dat$Education > 5, ]

# Create a new fit
new.fit <- lm(Score ~ Education, data=new.dat)

# New predicted score values from education
new.dat$predicted <- predict(new.fit)

# new residuals
new.dat$residual <- resid(new.fit)

# Correlation between the two attributes
cor(new.dat$Education, new.dat$Score)

# plot with very large and small residuals and education removed 
qplot(new.dat$Education, new.dat$residual) +
  geom_point(color='blue') +
  geom_hline(yintercept = 0, color = 'red') +
  ylab('Residuals') + xlab('Education') +
  ggtitle("New reduced Residual plot") +
  theme(plot.title = element_text(hjust = 0.5))
  

# 3)
# attach dataset dat
attach(dat)

# Create a linear model predicting Score 
# from Education + Income + WorkforceWomen
mlr.fit <- lm(Score ~ Education + Income + WorkforceWomen)

# Observe the models variances
anova(mlr.fit)

#f distribution F3,98,0.05
# 95% confidence, alpha 0.05, degrees of freedom 3 and 98
# n = sample size, k = number of predictors, - 1
# res df = n - k - 1
# res df = 102 - 3 - 1 = 98
# 2.697423
qf(0.95, 3, 98)

# Observe the F-statistic with summary
# 0.407 > 2.697423
summary(mlr.fit)

# 4)
# Evaluate the individual predictors
# Using right hand tailed T test a = 0.975, df = 98
# T value needs to be >= 1.984467
qt(0.975, 98)

summary(mlr.fit)

confint(mlr.fit, level = 0.95)

detach(dat)