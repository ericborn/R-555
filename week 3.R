# 1)
# Read the data into R
dat <- read.csv(file='c:/CS_555_Assignment_3.csv', sep = ',')

# 2)
# The x-axis should be the explanatory or independent variable, which is generally the 
# one that occurs first. For this I chose the number of fish meals per week, as the fishermen 
# would need to eat the fish before their mercury levels could rise. The y-axis is the response 
# or dependent variable, which would be the response to eating fish each week and is represented 
# by the mercury levels in fisherman. 
ggplot(dat, aes(x=NumFishMeals, y=TotalMercury)) +
  geom_point(color='blue') +
  xlab('Number of fish meals (per week)') + ylab ('Mercury levels in fisherman') +
  ggtitle("Scatterplot of fish meals (per week) vs mercury levels in fisherman") +
  theme(plot.title = element_text(hjust = 0.5))

# 3)
# Correlation coefficient - 0.7398083
cor(dat$NumFishMeals, dat$TotalMercury)

# 4)
# Regression line added with geom_smooth, method = lm which is linear model
ggplot(dat, aes(x=NumFishMeals, y=TotalMercury)) +
  geom_point(color='blue') +
  geom_smooth(color='red', method = 'lm', se = FALSE) +
  xlab('Number of fish meals (per week)') + ylab ('Mercury levels in fisherman') +
  ggtitle("Scatterplot of fish meals (per week) vs mercury levels in fisherman") +
  theme(plot.title = element_text(hjust = 0.5))

# 5)
xbar <- mean(dat$NumFishMeals)
sx <- sd(dat$NumFishMeals)
ybar <- mean(dat$TotalMercury)
sy <- sd(dat$TotalMercury)
r <- cor(dat$NumFishMeals, dat$TotalMercury)

beta1 <- r*sy/sx
beta1

beta0 <- ybar - beta1*xbar
beta0

m <- lm(dat$TotalMercury ~ dat$NumFishMeals)
summary(m)

# 6)
# Anova
anova(m)

# f-test
# 2.75743
qf(0.90, 1, 98)

# Confidence interval at 90%
# 0.3277582 0.4457599
confint(m, level = 0.90)

# Reg SS  Res SS
# 1394.2  1153.2
reg.ss <- 1394.2
res.ss <- 1153.2

total.ss <- reg.ss + res.ss

r.squared <- reg.ss / total.ss
