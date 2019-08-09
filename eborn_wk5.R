#library(ggplot2)
library(plotly)
library(car)

# 1)
# Read the data into R
dat <- read.csv(file='c:/CS_555_Assignment_5.csv', sep = ',')

# Count of students
# 15 in each group
table(dat$group)

# summary of the entire dataset
summary(dat)

# Split the data into three groups by type
chem.stu <- dat[dat$group=='Chemistry student',]
math.stu <- dat[dat$group=='Math student',]
phys.stu <- dat[dat$group=='Physics student',]

# Review a summary of each group
summary(chem.stu)
summary(math.stu)
summary(phys.stu)

# Histogram for Age distributions
age1 <- plot_ly(chem.stu, x = ~age, type = 'histogram', name = 'chemistry')
age2 <- plot_ly(math.stu, x = ~age, type = 'histogram', name = 'math')
age3 <- plot_ly(phys.stu, x = ~age, type = 'histogram', name = 'physics')

subplot(age1, age2, age3, nrows = 2) %>% 
  layout(title = "Age Distribution")

# Boxplot for age by group
y <- list(title = "Age")
x <- list(title = 'Student group')
plot_ly(dat, x =~group, y = ~age, type = 'box') %>%
  layout(title = 'Age distribution by Student group', xaxis = x, yaxis = y)

# Histogram for IQ distributions
iq1 <- plot_ly(chem.stu, x = ~iq, type = 'histogram', name = 'chemistry')
iq2 <- plot_ly(math.stu, x = ~iq, type = 'histogram', name = 'math')
iq3 <- plot_ly(phys.stu, x = ~iq, type = 'histogram', name = 'physics')

subplot(iq1, iq2, iq3, nrows = 2) %>% 
  layout(title = "IQ Distribution")

# Boxplot for IQ by group
y <- list(title = "IQ")
x <- list(title = 'Group')
plot_ly(dat, x =~group, y = ~iq, type = 'box') %>%
  layout(title = 'IQ distribution by group type', xaxis = x, yaxis = y)

# Whole dataset scatterplot
y <- list(title = "IQ")
x <- list(title = 'Age')
plot_ly(data = dat, x = ~age, y= ~iq, color = ~group, type = 'scatter') %>%
  layout(title = 'IQ vs Age distribution', xaxis = x, yaxis = y)

# 2)
# Fit a one way anova model
my_anova <- aov(dat$iq~dat$group)

# F value = 512
# MSB = 646.9
# MSW = 1.3
summary(my_anova)
f <- 512

# Statistic value
# 497.6154
value <- 646.9 / 1.3

# testing if the statistic value is less than the f value returns True
# Therefore we cannot reject the null hypothesis
value < f

# 3)
# Create dummy variables for the dataset
dat$g0 <- ifelse(dat$group=='Chemistry student', 1, 0)
dat$g1 <- ifelse(dat$group=='Math student', 1, 0)
dat$g2 <- ifelse(dat$group=='Physics student', 1, 0)

# Reference group is Chemistry student g0, which is excluded from the model
m2 <- lm(dat$iq ~ dat$g1 + dat$g2, data = dat)

# Comparing the two models using aov
aov(my_anova)
aov(m2)

# Comparing summaries of both models
summary(m1)
summary(m2)

# 4)
# ANCOVA
# Now we run ANVOA with adjusting for age 
Anova(lm(dat$iq ~ dat$group), type=3)

Anova(lm(dat$iq ~ dat$group + dat$age), type=3)

