library(ggplot2)
library(plotly)

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

# Age distributions
age1 <- plot_ly(chem.stu, x = ~age, type = 'histogram', name = 'chemistry')
age2 <- plot_ly(math.stu, x = ~age, type = 'histogram', name = 'math')
age3 <- plot_ly(phys.stu, x = ~age, type = 'histogram', name = 'physics')

subplot(age1, age2, age3, nrows = 2) %>% 
  layout(title = "Age Distribution")

# IQ distributions
iq1 <- plot_ly(chem.stu, x = ~iq, type = 'histogram', name = 'chemistry')
iq2 <- plot_ly(math.stu, x = ~iq, type = 'histogram', name = 'math')
iq3 <- plot_ly(phys.stu, x = ~iq, type = 'histogram', name = 'physics')

subplot(iq1, iq2, iq3, nrows = 2) %>% 
  layout(title = "IQ Distribution")

iq1 <- plot_ly(chem.stu, x = ~iq, type = 'histogram', name = 'chemistry')
iq2 <- plot_ly(math.stu, x = ~iq, type = 'histogram', name = 'math')
iq3 <- plot_ly(phys.stu, x = ~iq, type = 'histogram', name = 'physics')

# Whole dataset scatter
y <- list(title = "IQ")
x <- list(title = 'Age')
plot_ly(data = dat, x = ~age, y= ~iq, color = ~group, type = 'scatter') %>%
  layout(title = 'IQ vs Age distribution', xaxis = x, yaxis = y)


#data.frame(table(dat$group))

is.factor(dat$group.Chem)

mean(dat$iq, by=list(dat$group))

aggregate(dat$age, by=list(dat$group), summary)
aggregate(dat$iq, by=list(dat$group), mean)

aggregate(dat$age, by=list(dat$group), sd)
aggregate(dat$iq, by=list(dat$group), sd)


anova(dat)
