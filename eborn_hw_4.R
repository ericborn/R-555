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
# Simple linear regression
ggplot(dat, aes(x=Education, y=Score)) +
  geom_point(color='blue') +
  geom_smooth(color='red', method = 'lm', se = FALSE) +
  xlab('Years of Education') + ylab ('Prestige Score') +
  ggtitle("Scatterplot between years of education and prestige score") +
  theme(plot.title = element_text(hjust = 0.5))

# Boxplot for education
ylab <- list(title = "Education")
plot_ly(dat, y = ~Education, name = 'Education', type = 'box')%>%
  layout(yaxis = ylab, title = 'Years of Education')

# Boxplot for score
ylab <- list(title = "Score")
plot_ly(dat, y = ~Score, name = 'Score', type = 'box')%>%
  layout(yaxis = ylab, title = 'Prestige Score')

min(dat$Education)
max(dat$Education)

subset(dat, Education < 5 | Education > 17)

min(dat$Score)
max(dat$Score)
subset(dat, Score < 18 | Score > 90)
