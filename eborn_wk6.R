#library(ggplot2)
library(plotly)

# 1)
# Read the data into R
dat <- read.csv(file='c:/CS_555_Assignment_6.csv', sep = ',')

# creates a new column temp_level
# 1 if body temp >= 98.6, 0 if below < 98.6
dat$temp_level <- ifelse(dat$temp >= 98.6, 1, 0)
