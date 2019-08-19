library(plotly)
library(corrplot)

# read the csv file into R
dat <- read.csv(file='C:/Users/TomBrody/Desktop/School/555/Final/SD-crime.csv', sep = ',')

# Remove columns 2-6, 8-9, 11-12, 15, 16 which are not being used during this analysis
crime.df <- dat[, c(1,7,10,13,14)]

# Take a look at the data structure after removing columns
head(crime.df)

# Histogram for crime distributions
c1 <- plot_ly(crime.df, x = ~Total.Violent.Crime, type = 'histogram', name = 'Violent Crime')
c2 <- plot_ly(crime.df, x = ~Total.Burglary, type = 'histogram', name = 'Burglary')
c3 <- plot_ly(crime.df, x = ~Total.Thefts, type = 'histogram', name = 'Thefts')
c4 <- plot_ly(crime.df, x = ~Motor.Vehicle.Theft, type = 'histogram', name = 'Vehicle Thefts')

# Generate the plot
subplot(c1, c2, c3, c4, nrows = 2) %>% 
  layout(title = "Crime Distributions")

# create a new column containing just the year
crime.df$year <- substr(crime.df$month, 5, 6)

# summary of each main category
summary(crime.df$Total.Violent.Crime)
summary(crime.df$Total.Burglary)
summary(crime.df$Total.Thefts)
summary(crime.df$Motor.Vehicle.Theft)

# create a new column to track changes from month to month
for (row in 1:nrow(crime.df)){
  crime.df$violent.change[row + 1] <- (crime.df$Total.Violent.Crime[row + 1]
                             - crime.df$Total.Violent.Crime[row])
}

for (row in 1:nrow(crime.df)){
  crime.df$burglary.change[row + 1] <- (crime.df$Total.Burglary[row  + 1]
                                   - crime.df$Total.Burglary[row])
}

for (row in 1:nrow(crime.df)){
  crime.df$thefts.change[row + 1] <- (crime.df$Total.Thefts[row + 1]
                                    - crime.df$Total.Thefts[row])
}

for (row in 1:nrow(crime.df)){
  crime.df$vehicle.change[row + 1] <- (crime.df$Motor.Vehicle.Theft[row + 1]
                                  - crime.df$Motor.Vehicle.Theft[row])
}

# create a dataframe containing only the monthly changes
crime.change <- crime.df[, c(6:9)]

# summary of those changes
summary(crime.change)

# create sums by year for each of the main categories
crime.year <- setNames(aggregate(list(crime.df$Total.Violent.Crime, crime.df$Total.Burglary, 
                      crime.df$Total.Thefts, crime.df$Motor.Vehicle.Theft), 
                 by=crime.df['year'], sum), c('year','violent', 'burglary', 'theft', 'vehicle'))

# summary of the data summed by year
summary(crime.year)

# create new columns to track yearly changes
crime.year$violent.change = 0
crime.year$burglary.change = 0
crime.year$thefts.change = 0
crime.year$vehicle.change = 0

# create new columns to track yearly percent changes
crime.year$violent.pct = 0
crime.year$burglary.pct = 0
crime.year$thefts.pct = 0
crime.year$vehicle.pct = 0

# calculate the amount change between years
for (row in 1:nrow(crime.year)){
  crime.year$violent.pct[row + 1] <- (crime.year$violent[row + 1]
                                   / crime.year$violent[row]) * 100
}

for (row in 1:nrow(crime.year)){
  crime.year$burglary.pct[row + 1] <- (crime.year$burglary[row + 1]
                                    / crime.year$burglary[row]) * 100
}

for (row in 1:nrow(crime.year)){
  crime.year$thefts.pct[row + 1] <- (crime.year$theft[row + 1]
                                  / crime.year$theft[row])* 100
}

for (row in 1:nrow(crime.year)){
  crime.year$vehicle.pct[row + 1] <- (crime.year$vehicle[row + 1]
                                    / crime.year$vehicle[row]) * 100
}


# calculate the amount change between years
for (row in 1:nrow(crime.year)){
  crime.year$violent.change[row + 1] <- (crime.year$violent[row + 1]
                                         - crime.year$violent[row])
}

for (row in 1:nrow(crime.year)){
  crime.year$burglary.change[row + 1] <- (crime.year$burglary[row + 1]
                                          - crime.year$burglary[row])
}

for (row in 1:nrow(crime.year)){
  crime.year$thefts.change[row + 1] <- (crime.year$theft[row + 1]
                                        - crime.year$theft[row])
}

for (row in 1:nrow(crime.year)){
  crime.year$vehicle.change[row + 1] <- (crime.year$vehicle[row + 1]
                                          - crime.year$vehicle[row])
}

# Percentage change from 2008 to 2018
round((1 - tail(crime.year$violent, 1) / crime.year$violent[1]) * 100, 2)
round((1 - tail(crime.year$burglary, 1) / crime.year$burglary[1]) * 100, 2)
round((1 - tail(crime.year$theft, 1) / crime.year$theft[1]) * 100, 2)
round((1 - tail(crime.year$vehicle, 1) / crime.year$vehicle[1]) * 100, 2)

# summary of the data summed by year
summary(crime.year)

# setup correlations beteen each category
cor.matrix <- cor(crime.df[, c(2:5)])

# Plot the correlations
corrplot(cor.matrix, method = 'number', type = 'lower', order = 'hclust')

# Highest correlation is between motor vehicle theft and burglary
# generate a scatter plot to observe the correlation
y <- list(title = "Motor Vehicle Theft")
x <- list(title = 'Burglary', categoryorder = 'array', categoryarray = 'row')
plot_ly(data = crime.df, x = ~Motor.Vehicle.Theft, y= ~Total.Burglary, 
        type = 'scatter') %>%
  layout(title = 'Total Burglaries vs Motor Vehicle Theft', xaxis = x, yaxis = y)
