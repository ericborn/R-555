# read the csv file into R
dat <- read.csv(file='C:/Users/TomBrody/Desktop/School/555/Final/SD-crime.csv', sep = ',')

# Take a look at the data structure
head(dat)

# Remove columns 2-6, 8-9, 11-12, 14, 16 which are not being used during this analysis
crime.df <- dat[, c(1,7,10,13,15)]

# Write cleaned data to csv
# write.csv(crime.df, file='C:/Users/TomBrody/Desktop/School/555/Final/SD-crime-clean.csv' )

# Take a look at the new data structure with removed columns
head(crime.df)

# create a new column containing just the year
crime.df$year <- substr(crime.df$month, 5, 6)






# summary of the entire dataset
summary(crime.df$Total.Violent.Crime)
summary(crime.df$Total.Burglary)
summary(crime.df$Total.Thefts)





reshape(crime.df)

# Histogram for IQ distributions
c1 <- plot_ly(crime.df, x = ~Total.Violent.Crime, type = 'histogram', name = 'Total.Violent.Crime')
c2 <- plot_ly(crime.df, x = ~Total.Burglary, type = 'histogram', name = 'Total.Burglary')
c3 <- plot_ly(crime.df, x = ~Total.Thefts, type = 'histogram', name = 'Total.Thefts')

subplot(c1, c2, c3, nrows = 2) %>% 
  layout(title = "Crime Distribution")

# drop empty factor levels
crime.df <- droplevels(crime.df)

# Reset rownames from 1 to n
rownames(crime.df) <- 1:nrow(crime.df)

# create a column containing with row numbers
# used to sort graph data chronologically
crime.df$row <- 1:nrow(crime.df)


# Violent Crimes commited by year 
y <- list(title = "Violent Crimes")
x <- list(title = 'Month', categoryorder = 'array', categoryarray = 'row')
plot_ly(data = crime.df, x = ~month, y= ~Total.Violent.Crime, type = 'bar') %>%
  layout(title = 'Total Violent Crimes commited by year', xaxis = x, yaxis = y)


# Violent Crimes Line plot
y <- list(title = "Violent Crimes")
x <- list(title = 'Month', categoryorder = 'array', categoryarray = 'row')
plot_ly(data = crime.df, x = ~month, y= ~Total.Violent.Crime, type = 'scatter', mode = 'lines') %>%
  layout(title = 'Total Violent Crimes commited by year', xaxis = x, yaxis = y)

# Violent Crimes Line plot, smoothed spline as line
plot_ly(data = crime.df, x = ~month) %>%
  add_lines(y = ~Total.Violent.Crime, name = "spline", line = list(shape = "spline"))

