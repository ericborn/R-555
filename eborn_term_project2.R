# read the csv file into R
dat <- read.csv(file='C:/Users/TomBrody/Desktop/School/555/Final/SD-crime.csv', sep = ',')

# Take a look at the data structure
head(dat)

# Remove columns 2-6, 8-9, 11-12, 14, 16 which are not being used during this analysis
crime.df <- dat[, c(1,7,10,13,15)]

# create a new column containing just the year
crime.df$year <- substr(crime.df$month, 5, 6)

# summary of the entire dataset
summary(crime.df$Total.Violent.Crime)
summary(crime.df$Total.Burglary)
summary(crime.df$Total.Thefts)


crime.df$Total.Violent.Crime[1]

for (row in 1:length(crime.df$Total.Violent.Crime)){
  crime.df$violent.change[row] <- (crime.df$Total.Violent.Crime[row]
                             - crime.df$Total.Violent.Crime[row + 1])
}


if (crime.df$Total.Violent.Crime[row] < crime.df$Total.Violent.Crime[row + 1]){
  print
}


aggregate(crime.df['Total.Violent.Crime'], by=crime.df['year'], sum)

agg <- aggregate(list(crime.df$Total.Violent.Crime, crime.df$Total.Burglary, 
                      crime.df$Total.Thefts, crime.df$Total.Property.Crime), 
                 by=crime.df['year'], sum)

agg$violent <- agg$c.884L..969L..1000L..1255L..1053L..964L..1169L..1080L..1141L..
agg$burglary <- agg$c.1288L..1408L..1432L..1494L..1382L..1298L..1399L..1278L..1412L..
agg$thefy <- agg$c.3801L..3810L..3723L..3994L..3854L..3889L..3922L..3817L..3896L..
agg$property <- agg$c.6939L..6908L..6735L..7312L..6795L..6756L..6966L..6602L..7238L..


crime.year <- agg[, c(6:9)]








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

