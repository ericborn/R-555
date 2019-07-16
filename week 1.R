# load library plotly, used for visualizations
library(plotly)

# 1)
# Read the csv file
dat <- read.csv(file="c:/CS_555_Assignment_1.csv", sep = ",")

# Create a label for the box plot
names(dat) <- c("Duration")

# Initalize the boxplot
box <- plot_ly(dat, y = ~Duration, type = "box")%>%
  layout(title = "Hospital stays measured in days")

# Output the plot
box

# 2)
# Create label and initalize the hisogram
ylab <- list(title = "Number of patients")
xlab <- list(title = 'Duration in days')
hist <- plot_ly(x = dat$Duration, type = "histogram")%>%
  layout(xaxis = xlab, yaxis = ylab, title = "Hospital stays with C. Difficile")

# output the histogram
hist

# 3)

# Build a dataframe containing the summary data
staySummary <- data.frame("Measure" = c("Mean", "Median", "Standard Dev", "1st Quartile", "3rd Quartile", "Min", "Max"),
                          "Value" = c(round(mean(dat$Duration), digits = 1), median(dat$Duration),
                                      round(sd(dat$Duration), digits = 2), unname(quantile(dat$Duration, 0.25)),
                                      unname(quantile(dat$Duration, 0.75)), min(dat), max(dat)))

# Convert factors to character
staySummary$Measure <- as.character(staySummary$Measure)

# Create plot using previously created dataframe
stat.table <- plot_ly(
  type = 'table',
  height = 250,
  width = 500,
  header = list(
    values = c('Measure', 'Value'),
    line = list(width = 1, color = 'black'),
    fill = list(color = c('#1f77b4', '#1f77b4')),
    font = list(famile = 'Arial', size = 14, color = 'white')
  ),
  cells = list(
    values = rbind(staySummary$Measure, staySummary$Value),
    align = c('center'),
    line = list(width = 1, color = 'black')
  ))

# Output table
stat.table