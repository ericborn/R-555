library(plotly)
# Read the csv file
dat <- read.csv(file='c:/CS_555_Assignment_2.csv', sep = ',')

# 1)
# Summarize the data
summary(dat)
sd(dat$nonpart)
sd(dat$part)

# nonpart           part      
# Min.   :118.1   Min.   :  0.0  
# 1st Qu.:254.6   1st Qu.:221.4  
# Median :346.2   Median :361.9  
# Mean   :327.1   Mean   :337.3  
# 3rd Qu.:382.3   3rd Qu.:433.1  
# Max.   :500.1   Max.   :653.2 
# SD     :94.6    SD     :180.3


# Boxplot fot the data
ylab <- list(title = "Meal Calories")
plot_ly(dat, y = ~nonpart, name = 'non-participant', type = 'box')%>%
  add_trace(y = ~part, name = 'participant')%>%
  layout(yaxis = ylab, title = 'Childrens meal participation and calories')

# Participant children had a lower minimum and higher maximum calorie value than non-participants.
# 0 to 118 for the minimum. Since the lowest values are 0's, I have a feeling its bad data.
# If the 0's are thrown out, the lowest value would be 177.38 compared to 118 or a difference of 59.38.
# 653.2 to 500.1 for the maximum values, a difference of 153.1.
# The means are only 10.2 calories higher for the participants over the non-participants, 337.27 to 327.05.
# If the 0's are excluded from the means the participants raises up to 383.26 an increase of 49.02

# 2)
# side-by-side histograms for the data

yAxis = seq(1,9)

# Non-participant
nonPart <- plot_ly(dat, x = ~nonpart, name = 'non-participant', type = 'histogram')

# participant
part <- plot_ly(dat, x = ~part, name = 'participant', type = 'histogram')

# Draw plots for both groups
ylab <- list(title = 'Total Children')
xlab <- list(title = 'Meal Calories')
subplot(nonPart, part)%>%
  layout(xaxis = xlab, yaxis = ylab, title = 'Childrens meal participation and calories')

# The side-by-side plots aren't the best to use as comparisons as their Y-axis scales
# aren't even and I was unable to figure out how to adjust the plots to match.
# I created a set of stacked histograms instead which allows easier comparison.

# Stacked histograms
xlab <- list(title = 'Meal Calories')
ylab <- list(title = 'Total Children')
plot_ly(dat, x = ~nonpart, name = 'non-participant', type = 'histogram')%>%
  add_trace(x = ~part, name = 'participant')%>%
  layout(xaxis = xlab, title = 'Childrens meal participation and calories')

# The non-participants have less overall range than the participants, 100-500 calories
# with most falling between the 200-400 calorie range.
# The participant group has a wider range from 0-700 with a shorter peak of 7 children
# at the 400 calorie range.

# 3)
# step 1. Set up the hypotheses and select the alpha level
# The hypothesis is that participants consumed more calories than non-participants

# Null hypothesis
# H0: u = 327.1 (participants consumed the same amount of calories as non-participants)

# Hypothesis to test
# h1 : u > 327.1 (participants consumed more calories than non-participants)

# The alpha level being used is 0.05 or 5% which is 95% confidence.

# step 2. Select the appropriate test statistic
# For this test I'll be using a one sided hypothesis and a test of significance since 
# we're measuring if participants ate more calories than non-participants. 

# step 3. State the decision rule
# The decision rule will be that if the participants consumed more calories than non-participants
# Reject if p <= alpha (0.05)

# step 4. Compute the test statistic and the associated p-value
ttest1 <- t.test(dat$part, dat$nonpart, alternative="greater", conf.level=0.05)

print(ttest1)

#p-value = 0.4017

# step 5. State your conclusion
# 0.4017 > 0.05 so the null hypothesis cannot be rejected and there may not be a difference
# between non-participants and participants