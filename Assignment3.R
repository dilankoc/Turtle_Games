### Week 4 : Visualise the data
# ------------------------------------------------------------------------------

# Import libraries.
library(tidyverse)

# Import the data set.
dataset <- read.csv('turtle_sales.csv')

dim(dataset)

# Remove redundant columns.
turtle <- select(dataset, -c('Ranking', 'Year', 'Genre', 'Publisher'))

# Check if the columns has removed.
names(turtle)
dim(turtle)

# Save the clean dataset.
# write.csv(turtle_sales, 'turtle_sales_clean.csv')

# Convert Product data type.
# turtle_sales <- mutate(turtle_sales, Product = as.factor(Product))

# Check the summary of the data set.
summary(turtle)

as_tibble(turtle)

# Check if the data set has any missing values.
sum(is.na(turtle))

# lapply(lapply(turtle_sales, is.na), table)

# Create a scatterplot.
qplot(y = Global_Sales, data = turtle)
qplot(y = NA_Sales, data = turtle)
qplot(y = EU_Sales, data = turtle)

# Create a histogram.
qplot(Global_Sales, data = turtle, bins = 40)
qplot(NA_Sales, data = turtle, bins = 40)
qplot(EU_Sales, data = turtle, bins = 40)

# Create a boxplot.
qplot(Global_Sales, data = turtle, geom = 'boxplot')
qplot(NA_Sales, data = turtle, geom = 'boxplot')
qplot(EU_Sales, data = turtle, geom = 'boxplot')


# turtle_sales dataset doesn't follow a normal distribution. 
# Outliers exists.

# -----------------------------------------------------------------------------
### Week 5 : Clean, manipulate, and visualise the data.
# -----------------------------------------------------------------------------

# Import libraries.
library(moments)

### Remove the outliers.

# Using boxplot to detect the presence of outliers.
boxplot(turtle[, c('Global_Sales', 'NA_Sales', 'EU_Sales')])

# Eliminate outliers.
#for (x in c('Global_Sales', 'NA_Sales', 'EU_Sales'))
#{
# value = turtle_sales[,x][turtle_sales[,x] %in% boxplot.stats(turtle_sales[,x])$out]
# turtle_sales[,x][turtle_sales[,x] %in% value] = NULL
#}

# Aggregate and group the data set.
turtle_sales <- turtle %>% group_by(Product) %>%
  summarise(Global_sum = sum(Global_Sales), 
            NA_sum = sum(NA_Sales),
            EU_sum = sum(EU_Sales),
            .groups = 'drop')

print(turtle_sales)

# Sort the variables to identify the product which sold the most and the least.
# arrange(turtle_sales_sum, des(Global_Sales_sum))

turtle_sales_G <- turtle_sales[order(turtle_sales$Global_sum),]
turtle_sales_N <- turtle_sales[order(turtle_sales$NA_sum),]
turtle_sales_E <- turtle_sales[order(turtle_sales$EU_sum),]

## Determine min, max, and mean values.

# Global_Sales
min(turtle_sales$Global_sum)
max(turtle_sales$Global_sum)
mean(turtle_sales$Global_sum)

# NA_Sales.
min(turtle_sales$NA_sum)
max(turtle_sales$NA_sum)
mean(turtle_sales$NA_sum)

# EU_Sales.
min(turtle_sales$EU_sum)
max(turtle_sales$EU_sum)
mean(turtle_sales$EU_sum)


## Determine the normality of the data set using histogram and boxplot.

# Global Sales.
hist(turtle_sales$Global_sum)
boxplot(turtle_sales$Global_sum)

# EU_Sales.
hist(turtle_sales$EU_sum)
boxplot(turtle_sales$EU_sum)

# NA_Sales.
hist(turtle_sales$NA_sum)
boxplot(turtle_sales$NA_sum)

### Determine the normality of the data set with qqplot.

# Global_Sales.
qqnorm(turtle_sales$Global_sum)
qqline(turtle_sales$Global_sum)

# NA_Sales.
qqnorm(turtle_sales$NA_sum)
qqline(turtle_sales$NA_sum)

# EU_Sales.
qqnorm(turtle_sales$EU_sum)
qqline(turtle_sales$EU_sum) 

### Determine the normality with Shapiro-Wilk Test.
shapiro.test(turtle_sales$Global_sum)
shapiro.test(turtle_sales$NA_sum)
shapiro.test(turtle_sales$EU_sum)

### Determine the normality with skewness and kurtosis.

# Global_Sales.
skewness(turtle_sales$Global_sum)
kurtosis(turtle_sales$Global_sum)

# NA_Sales.
skewness(turtle_sales$NA_sum)
kurtosis(turtle_sales$NA_sum)

# EU_Sales.
skewness(turtle_sales$EU_sum)
kurtosis(turtle_sales$EU_sum)


### Determine the correlation.

# Check correlation coefficient between Global Sales and NA Sales
cor(turtle_sales$NA_sum, turtle_sales$Global_sum)

# Check correlation coefficient between Global Sales and EU Sales
cor(turtle_sales$EU_sum, turtle_sales$Global_sum)

# Check correlation coefficient between Global Sales and EU Sales
cor(turtle_sales$EU_sum, turtle_sales$NA_sum)

# Check correlation coefficient between Global Sales and EU Sales + NA Sales
cor(turtle_sales$NA_sum + turtle_sales$EU_sum, turtle_sales$Global_sum)

## Strong positive correlation. ##

# Creeate a scatter plot.
ggplot(turtle_sales, 
       aes(EU_sum, Global_sum)) +
  geom_point(alpha = .5, size = 3) +
  geom_smooth(method = 'lm', se = FALSE, size = 1.5) +
  labs(title = "Relationship between Global sales and Europe sales",
       x = "Europe Sales",
       y = "Global Sales")

ggplot(turtle_sales, aes(NA_sum, Global_sum)) +
  geom_point(alpha = .5, size = 3) +
  geom_smooth(method = 'lm', se = FALSE, size = 1.5) +
  labs(title = "Relationship between North America sales and Global sales",
       x = "North America Sales",
       y = "Global Sales")

ggplot(turtle_sales, aes(EU_sum, NA_sum)) +
  geom_point(alpha = .5, size = 3) +
  geom_smooth(method = 'lm', se = FALSE, size = 1.5) +
  labs(title = "Relationship between Europe sales and North America sales",
       x = "Europe Sales",
       y = "North America sales")


## We can confirm that data is not normally distributed hence may not be suitable for 
# regression analysis. Data normalisation required before regression.


# -----------------------------------------------------------------------------
### Week 6: Making recommendations to the business
# -----------------------------------------------------------------------------
# Try to normalise the data with log() transformarion.
turtle$log_Global_Sales <- log(turtle$Global_Sales)
turtle$log_NA_Sales <- log(turtle$NA_Sales)
turtle$log_EU_Sales <- log(turtle$EU_Sales)

### Determine the normality with skewness and kurtosis.

# Global_Sales.
skewness(turtle$Global_Sales)
kurtosis(turtle$Global_Sales)

# NA_Sales.
skewness(turtle$NA_Sales)
kurtosis(turtle$NA_Sales)

# EU_Sales.
skewness(turtle$EU_Sales)
kurtosis(turtle$EU_Sales)

## Using the log transformation did not improve the normality of the dataset.##


### Create a Simple Linear Regression ###

# Global_Sales vs NA_Sales
lm1 <- lm(Global_Sales ~ NA_Sales, data = turtle)

# View the linear model.
lm1
summary(lm1)

# Plot the residuals.
plot(lm1$residuals)

# Calculate the sum of squares error to determine the strength.
SSE1 = sum(lm1$residuals^2)

# Plot the relationship.
plot(turtle$NA_Sales, turtle$Global_Sales)
# Add a line-of-best-fit.
abline(coefficients(lm1))



# Global_sales vs EU_Sales.
lm2 <- lm(Global_Sales ~ EU_Sales, data = turtle)

# View the linear model.
lm2
summary(lm2)

# Plot the residuals.
plot(lm2$residuals)

# Calculate the sum of squares error to determine the strength.
SSE2 = sum(lm2$residuals^2)

# Plot the relationship.
plot(turtle$EU_Sales, turtle$Global_Sales)
# Add a line-of-best-fit.
abline(coefficients(lm2))



# NA_Sales vs EU_Sales.
lm3 <- lm(NA_Sales ~ EU_Sales, data = turtle)

# View the linear model.
lm3
summary(lm3)

# Plot the residuals.
plot(lm3$residuals)

# Calculate the sum of squared error to determine the strength.
SSE3 = sum(lm3$residuals ^ 2)

# Plot the relationship.
plot(turtle$EU_Sales, turtle$NA_Sales)
# Add a line-of-best-fit.
abline(coefficients(lm3))


### Create a Multiple Linear Regression ###

# Create a model.
mlr <- lm(Global_Sales ~ NA_Sales + EU_Sales, data = turtle)

# See the model.
summary(mlr)


### Predict Global Sales based on the provided values to make comparisons with observed values ###

## Test 1 ##

# Create variables.
NA_Sales <- c(34.02)
EU_Sales <- c(23.80)

# Create a dataframe.
test1 <- data.frame(NA_Sales, EU_Sales)

test1

# Predict with test1.
predict(mlr, newdata = test1, interval = 'confidence')



## Test 2 ##

# Create variables.
NA_Sales <- c(3.93)
EU_Sales <- c(1.56)

# Create a dataframe.
test2 <- data.frame(NA_Sales, EU_Sales)

test2

# Predict with test1.
predict(mlr, newdata = test2, interval = 'confidence')


## Test 3 ##

# Create variables.
NA_Sales <- c(2.73)
EU_Sales <- c(0.65)

# Create a dataframe.
test3 <- data.frame(NA_Sales, EU_Sales)

test3

# Predict with test1.
predict(mlr, newdata = test3, interval = 'confidence')


# Create variables.
NA_Sales <- c(2.26)
EU_Sales <- c(0.97)

# Create a dataframe.
test4 <- data.frame(NA_Sales, EU_Sales)

test4

# Predict with test1.
predict(mlr, newdata = test4, interval = 'confidence')


## Test 5 ##

# Create variables.
NA_Sales <- c(22.08)
EU_Sales <- c(0.52)

# Create a dataframe.
test5 <- data.frame(NA_Sales, EU_Sales)

test5

# Predict with test1.
predict(mlr, newdata = test5, interval = 'confidence')



#########################################################


