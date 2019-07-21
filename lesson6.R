# Brett Waugh
# 16 July 2019
# LIS4761 - Data Mining
# Lesson 6 - Linear Modeling

# Necessary libraries.
require(readxl)
require(ggplot2)

# Load in data. 
# Data is taken from: http://college.cengage.com/mathematics/brase/understandable_statistics/7e/students/datasets/mlr/excel/mlr01.xls
data <- read_excel("/home/brett/LIS4761_Data_Mining/data/mlr01.xls")

# Turn the data into a dataframe.
df <- as.data.frame(data)
names(df) <- c("fawn_count", "adult_size", "annual_precipitation","winter_severity_index")

# Ensure that there are eight observations and four columns.
str(df)

# Attach df.
attach(df)

# Fawn versus adult graph.
ggplot(df, aes(x = adult_size, y = fawn_count)) + 
  geom_line() +
  labs(
    x = "Adult",
    y = "Fawn",
    title = "Adult vs. Fawn Count",
    caption = "Data is taken from: http://college.cengage.com/mathematics/brase/understandable_statistics/7e/students/datasets/mlr/excel/mlr01.xls"
  )

# Fawn versus precipitation graph.
ggplot(df, aes(x = fawn_count, y = annual_precipitation)) + 
  geom_line() +
  labs(
    x = "Precipitation",
    y = "Fawn",
    title = "Precipitation vs. Fawn Count",
    caption = "Data is taken from: http://college.cengage.com/mathematics/brase/understandable_statistics/7e/students/datasets/mlr/excel/mlr01.xls"
  )

# Fawm versus winter severity graph.
ggplot(df, aes(x = winter_severity_index, y = fawn_count)) + 
  geom_line() + 
  labs(
    x = "Winter Severity (Index)",
    y = "Fawn",
    title = "Winter Severity vs. Fawn Count",
    caption = "Data is taken from: http://college.cengage.com/mathematics/brase/understandable_statistics/7e/students/datasets/mlr/excel/mlr01.xls"
  )

# Scatterplot of fawn vversus adult, with color as precipitation and
# the winter severity as the size. 
ggplot(df, aes(x = adult_size, y = fawn_count, color = annual_precipitation, size = winter_severity_index)) + 
  geom_count() + 
  labs(
    x = "Adult",
    y = "Fawn",
    title = "Adult vs. Fawn Count",
    subtitle  = "Including Precipitation & Winter Severity",
    caption = "Data is taken from: http://college.cengage.com/mathematics/brase/understandable_statistics/7e/students/datasets/mlr/excel/mlr01.xls"
  )

# First linear model: fawns and winter. 
m1 <- lm(fawn_count ~ winter_severity_index)

# Second linear model: fawns, winters and precipitation.
m2 <- lm(fawn_count ~ winter_severity_index + annual_precipitation)

# Third linear model: fawns, winters, precipitation, and adult. 
m3 <- lm(fawn_count ~ winter_severity_index + annual_precipitation + adult_size)

##### Which model works best? ######
# The third model worked the best. It produced an Adjusted R Squared value of 0.955.
# The p-value for the model was 0.001 which is less than 0.05. 

###### Which of the predictors are statistically significant in each model? ######
# Annual precipitation was the most statistically significant predictor. 
# Precipitation was 0.0366, while adult wa 0.027, and winter being
# 0.022. 

###### If you wanted to create the most parsimonious model               #####
###### (i.e., the one that did the best job with the fewest predictors), #####
###### what would it contain?                                            #####
# It would contain the precipitation variable and 
# possibly the the adult variable. 
