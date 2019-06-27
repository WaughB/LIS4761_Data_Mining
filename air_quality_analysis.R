# Brett W. 
# LIS4761 - Data Mining
# Lesson 2: Pictures Versus Numbers
# Visualization Homework: Air Quality Analysis

# Step 1: Load the data. 
attach(airquality)
require(ggplot2)
require(reshape2)

# Step 2: Clean the data. 
# The ggplot2 library I use will automatically takes care of the NA's.
# If I were using other methods, I would have to address the NA's.
# In Step 4, NA's are dealt with to create the heatmap. 

# Step 3: Understand the data distribution. 

# Histogram for ozone.
ggplot(airquality, aes(Ozone)) + geom_histogram() + ggtitle("Histogram of Ozone (New York, 1973)")

# Histogram for solar radiation. 
ggplot(airquality, aes(Solar.R)) + geom_histogram() + ggtitle("Histogram of Solar Radiation (New York, 1973)")

# Histogram for wind speeds.
ggplot(airquality, aes(Wind)) + geom_histogram() + ggtitle("Histogram of Wind Speeds (New York, 1973)")

# Histogram for temperature. 
ggplot(airquality, aes(Temp)) + geom_histogram() + ggtitle("Histogram of Temperatures (New York, 1973)")

# Histogram for month.
ggplot(airquality, aes(Month)) + geom_histogram() + ggtitle("Histogram of Months (New York, 1973)")

# Histogram for day. 
ggplot(airquality, aes(Day)) + geom_histogram() + ggtitle("Histogram of Days (New York, 1973)")

# Boxplot for ozone.
ggplot(airquality, aes(Month, Ozone, group = Month)) + geom_boxplot() + ggtitle("Histogram of Ozone by Month (New York, 1973)")

# Boxplot for wind speeds.
ggplot(airquality, aes(Month, Wind, group = Month)) + geom_boxplot() + ggtitle("Histogram of Wind Speeds by Month (New York, 1973)")

# Step 4: Look at all the data via a heatmap. 
df <- data.frame(airquality[, -5])
df <- na.omit(df)

cormat <- round(cor(df), 2)

melted_cormat <- melt(cormat)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + geom_tile() + ggtitle("Heatmap of New York Air Quality (1973)")

# Step 5: Look at all the data via a scatter chart. 
ggplot(airquality, aes(x=Wind,y= Temp, dotsize = Ozone, color = Solar.R)) + geom_point() + ggtitle("Scatterplot of New York Air Quality (1973)")

# Step 6: Final analysis.
### Do you see any patterns after exploring the data ###
# One of the patterns I thought was intersting was the histogram of wind speeds by month. 
# This visualization clearly shows that some months in New York had significantly faster
# winds than other months. Similar results for the histogram on ozone.
# Another interesting graph was the heatmap. People are easily able to distinguish 
# patterns using heatmaps. In this case, the relationships like temperature and ozone
# and solar radiation and temperature are more easily discovered. 

### What was the most useful visualization ###
# The histograms in my opinion were the most useful visualizations. 
# While the final scatterplot may have the most variables, too many 
# variables can be confusing. Histograms convey more analytic data
# in this situation. 

