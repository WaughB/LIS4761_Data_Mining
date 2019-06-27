# Brett W. 
# LIS4761 - Data Mining
# Lesson 2: Pictures Versus Numbers
# Followed from video here: https://www.youtube.com/watch?v=7XKnbMA1kkA

# Install packages
install.packages("ggplot2")
library(ggplot2)

# First section - MTCARS data
mtc <- mtcars
hist(mtc$mpg, breaks = 4)

# Basic histogram. 
ggplot(mtc, aes(x=mpg)) + geom_histogram(bins=5)

# Slightly better histogram. 
ggplot(mtc, aes(x=mpg)) + geom_histogram(bins=5, color="black", fill="white")
g <- ggplot(mtc, aes(x=mpg)) + geom_histogram(bins=5, color="black", fill="white")
g

# Basic histogram with a title. 
g + ggtitle("MPG Buckets")

# Basic histogram with larger binwidth. 
g <- ggplot(mtc, aes(x=mpg)) + geom_histogram(binwidth = 10, color="black", fill="white")
g

# Basic histogram with a smaller binwidth. 
g <- ggplot(mtc, aes(x=mpg)) + geom_histogram(binwidth = 2, color="black", fill="white")
g

# Basic histogram with more bins. 
g <- ggplot(mtc, aes(x=mpg)) + geom_histogram(bins = 10, color="black", fill="white")
g

# Second section - Travel time data
timeToNYC <- c(4, 4.5, 3.5, 5, 4, 4.2)
timeToNYCWeek2 <- c(4.5, 5, 3.8, 5.2, 4.6, 4.3)

day <- c("mon", "tues", "wed", "thurs", "fri", "sat")
week1 <- c(1, 1, 1, 1, 1, 1)
week2 <- c(2, 2, 2, 2, 2, 2)

time <- c(timeToNYC, timeToNYCWeek2)

week <- as.factor(c(week1, week2))
dayOfWeek <- c(day, day)
df <- data.frame(day, timeToNYC, timeToNYCWeek2)
df

# Line graph of first week. 
ggplot(df, aes(x=day, y=timeToNYC, group=1)) + geom_line()

# Dotted line graoh of first week. 
g <- ggplot(df, aes(x=day, y=timeToNYC, group=1)) + geom_line(color="red", linetype="dashed", size=1.5)
g

# More takes on the line graph. 
g + geom_point()
g + geom_point(color="blue", size=4)
g + geom_point(color="blue", size=4) + ylab("Time to NYC (in hours)")
g

# Dotplot of first week data. 
ggplot(df, aes(x=day, y=timeToNYC, group=1)) + geom_point(color="blue", size=4)

# Third section -  Comparing data
df <- data.frame(dayOfWeek, time, week)
df

# Double line graph for travel time between the first and second week. 
g <- ggplot(df, aes(x=dayOfWeek, group=week, color=week)) + geom_line(aes(y=time))
g

# Double line graph for travel times between first and second week. 
g <- g + ylab("Time to NYC (in hours)") + ggtitle("Comparing Weekly Times")
g

