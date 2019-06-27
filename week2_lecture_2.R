# Brett W. 
# LIS4761 - Data Mining
# Lesson 2: Pictures Versus Numbers
# Followed from video here: https://www.youtube.com/watch?v=_MuDq5-dsQA

# Install packages
install.packages("ggplot2")
library(ggplot2)

# First section - MTCARS data
mtc <- mtcars

# General boxplot of information.
ggplot(mtcars, aes(x=factor(0), mpg)) + geom_boxplot()

# Three boxplots of data by the number of cylinders. 
ggplot(mtcars, aes(group=cyl, x=cyl, mpg)) + geom_boxplot()

# Same boxplot as above, but flipped horizontally. 
ggplot(mtcars, aes(group=cyl, x=cyl, mpg)) + geom_boxplot() + coord_flip()

# Barplot of mtcars data. 
ggplot(mtcars, aes(x=cyl)) + geom_bars()

# Comparison to default histogram. 
hist(mtc$cyl)

# Creates a variable for names of cars. 
car.names <- rownames(mtc)
car.names

mtc[1,]

mtc[,1]

# Barplot for the car's weights. 
g <- ggplot(mtc, aes(x=car.names, y = wt)) + geom_bar(stat="identity")
g

# Barplot with more formatting. 
g <- g + theme(axis.test.x=element_text(angle = 90, hjust=1))
g

# Barplot with a title. 
g + ggtitle("My car weight chart")
g

# Barplot with color to distinguish the gear by cylinder. 
ggplot(mtc, aes(x=cyl, fill=factor(gear))) + geom_bar()

# Unstacked version of the barplot above. 
ggplot(mtc, aes(x=cyl,fill=factor(gear))) + geom_bar(position="dodge")

# Scatterplot of mpg vs. weight. 
ggplot(mtc, aes(x=mpg, y=wt)) + geom_point()

# Scatterplot that takes into account speed. 
ggplot(metc, aes(x=mpg, y=wt)) + geom_point(aes(size=qsec))

# Another take on adding in speed
g <- ggplot(mtc, aes(x=mpg, y=wt)) + geom_point(aes(size=qsec, color=qsec))
g

# Scatterplot with the car names next to the points. 
g + geom_text(aes(label=car.names), size=3)
g

# Scatterplot with names of the cars vs. mpg. 
ggplot(mtc, aes(x=mpg, y=car.names)) + geom_point(size=3)

# Orders the car names by mpg. 
ggplot(mtc, aes(x=mpg, y=reorder(car.names, mpg))) + geom_point(size=3)