# Brett Waugh
# 21 July 2019
# LIS4761 - Data Mining
# Lesson 7 - Using aRules on the Titanic dataset

# Necessary libraries.
require(dplyr)
require(tidyr)
require(tidyverse)
require(arules)
require(arulesViz)

# Get the data in. 
load("/home/brett/LIS4761_Data_Mining/data/titanic.raw.rdata")

# Convert data to dataframe. 
t <- titanic.raw

##### First set of questions #####

# Total number of people.
totalNum <- t %>%
  count()

# Percentage of people that survived.
numSurvived <- t %>%
  filter(Survived == "Yes") %>%
  count()

# Calculation. 
percSurvived <- 100 * (numSurvived / totalNum)
percSurvived

# Percentage of people that were children. 
numChild <- t %>%
  filter(Age == "Child") %>%
  count()

# Calculation. 
percChildren <- 100 * (numChild / totalNum)
percChildren

# Percentage of people that were female.
numFemale <- t %>%
  filter(Sex == "Female") %>%
  count()

# Calculation.
percFemale <- 100 * (numFemale / totalNum)
percFemale

# Percentage of people that were first class. 
num1Class <- t %>%
  filter(Class == "1st") %>%
  count()

# Calculation.
perc1Class <- 100 * (num1Class / totalNum)
perc1Class

##### Second set of questions #####

# What percentage of children survived?
numChildSurvived <- t %>%
  filter(Survived == "Yes" & Age == "Child") %>%
  count()

# Calculation. 
percChildSurvived <- 100 * (numChildSurvived / numChild)
percChildSurvived

# What percentage of females survived?
numFemaleSurvived <- t %>%
  filter(Survived == "Yes" & Sex == "Female") %>%
  count()

# Calculation. 
percFemaleSurvived <- 100 * (numFemaleSurvived / numFemale)
percFemaleSurvived

# What percentage of first-class people survived?
num1ClassSurvived <- t %>%
  filter(Survived == "Yes" & Class == "1st") %>%
  count()

# Calculation. 
perc1ClassSurvived <- 100 * (num1ClassSurvived / num1Class)
perc1ClassSurvived

# What percentage of third class people survived?
num3ClassSurvived <- t %>%
  filter(Survived == "Yes" & Class == "3rd") %>%
  count()

# Number of people in third class.
num3Class<- t %>%
  filter(Class == "3rd") %>%
  count()

# Calculation. 
perc3ClassSurvived <- 100 * (num3ClassSurvived / num3Class)
perc3ClassSurvived

##### Third set of questions #####

# A function that returns a new dataframe of people that satisfies the specified criteria of sex, age, class and survived.
myFunction1 <- function(class,sex,age,survived){
  df1 <- t[t$Class == class,] # filter the data that satisfied the criteria that "Class" = a
  df2 <- df1[df1$Sex == sex,] # filter the data that satisfied the criteria that "Sex" = b
  df3 <- df2[df2$Age == age,] # filter the data that satisfied the criteria that "Age" = c
  df4 <- df3[df3$Survived == survived,] # filter the data that satisfied the criteria that "Survived" = d
  return(df4)}

# Test the function with a sample data.
myFunction1("1st","Female","Adult","No")

# Write a function that calculates the survival percentage for parameters age, class and sex.
myFunction2 <- function(age,sex,class){
  # Age.
  alpha <- t %>%
    filter(Survived == "Yes" & Age == age) %>%
    count() 
  
  alpha1 <- t %>%
    filter(Age == age) %>%
    count()
  
  alphaResult <- 100 * (alpha / alpha1)
  
  # Sex. 
  beta <- t %>%
    filter(Survived == "Yes" & Sex == sex) %>%
    count()
  
  beta1 <- t %>%
    filter(Sex == sex) %>%
    count()
  
  betaResult <- 100 * (beta / beta1)
  
  # Class. 
  charlie <- t %>%
    filter(Survived == "Yes" & Class == class) %>%
    count()
  
  charlie1 <- t %>%
    filter(Class == class) %>%
    count()
  
  charlieResult <- 100 * (charlie / charlie1)
  
  # Cumulative survival rate. 
  deltaResult <- (alphaResult * betaResult * charlieResult) / 1000000
  
  # Create the results dataframe. 
  dfResult <- data.frame(alphaResult, betaResult, charlieResult, deltaResult)
  names(dfResult) <- c("Age", "Sex", "Class", "Cumulative Survival Rate")
  
  return(dfResult)}

# Test the function with sample data.
myFunction2("Adult", "Male", "3rd")

# Use the function to compare age and third-class male survival rates. People in which category are more likely to survive? 

# Male child survival rates by class.
myFunction2("Child", "Male", "1st")
myFunction2("Child", "Male", "2nd")
myFunction2("Child", "Male", "3rd")

# Male adult survival rates by class.
myFunction2("Adult", "Male", "1st")
myFunction2("Adult", "Male", "2nd")
myFunction2("Adult", "Male", "3rd")

# Based on the results, a male child in first class is the most likely to survive. 
# The male child in first class had a cumulative survival rate of 0.07, with the 
# least likely to survive being adult male in third class (0.02). 

# Use the function to compare age and first-class female survival rates. People in which category are more likely to survive? 

# Female child survival rates by class.
myFunction2("Child", "Female", "1st")
myFunction2("Child", "Female", "2nd")
myFunction2("Child", "Female", "3rd")

# Female adult survival rates by class.
myFunction2("Adult", "Female", "1st")
myFunction2("Adult", "Female", "2nd")
myFunction2("Adult", "Female", "3rd")

# Based on the results, a female child in first class is the most likely to survive.
# The female child in first class had a cumulative survival rate of 0.24, with the 
# least likely to survive being adult female in third class (0.06).

##### Fourth set of questions #####

# Use aRules to calculate some rules (clusters) for the titanic dataset.
rules <-  apriori(t, parameter = list(support = 0.005, confidence = 0.5))
inspect(rules)

daRules <- apriori(t, parameter = list(support = 0.01, confidence = 0.5))
inspect(daRules)

# Visualize the results.
par(mfrow = c(1,1))
plot(daRules)

# Pick the three most interesting and useful rules.
good <- daRules[quality(daRules)$lift > 3]
top3 <- head(good, 3)
plot(top3)
inspect(top3)

# The top three rules are:
# 1. Class=2nd, Age=Child    => Survived=Yes
# This reads that second class children likely survived.

# 2. Age=Child, Survived=No  => Class=3rd
# This reads that children that didn't survive were likely in third class.

# 3. Class=2nd, Survived=Yes => Sex=Female
# This reads that survivers in second class are likely female.

# How does this compare to the descriptive analysis we did on the same dataset?
# These results are consistent with some of the descriptitive analysis used. 
# Females and children were more likely to survive while adult, male, and
# lower class hurt chances at survival.



