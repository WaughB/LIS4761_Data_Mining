# Brett W. 
# LIS4761 - Data Mining
# Lesson 8: Support Vector Machines
# SVM Homework -- Using SVM on an Air Quality Dataset

# Necessary libraries. 
require(dplyr)
require(kernlab)
require(ggplot2)
require(caret)
require(e1071)
require(gridExtra)

# Figure out averages of Ozone and Solar.R. 
ozone_mean <- airquality %>%
  summarize(ozone_mean = mean(airquality$Ozone, na.rm = TRUE))

solar_mean <- airquality %>%
  summarize(solar_mean = mean(airquality$Solar.R, na.rm = TRUE))

# Create dataframe. 
df <- airquality

# Replace NA's in Ozone and Solar.R with the average values. 
ozone_mean <- as.integer(ozone_mean)
df$Ozone[is.na(df$Ozone)] <- ozone_mean

solar_mean <- as.integer(solar_mean)
df$Solar.R[is.na(df$Solar.R)] <- solar_mean

# Seperate data into training and testing set. 
sz <- round(.8 * dim(df)[1]) 
training_set <- df[1:sz,]
testing_set <- df[-(1:sz),]

# Create model using KSVM. 
ksvmMod <- ksvm(training_set$Ozone ~ ., training_set)

ksvmMod

ksvmPred <- predict(ksvmMod, testing_set)

ksvmResults <- table(ksvmPred, testing_set$Ozone)

# Calculate RMSE for KSVM. 
ksvmRMSE <- RMSE(testing_set$Ozone, ksvmPred)

# Graph for KSVM.
ksvmGraph <- ggplot(df) + 
  geom_point(aes(x=Temp, y=Wind, color=ksvmRMSE, size = ksvmRMSE)) +
  ggtitle("KSVM Performance")

# Create model using SVM. 
svmMod <- svm(Ozone ~ ., data = df, kernal="radial", cost=25, na.action =na.omit, scale = TRUE, cross=10)
print(svmMod)

# Results of training set. 
pred_train_svm <- predict(svmMod, training_set)

# Results of test set.
pred_test_svm <- predict(svmMod, testing_set)

# Calculate the RMSE for the SVM. 
svmRMSE <- RMSE(testing_set$Ozone, pred_test_svm)

# Graph for SVM.
svmGraph <- ggplot(df) + 
  geom_point(aes(x=Temp, y=Wind, color=svmRMSE, size = svmRMSE)) +
  ggtitle("SVM Performance")

# Create model using LM. 
ControlParameters <-trainControl(method="repeatedcv", 
                                 number=10,
                                 repeats=10)

lmMod <-train(Ozone ~ ., 
                   data=training_set,
                   method="glm",
                   trControl= ControlParameters
)

lmMod

# Calculate the RMSE for the LM. 
lmRMSE <- lmMod$results$RMSE

# Graph for LM.
lmGraph <- ggplot(df) + 
  geom_point(aes(x=Temp, y=Wind, color=lmRMSE, size = lmRMSE)) +
  ggtitle("LM Performance")

# Combine three previous graphs for comparison.
grid.arrange(ksvmGraph, svmGraph, lmGraph, nrow = 3)

# Create goodOzone variable. Zero if lower than average, 
# one if higher than usual.
df$goodOzone <- NA

df$goodOzone[df$Ozone >= ozone_mean] <- 1
df$goodOzone[df$Ozone < ozone_mean] <- 0

## Create KSVM based on goodOzone.
# Seperate data into training and testing set. 
sz <- round(.8 * dim(df)[1]) 
training_set <- df[1:sz,]
testing_set <- df[-(1:sz),]

ksvmMod2 <- ksvm(training_set$goodOzone ~ ., training_set)

ksvmMod2

ksvmPred2 <- predict(ksvmMod2, testing_set)

ksvmResults2 <- table(ksvmPred2, testing_set$Ozone)

# Calculate RMSE for KSVM. 
ksvmRMSE2 <- RMSE(testing_set$goodOzone, ksvmPred2)

# Graph for KSVM.
ksvmGraph2 <- ggplot(df) + 
  geom_point(aes(x=Temp, y=Wind, color=ksvmRMSE2, size = ksvmRMSE2)) +
  ggtitle("Second KSVM Performance")

## Create SVM based on gooOzone. *** TODO ***
svmMod2 <- svm(goodOzone ~ ., data = df, kernal="radial", cost=25, na.action =na.omit, scale = TRUE, cross=10)
print(svmMod2)

# Results of training set. 
pred_train_svm2 <- predict(svmMod2, training_set)

# Results of test set.
pred_test_svm2 <- predict(svmMod2, testing_set)

# Calculate the RMSE for the SVM. 
svmRMSE2 <- RMSE(testing_set$Ozone, pred_test_svm2)

# Graph for SVM.
svmGraph2 <- ggplot(df) + 
  geom_point(aes(x=Temp, y=Wind, color=svmRMSE, size = svmRMSE)) +
  ggtitle("SVM Performance")

## Create Naive-Bayes based on goodOzone.
nb <- naiveBayes(goodOzone ~ ., data = training_set)

test_nb <- predict(nb, testing_set)

# nbResults <- table(test_nb, testing_set$goodOzone)

nbRMSE <- RMSE(testing_set$goodOzone, test_nb)

# Calculate the RMSE for the Naive-Bayes. 
nbGraph <- ggplot(df) + 
  geom_point(aes(x=Temp, y=Wind, color=nbRMSE, size = nbRMSE)) +
  ggtitle("Naive-Bayes Performance")

# Combine three previous graphs for comparison. 
grid.arrange(ksvmGraph2, svmGraph2, nbGraph, nrow = 3)

# Best model?
# The KSVM had the lowest RMSE of the models. It is
# worth noting that I did cross-validiate the other models, 
# but I did not cross-validiate the KSVM. 
# The linear model did not perform well relative to the
# KSVM and SVM. Because I did not eliminate more variables, 
# the linear model had more difficulties with the greater 
# number of features. 