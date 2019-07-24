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


# Load in the data. 
attach(airquality)

# Figure out averages of Ozone and Solar.R. 
solar_mean <- airquality %>%
  summarize(ozone_mean = mean(Ozone, na.rm = TRUE))

airquality %>%
  summarize(solar_mean = mean(Solar.R, na.rm = TRUE))

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
ksvmMod <- ksvm(df$Ozone ~ ., df$training)

ksvmMod

ksvmPred <- predict(ksvmMod, testing_set)

ksvmResults <- table(ksvmPred, testing_set$Ozone)

# Calculate RMSE for KSVM. 
ksvmRMSE <- RMSE(testing_set$Ozone, ksvmPred)

# Graph for KSVM. 


# Create model using SVM. 
svmMod <- svm(Ozone ~ ., data = df, kernal="radial", cost=25, na.action =na.omit, scale = TRUE, cross=10)
print(svmMod)

# Results of training set. 
pred_train_svm <- predict(svmMod, training_set)

# Results of test set.
pred_test_svm <- predict(svmMod, testing_set)
resultFinal <- mean(pred_test_svm==testing_set$Ozone)

# Calculate the RMSE for the SVM. 
svmRMSE <- RMSE(testing_set$Ozone, pred_test_svm)

# Graph for SVM. 



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
lmMod$results$RMSE

# Graph for LM. 


# Create goodOzone variable. Zero if lower than average, 
# one if higher than usual.



