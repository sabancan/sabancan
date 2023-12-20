library(caret)
library(C50)
library(ISLR)
library(dplyr)

require(tree)

Fraud_Chk_Data <- read.csv("C:/Hymaa/Data Science/Decision Trees/Fraud_check.csv")

taxable_income <- ifelse(Fraud_Chk_Data$Taxable.Income <= 30000, "Risky", "Good")
Fraud_Chk_Data <- data.frame(Fraud_Chk_Data, taxable_income)

Fraud_Chk_Data <- select(Fraud_Chk_Data,-c("Taxable.Income"))

# splittinf data file into 2 part for training and testing
inTrainingLocal <- createDataPartition(Fraud_Chk_Data$taxable_income,p=.75,list = F) # 70% data is training dataset
training <- Fraud_Chk_Data[inTrainingLocal,]
testing <- Fraud_Chk_Data[-inTrainingLocal,]

#Model Building
model <- C5.0(training$taxable_income ~ ., data=training,trials=1) #Trails - Boosting Parameter

#Generate the model summary
summary(model)

#Predict for test dataset
pred <- predict.C5.0(model,select(testing,-c("taxable_income")))
a <- table(testing$taxable_income,pred)

#Accuracy of Model
sum(diag(a))/sum(a)
confusionMatrix(testing$taxable_income,pred)

plot(model)
