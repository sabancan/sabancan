library(caret)
library(C50)
library(ISLR)
library(dplyr)

require(tree)

carseats <- read.csv("C:/Hymaa/Data Science/Decision Trees/Company_Data.csv")

#Sales_cat <- cut(carseats$Sales,breaks = c(0,2,4,6,8,10,20),labels = c("<2","2-4","4-6","6-8","8-10",">10"),right=FALSE)
#carseats$Sales_Cat<- Sales_cat

names(carseats)
hist(carseats$Sales)

High_Sales <- ifelse(carseats$Sales < 8, "No", "Yes")
carseats <- data.frame(carseats, High_Sales)

carseats <- select(carseats,-c("Sales"))

set.seed(9)

# splittinf data file into 2 part for training and testing
inTrainingLocal <- createDataPartition(carseats$High_Sales,p=.75,list = F) # 70% data is training dataset
training <- carseats[inTrainingLocal,]
testing <- carseats[-inTrainingLocal,]

#Model Building
model <- C5.0(training$High_Sales ~ ., data=training,trials=1) #Trails - Boosting Parameter

#Generate the model summary
summary(model)

#Predict for test dataset
pred <- predict.C5.0(model,select(testing,-c("High_Sales")))
a <- table(testing$High_Sales,pred)

#Accuracy of Model
sum(diag(a))/sum(a)

confusionMatrix(testing$High_Sales,pred)

plot(model)
