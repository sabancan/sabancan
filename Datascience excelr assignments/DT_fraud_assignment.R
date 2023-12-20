library(C50)
library(tree)
library(gmodels)
thisdata<- Fraud_check
hist(thisdata$Taxable.Income)
fraud_good<- ifelse(thisdata$Taxable.Income<=30000, "fraud", "good")
fraud_good_data<- data.frame(thisdata , fraud_good)
View(fraud_good_data)
  

FD<-fraud_good_data
MYdata<-FD[,-3]

FD_train<-MYdata[1:420,]
FD_test<-MYdata[421:600,]
Dec_Tree<-C5.0(fraud_good ~ Undergrad + Marital.Status + City.Population + Work.Experience, data = FD_train)
summary(Dec_Tree)
plot(Dec_Tree)

pred_tree <- as.data.frame(predict(Dec_Tree,newdata=FD_test)
pred_tree["final"]<- NULL
pred_test_df <- predict(Dec_Tree,newdata=FD_test)

mean(pred_test_df==FD$fraud_good)
CrossTable(FD_test$fraud_good,pred_test_df)
library(caret)
confusionMatrix(FD_test$fraud_good,pred_test_df)
                