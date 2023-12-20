library(C50)
library(tree)
library(gmodels)
data<-Company_Data
hist(data$Sales)
HighSales<- ifelse(data$Sales<10, "No", "Yes")
HighSalesData <- data.frame(data, HighSales)
View(HighSalesData)

HSD<-HighSalesData
NewData<-HSD[,2:12]

HSD_train<-NewData[1:200,]
HSD_test<-NewData[201:400,]
Dec_Tree<-C5.0(HighSales ~ CompPrice + Income + Advertising + Population + Price + ShelveLoc
               + Age + Education + Urban + US, data = HSD_train)
summary(Dec_Tree)
plot(Dec_Tree)

pred_tree <- as.data.frame(predict(Dec_Tree,newdata=HSD_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(Dec_Tree,newdata=HSD_test)

mean(pred_test_df==HSD$HighSales)
CrossTable(HSD_test$HighSales,pred_test_df)
library(caret)
confusionMatrix(HSD_test$HighSales,pred_test_df)
