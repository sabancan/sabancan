Train.sal<-read.csv("C:/Users/Acer/Downloads/Datasets/SalaryData_Train(1).csv")
Test.Sal<-read.csv("C:/Users/Acer/Downloads/Datasets/SalaryData_Test(1).csv")
library(kernlab)
plot(Train.sal)
Sal_class<-ksvm(Salary~.,data=Train.sal,kernel="rbfdot")
Sal_prediction<-predict(Sal_class, Test.Sal)
head(Sal_prediction)
agr<-Sal_prediction==Test.Sal$Salary
prop.table(table(agr))

round(prop.table(table(Train.sal$Salary))*100,digits = 1)
library(DMwR)
Smoted<-SMOTE(Salary~.,data=Train.sal,perc.under = 100)
round(prop.table(table(Smoted$Salary))*100,digits = 1)
#library(caret)
#cor(Train.sal)