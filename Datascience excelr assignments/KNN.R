wbcd<- read.csv(C:\Users\Dell\Desktop)
wbcd<- KNN
wbcd<-wbcd[,-1]
#table of diagnosis
table(wbcd$daignosis)
#tables or proportions with more informative labels
round(prop.table(table(wbcd$diagnosis))*100, digits = 1)
#create normalization function
normalize<- function(x) {return((x-min(x)/max(x)-min(x)))}
#normalize the wbcd data
wbcd_n<- as.data.frame(lapply(wbcd[2:31], normalize))
wbcd_nl<- cbind(wbcd_n,wbcd$diagnosis)
#create traning and test data 
wbcd_train<- wbcd[1:469, ]
wbcd_test<- wbcd[470:569, ]
#create labels for traning and test data
wbcd_train_labels<- wbcd[1:469, 1]
wbcd_test_labels<- wbcd[470:569, 1]
#traning a model on the data 
library(class)
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,cl = wbcd_train_labels, k=20)

















# use the scale() function to z-score standardize a data frame
wbcd_z <- as.data.frame(scale(wbcd[-1]))

# confirm that the transformation was applied correctly
summary(wbcd_z$area_mean)

# create training and test datasets
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]

# re-classify test cases
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=21)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)

# try several different values of k
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=1)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=5)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=11)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=15)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)
From Srinivas ExcelR to Everyone:  12:58 PM
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=27)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE,prop.c = FALSE, prop.r = FALSE)

