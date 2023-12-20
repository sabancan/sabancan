crime_data
mydata<-crime_data
mydata<- scale(crime_data[,2:5])
d <- dist(mydata , method = "euclidean")
fit <- hclust(d , method = "average")
plot(fit)
groups <- cutree(fit , k=6)
rect.hclust(fit , k=6 , border = "purple")
clusters=data.frame(crime_data[,1] , 'cluster'=groups)


library(plyr)
data<- crime_data
plot(data)
km<- kmeans(data,5)
km$centers
km$cluster
library(animation)
windows()
km<- kmeans.ani(data,5)
wss<-c()
for(i in 2:15) wss[i]<-sum(kmeans(mydata,centers = i)$withinss)
plot(1:15,wss,type = "b",xlab = "No of clusters",ylab = "Avg distance")

