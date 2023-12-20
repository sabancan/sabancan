mydata<- read.csv("/Volumes/Data/Course Content/DS Content/Clustering/EastWestAirlines")
mydata<- EastWestAirlines
mydata<- scale(EastWestAirlines[,2:12])
df <- dist(mydata , method = "euclidean")
fit <- hclust(df , method = "average")
plot(fit)
groups<- cutree(fit , k=4)
rect.hclust(fit , k=10 , border = "blue")
clusters = data.frame(EastWestAirlines[, 1] , 'clusters' = groups)
library(plyr)

data<- mydata
plot(data)
km<- kmeans(data,10)
km$centers
km$cluster
library(animation)
windows()
km<- kmeans.ani(data,10)
wss<-c()
for(i in 2:15) wss[i]<-sum(kmeans(data,centers = i)$withinss)
plot(1:15,wss,type = "b",xlab = "No of clusters",ylab = "Avg distance")
