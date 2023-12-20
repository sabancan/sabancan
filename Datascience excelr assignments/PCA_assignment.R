library(gdata)
library(xlsx)

data<-wine
pca<-princomp(wine[,2:14],cor=TRUE,scores = TRUE,covmat = NULL)
summary(pca)
pca$scores
pca$loadings  
plot(pca$scores[,1:3],col="Red",pch=19,cex=0.3,lwd=3)
text(pca$scores[,1:3],labels=c(1:14),cex=1)
mydata<-pca$scores[,1:3]

d <- dist(mydata , method = "euclidean")
fit <- hclust(d , method = "complete")
plot(fit)
groups <- cutree(fit , k=4)
rect.hclust(fit , k=4 , border = "red")
clusters=data.frame(mydata, 'cluster'=groups)

library(plyr)
plot(mydata)
km<- kmeans(mydata,3)
km$centers
km$cluster
library(animation)
windows()
km<- kmeans.ani(mydata,3)
wss<-c()
for(i in 2:15) wss[i]<-sum(kmeans(mydata,centers = i)$withinss)
plot(1:15,wss,type = "b",xlab = "No of clusters",ylab = "Avg distance")

