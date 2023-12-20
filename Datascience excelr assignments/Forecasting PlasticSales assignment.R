data<- `PlasticSales`
Sales<- data$Sales
View(Sales)
plot(Sales)


train<-Sales[1:45]
test<-Sales[46:60]
plot(train)
plot(test)
Acf(train)
Pacf(train)
a<-arima(train,order=c(5,1,0),method="ML")

plot(forecast(a,h=12),xaxt="n")

library(forecast)
model_AA <- auto.arima(train)
model_AA
pred_AA <- data.frame(forecast(model_AA))
acf(model_AA$residuals)
pacf(model_AA$residuals)
windows()
plot(forecast(model_AA,h=12),xaxt="n")

