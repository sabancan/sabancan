`Airlines+Data`
data<- `Airlines+Data`
Airlines<- data$Passengers
View(Airlines)
plot(Airlines)


train<-Airlines[1:60]
test<-Airlines[61:96]
plot(train)
plot(test)
Acf(train)
Pacf(train)
a<-arima(train,order=c(0,1,13),method="ML")
library(forecast)
model_AA <- auto.arima(train)
model_AA
pred_AA <- data.frame(forecast(model_AA))
acf(model_AA$residuals)
pacf(model_AA$residuals)
windows()
plot(forecast(model_AA,h=10),xaxt="n")
 