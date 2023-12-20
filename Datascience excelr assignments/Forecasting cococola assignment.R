CocaCola_Sales_Rawdata
myydata<- CocaCola_Sales_Rawdata
Cococola<- myydata$Sales
plot(Cococola)


train<-Cococola[1:30]
test<-Cococola[31:42]
plot(train)
plot(test)
Acf(train)
Pacf(train)
a<-arima(train,order=c(0,1,3),method="ML")

plot(forecast(a,h=5),xaxt="n")

library(forecast)
model_AA <- auto.arima(train)
model_AA
pred_AA <- data.frame(forecast(model_AA))
acf(model_AA$residuals)
pacf(model_AA$residuals)
windows()
plot(forecast(model_AA,h=12),xaxt="n")
