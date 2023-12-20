pnorm(70,60,10)
1-pnorm(50,60,10)
pnorm(680,711,29)
pnorm(697,711,29)
pnorm(740,711,29)

1990+(211.29*1.645)
1990-(211.29*1.645)

reg_model<-lm(sunday~daily,data=NewspaperData[,-1])
summary(reg_model)
daily=250
sunday = 13.83+(1.33*daily)
sunday
sunday=predict(reg_model,newdata=data.frame(daily=250))
sunday
model<-lm(sunday~daily,Newspaper data)
summary (model)


