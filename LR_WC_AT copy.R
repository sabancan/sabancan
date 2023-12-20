

library("lattice")
attach(WC_AT1)
dotplot(AT,main="AT dotplot predictions",col="green")
dotplot(Waist,main="AT dotplot predictions",col="green")
boxplot(AT,col="green")
boxplot(Waist,col="green")

reg<-lm(AT~Waist)
summary(reg)
predict(reg,data.frame(Waist=c(76.85,82)))
pred_E<-predict(reg)
Error<-data.frame(WC_AT1,"Pred"= pred_E,"Error"=WC_AT1$AT-pred_E)

