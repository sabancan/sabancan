airquality<-datasets::airquality
head(airquality)
tail(airquality)
summary(airquality)
summary(airquality$Temp)
summary(airquality[,3])
plot(airquality$Ozone)
plot(airquality$Ozone,airquality$wind)
plot(airquality)
plot(airquality$Ozone, type="b")
plot(airquality$Ozone, xlab='ozone concentration', ylab='no of instances',main='ozone levels in NY city',col='blue')
barplot(airquality$Ozone,main = 'Ozone Concentration in air',xlab = 'ozone levels', col='red',horiz=TRUE)

hist(airquality$Solar.R,
     main = 'solar radiation values in air',
     xlab = 'solar rad.',col='blue')
boxplot(airquality[,1:4], main='multilple box plots')
# airquality from 1 to 4 columns
x <-c(0.593,0.142,0.329,0.691,0.231,0.793,0.519,0.392,0.418)
t.test(x,alternative = "greater",mu=0.3)
pain=c(4,5,4,3,2,4,3,4,4,6,8,4,5,4,6,5,8,6,6,7,6,6,7,5,6,5,5)
drug=c(rep("A",9),rep("B",9),rep("C",9))
migraine=data.frame(pain,drug)
results=aov(pain~drug,data=migraine)
summary(results)
plot(pain~drug,data=migraine)

