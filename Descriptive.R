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
