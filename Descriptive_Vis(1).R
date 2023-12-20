data("airquality")

#airquality = read.csv('path/airquality.csv',header=TRUE, sep=",")

airquality <-datasets::airquality

head(airquality)
tail(airquality)
####################
summary(airquality$Temp)

plot(airquality$Ozone)

plot(airquality$Ozone, airquality$Wind)

plot(airquality)

# points and lines 
plot(airquality$Ozone, type= "b") # p: points, l: lines,b: both ,h

plot(airquality$Ozone, xlab = 'ozone Concentration', 
     ylab = 'No of Instances', main = 'Ozone levels in NY city',
     col = 'blue')


# Horizontal bar plot
barplot(airquality$Ozone, main = 'Ozone Concenteration in air',
        xlab = 'ozone levels', col= 'red',horiz = TRUE)




hist(airquality$Solar.R)




hist(airquality$Solar.R, 
     main = 'Solar Radiation values in air',
     xlab = 'Solar rad.', col='blue')





#Single box plot
boxplot(airquality$Solar.R)








# Multiple box plots
boxplot(airquality[,1:4], 
        main='Multiple')

#margin of the grid(mar), 
#no of rows and columns(mfrow), 
#whether a border is to be included(bty) 
#and position of the 
#labels(las: 1 for horizontal, las: 0 for vertical)

par(mfrow=c(3,3),mar=c(2,5,2,1),  las=0, bty="n")
plot(airquality$Ozone)
plot(airquality$Ozone, airquality$Wind)
plot(airquality$Ozone, type= "c")
plot(airquality$Ozone, type= "s")
plot(airquality$Ozone, type= "h")
barplot(airquality$Ozone, main = 'Ozone Concenteration in air',
        xlab = 'ozone levels', col='green',horiz = TRUE)
hist(airquality$Solar.R)
boxplot(airquality$Solar.R)
boxplot(airquality[,0:4], main='Multiple Box plots')

