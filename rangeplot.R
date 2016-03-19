library(ggplot2)
library(gplots)

globalTemp<-read.csv("data/GlobalTemperatures.csv")


#Convert from string to date time
globalTemp$dt <- as.Date(globalTemp$dt, format = "%Y-%m-%d")
gt1850_2015 <- na.omit(subset(globalTemp,as.numeric(format(globalTemp$dt, "%Y"))>= 1850))


#yearly temp
years <- as.numeric(format(gt1850_2015$dt, "%Y"))
yearly_temp<-gt1850_2015
yearly_temp$dt<-years
yearly_temp$dt<-years
names(yearly_temp)[names(yearly_temp)=="dt"] <- "year"
avg_yearly_temp <- aggregate(yearly_temp[, 1:8], list(yearly_temp$year), mean)

#sample datasets
head(globalTemp, 5)
head(gt1850_2015,5)
head(yearly_temp,5)

#plot avg, min, and max of world temperature averaged monthly
x<-years
y_min <- gt1850_2015$LandMinTemperature
y_avg <- gt1850_2015$LandAverageTemperature
y_max <- gt1850_2015$LandMaxTemperature


plot(x, y_max, col="blue",pch=17, ylim=c(-5,25),ylab="Temperature", xlab="year", main="Global Monthly Temperature")
par(new=TRUE)
plot(x, y_min, col="red",pch=15,ylim=c(-5,25),ylab="Temperature", xlab="year")
par(new=TRUE)
plot(x, y_avg, col="green", pch=16, ylim=c(-5,25),ylab="Temperature", xlab="year")
legend("topleft", legend=c("Minimum", "Average", "Maximum"), pch=c(15, 16, 17),col=c("red", "green", "blue"), cex=0.7,box.lty=1, box.lwd=1)


#plotn Yearly averages of avg, max and min
x2<-avg_yearly_temp$Group.1
yrly_avg_max <- avg_yearly_temp$LandMaxTemperature
yrly_avg_min <- avg_yearly_temp$LandMinTemperature
yrly_avg_avg <- avg_yearly_temp$LandAverageTemperature

plot(x2, yrly_avg_max, col="blue",pch= 17, xlab="Year", ylab="Temperature", ylim=c(0,17))
par(new=TRUE)
plot(x2, yrly_avg_avg, col="green", pch= 16,main="Global Monthly Average Temperature", xlab="Year", ylab="Temperature", ylim=c(0,17)) 
par(new=TRUE)
plot(x2, yrly_avg_min, col="red", pch= 15, xlab="Year", ylab="Temperature", ylim=c(0,17))
legend("topleft", legend=c("Minimum", "Average", "Maximum"), col=c("red", "green", "blue"), pch=c(15,16,17), cex=0.7,box.lty=1, box.lwd=1)
