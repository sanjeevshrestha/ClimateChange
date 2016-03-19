library(ggplot2) 
library(readr) 
library(zoo)
library(randomForest)


#Kaggle Input:
GlobalLandTemperaturesByCountry <- read.csv("data/GlobalLandTemperaturesByCountry.csv")


source("functions.R")
#subset the data and create some new variables
world_countries<-na.omit(subset(GlobalLandTemperaturesByCountry))
world_countries$dt<-as.Date(world_countries$dt,"%Y-%m-%d")
world_countries$Month<-as.numeric(format(world_countries$dt,"%m"))
world_countries$Month.String<-format(world_countries$dt,"%B")
world_countries$Year<-as.numeric(format(world_countries$dt,"%Y"))
#world_countries$elevation<-with(world_countries,sunPosition(as.numeric(format(dt,"%Y")),as.numeric(format(dt,"%m")),1,12,0,0,40.99,-74.56)$elevation)
#world_countries$azimuth<-with(world_countries,sunPosition(as.numeric(format(dt,"%Y")),as.numeric(format(dt,"%m")),1,12,0,0,40.99,-74.56)$azimuth)

ggplot(world_countries,aes(x=dt,y=AverageTemperature,colour=reorder(Month.String,Month,mean)))+geom_point()+geom_smooth()+ggtitle("Average Temperatures by\nMonth")+xlab("Year")+ylab("Average Temperature")+labs(colour='Month')
ggplot(world_countries,aes(x=dt,y=AverageTemperatureUncertainty))+geom_point()+geom_smooth()+ggtitle("Average Temperature Uncertainty\nOver Time In World")+xlab("Year")+ylab("Average Temperature Uncertainty")
with(subset(world_countries,Year>1750),plot(density(AverageTemperatureUncertainty),main="Density Plot of Average Temperature Uncertainty",xlab="Average Temperature Uncertainty"))


#rf<-randomForest(subset(world_countries,select=c(Year,elevation,azimuth,AverageTemperatureUncertainty)),world_countries$AverageTemperature)
#varImpPlot(rf,main="Variable Importance in Determining\n Average Temperatures")