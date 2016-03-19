#GlobalTemperature Trend through the years

GlobalTemp<-read.csv("data/GlobalTemperatures.csv")

GlobalTemp$dt<-as.Date(GlobalTemp$dt)
GlobalTemp$year<-year(GlobalTemp$dt)
GlobalTemp$month<-month(GlobalTemp$dt)
GlobalTemp$season<-ifelse(GlobalTemp$month %in% c(6,7,8),"Summer",
                          ifelse(GlobalTemp$month %in% c(9,10,11),"Fall",
                                 ifelse(GlobalTemp$month %in% c(12,1,2),"Winter","Spring")
                          ))

ggplot(data = GlobalTemp, 
       aes(dt,LandAverageTemperature, colour=season)) +
  geom_point(na.rm = T) +
  geom_smooth()+
  xlab("Year") +
  ggtitle("Land Average Temperature\n by Year")

ggplot(data = GlobalTemp, 
       aes(dt,LandAverageTemperatureUncertainty)) +
  geom_point() +
  geom_smooth()+
  xlab("Year") +
  ggtitle("Land Average Temperature Uncertainity\n by Year")

plot(density(na.omit(GlobalTemp$LandAverageTemperatureUncertainty)),main="Density Plot of Average Temperature Uncertainty",xlab="Average Temperature Uncertainty")


