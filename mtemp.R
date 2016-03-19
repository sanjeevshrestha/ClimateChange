# Load the necessary packages
library(ggplot2) # Data visualization
library(ggthemes) # themes to assist ggplot
library(data.table) # for fast reading and data manipulation

countries <- fread("data/GlobalLandTemperaturesByCountry.csv")
world_countries <- na.omit(subset(countries))

world_countries$dt <- as.Date(world_countries$dt, format = "%Y-%m-%d")
world_countries$month <- format(world_countries$dt, "%m")
world_countries$year <- format(world_countries$dt, "%Y")


monthly_temp <- world_countries[, .(Ave_monthly = mean(AverageTemperature, na.rm = TRUE)), by = .(Country, month)]

yearly_temp <- world_countries[, .(Ave_yearly = mean(AverageTemperature, na.rm = TRUE)), by = .(Country, year)]


selected_countries <- monthly_temp[Country %in%  c("Afghanistan", "Albania", "Antarctica", "Australia", "Austria", "Bangladesh", "Belize", "Bhutan", "Barbados", "Belgium",
"Brazil", "Burundi", "Cambodia","Chad", "China", "Congo", "Cuba", "Denmark", "Egypt", "Ecuador", "Estonia", "Ethiopia", "France", "Finland",
"Georgia", "Germany", "Ghana", "Greece", "Guatemala", "Hong Kong", "Hungary", "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland", "Israel",
"Italy", "Jamaica", "Japan", "Jordan", "Kuwait", "Macau", "Malaysia", "Mongolia", "Morocco", "Namibia", "Nepal", "Netherlands", "New Zealand",
"North Korea", "Philippines", "Peru", "Qatar", "Russia", "Romania", "South Africa", "Spain", "Sri Lanka", "South Korea", "Sudan", "Switzerland",
"Syria", "Ukraine", "United Arab Emirates", "United Kingdom", "United States", "Uruguay", "Vietnam", "Venezuela", "Yemen","Zimbabwe"), ]

# Heatmap
par(las=2)
ggplot(selected_countries, aes(x = Country, y = month, fill = Ave_monthly, frame = Country)) +
  geom_tile(color = "white", size = 0.1) +
  scale_fill_gradient(name = "Average Temperature",low = "white", high = "red") +
  coord_equal() +
  labs(x = "Countries", y = "Months", title = "Average Temp from 1750 - 2015") +
  theme(axis.ticks = element_blank()) +
  theme(axis.text = element_text(size = 9)) +
  theme(plot.title = element_text(size = 16)) +
  theme(legend.title = element_text(size = 14)) +
  theme(legend.text = element_text(size = 9))+
 theme(axis.text.x = element_text(angle = 90, hjust = 1))