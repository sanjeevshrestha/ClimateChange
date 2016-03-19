
library(sp)
library(maps)
library(raster)
library(gstat)
library(geoR)
library(maptools)
library(GISTools)
library(animation)



glt <- read.csv("data/GlobalLandTemperaturesByCity.csv", stringsAsFactors = FALSE)

glt$O1 <- substr(glt$Longitude, nchar(glt$Longitude), nchar(glt$Longitude))
glt$O2 <- substr(glt$Latitude, nchar(glt$Latitude), nchar(glt$Latitude))

glt$Longitude <- ifelse(glt$O1 == "W",
                        -as.numeric(substr(glt$Longitude, 1, nchar(glt$Longitude)-1)),
                        as.numeric(substr(glt$Longitude, 1, nchar(glt$Longitude)-1)))

glt$Latitude <- ifelse(glt$O2 == "S",
                       -as.numeric(substr(glt$Latitude, 1, nchar(glt$Latitude)-1)),
                       as.numeric(substr(glt$Latitude, 1, nchar(glt$Latitude)-1)))

glt$Year <- substr(glt$dt, 1, 4)

# Adapt to map IDs, i.e.
glt$Country <- ifelse(glt$Country=="United Kingdom", "UK", glt$Country)
glt$Country <- ifelse(glt$Country=="United States", "USA", glt$Country)

glt.continent = data.frame(
  Country = c(
    "Algeria","Angola","Benin","Botswana","Burkina Faso","Burundi","Cameroon","Central African Republic","Chad","Congo","Congo (Democratic Republic Of The)","Côte D'Ivoire","Djibouti","Egypt","Equatorial Guinea","Eritrea","Ethiopia","Gabon","Gambia","Ghana","Guinea","Guinea Bissau","Kenya","Lesotho","Liberia","Libya","Madagascar","Malawi","Mali","Mauritania","Mauritius","Morocco","Mozambique","Namibia","Niger","Nigeria","Reunion","Rwanda","Senegal","Sierra Leone","Somalia","South Africa","Sudan","Swaziland","Tanzania","Togo","Tunisia","Uganda","Zambia","Zimbabwe","Afghanistan","Bahrain","Bangladesh","Burma","Cambodia","China","Hong Kong","India","Indonesia","Iran","Iraq","Israel","Japan","Jordan","Kazakhstan","Laos","Lebanon","Malaysia","Mongolia","Nepal","Oman","Pakistan","Philippines","Qatar","Russia","Saudi Arabia","Singapore","South Korea","Sri Lanka","Syria","Taiwan","Tajikistan","Thailand","Turkey","Turkmenistan","United Arab Emirates","Uzbekistan","Vietnam","Yemen","Albania","Armenia","Austria","Azerbaijan","Belarus","Belgium","Bosnia And Herzegovina","Bulgaria","Croatia","Cyprus","Czech Republic","Denmark","Estonia","Finland","France","Georgia","Germany","Greece","Hungary","Iceland","Ireland","Italy","Latvia","Lithuania","Macedonia","Moldova","Montenegro","Netherlands","Norway","Poland","Portugal","Romania","Serbia","Slovakia","Slovenia","Spain","Sweden","Switzerland","Ukraine",
    #"United Kingdom",
    "UK",
    "Bahamas","Canada","Costa Rica","Cuba","Dominican Republic","El Salvador","Guatemala","Haiti","Honduras","Jamaica","Mexico","Nicaragua","Panama","Puerto Rico",
    #"United States",
    "USA",
    "Australia","New Zealand","Papua New Guinea","Argentina","Bolivia","Brazil","Chile","Colombia","Ecuador","Guyana","Paraguay","Peru","Suriname","Uruguay","Venezuela"
  ),
  Continent = c(
    "Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Africa","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Asia","Europe","Europe","Europe","Europe","Europe","Europe","Europe","Europe","Europe","Europe","Europe","Europe","Europe","Europe","Europe","Europe","Europe","Europe","Europe","Europe","Europe","Europe","Europe","Europe","Europe","Europe","Europe","Europe","Europe","Europe","Europe","Europe","Europe","Europe","Europe","Europe","Europe","Europe","Europe","Europe","North America","North America","North America","North America","North America","North America","North America","North America","North America","North America","North America","North America","North America","North America","North America","Oceania","Oceania","Oceania","South America","South America","South America","South America","South America","South America","South America","South America","South America","South America","South America","South America America"
  ))

#glt$Continent <- glt.continent[match(glt$Country, glt.continent$Country), ]$Continent

glt.y <- aggregate(list(
  AvgTemp    = glt$AverageTemperature,
  AvgTempUnc = glt$AverageTemperatureUncertainty
),
by = list(
  Year      = round(as.numeric(glt$Year) / 2) * 2,
  Country   = glt$Country,
  #City      = glt$City,
  Latitude  = round(glt$Latitude  / 2) * 2,
  Longitude = round(glt$Longitude / 2) * 2),
FUN = mean, na.rm = TRUE
)

glt.y <- subset(glt.y, !is.na(glt.y$AvgTemp))
coordinates(glt.y) <- ~ Longitude + Latitude



get.countries <- function(continents, resolution) {
  
  countries <- as.character(glt.continent[glt.continent$Continent %in% continents, ]$Country)
  
  # Chose map
  w <- map("world", fill = TRUE, plot = FALSE)
  IDs <- sapply(strsplit(w$names, ":"), function(x) x[1])
  w <- map2SpatialPolygons(w, IDs = IDs)
  
  # Subsetting by countries
  w <- w[names(w) %in% countries, ]
  
  ext.bez  <- extent(w)
  xy       <- abs(apply(as.matrix(bbox(ext.bez)), 1, diff))
  r        <- raster(ext.bez, ncol=xy[1], nrow=xy[2])
  res(r)   <- resolution
  
  # Rasterize
  ras <-rasterize(w, r)
  ras <- as(ras, "SpatialPixels")
  
  years <- unique(subset(glt.y@data, !is.na(AvgTemp))$Year)  # No data for 1746?
  
  # This GIF creation part has originally been forked from:
  # https://www.kaggle.com/tentotheminus9/d/kaggle/climate-data-from-ocean-ships/the-voyages-of-james-cook
  ani.options(interval = 0.1)
  
  saveGIF( {
    sapply(years, function(x) {
      
      d <- subset(glt.y, Year==x)
      m <- "Average Land Temperature in Celsius"
      #s <- seq(-5, 30, by=2.5)
      
      idw <- krige(AvgTemp ~ 1, d, ras, block= c(100))
      
      image(idw, xlab=x, asp=1, main=m)    
      
      sh <- shading(breaks = seq(0, 15, by=5), cols = heat.colors(5))
      choro.legend(px = "bottomleft", sh = sh, bg = "white", cex = 0.75)
      
      #legend.krige(
      #    x.leg=c(0, 3),
      #    y.leg=c(0, 30),
      #    val=idw$var1.pred,
      #    vert = TRUE,
      #    # offset.leg = 0.5,
      #    scale.vals = s)
      
      ani.pause()
    })
  },
  movie.name = paste(paste(continents, collapse="_"), ".gif", sep = ""),
  img.name = "Rplot", 
  convert = "convert",
  ani.width = 800,
  ani.height = 600)
  
 # plot(w, main = "The Grid")
#  points( glt.y[!is.na(over(glt.y, w)), ], pch=3)
}

get.countries(c("Europe", "Asia","Africa","North America","South America"), 0.3)
get.countries(c("Europe"), 0.1)
get.countries(c("Asia"), 0.4)
get.countries(c("Africa"), 0.2)
get.countries(c("North America"), 0.1)
get.countries(c("South America"), 0.4)
