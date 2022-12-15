# loading the package and data
mapdata <- get(load("../data/GPDDFiltered.RData"))
require(maps)

# creating a world map
mymap <- map(database = "world", fill = TRUE, col = "grey", border = "black", ylim = c(-60,90))

# superimpose the locations from the data
points(x = mapdata$lat, y = mapdata$long, col = rainbow(200), pch = 20)

#########################
#########################

# From the map driven, it is observed that the data is gathered around the east coast of Africa, and few data is obtained from the Atlantic Ocean and the Southern Ocean.
# Since the location of data source is not random enough, the results driven may not be representative for species worldwide, which could lead to a bias in analysis.
# Meanwhile, it is possible to use the data from Atlantic Ocean and Southern Ocean as representative for these area.
# However, due to the limited number of data, it could also cause bias by the comparatively small sample size.

#########################