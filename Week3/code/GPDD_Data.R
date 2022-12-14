# loading the mapping package
require(maps)

# loading the GPDD data
mapdata <- get(load("../data/GPDDFiltered.RData"))

# creating a world map
mymap <- map(database = "world")

# superimpose the locations from the data
points(x = mapdata$lat, y = mapdata$long, col = rainbow(200), pch = 20)

#########################
#########################

# From the map driven, it is observed that the data is gathered around the east
# coast of Africa, while few data is obtained from the Atlantic Ocean and the
# Southern Ocean. Since the location of data source is not random enough, the
# results driven may not be representative for species worldwide, which could
# lead to a bias in analysis.
# Meanwhile, it is possible for researchers to use the data from Atlantic Ocean
# and Southern Ocean as representative for these area. However, due to the
# limited number of data, it could also cause bias by the comparatively small
# sample size.
 
#########################