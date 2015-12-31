###### Map making - Toronto Neighbourhoods #######
library(maps)
library(mapdata)
library(maptools) # for shapefiles
library(scales) # for transparency 
library(ggplot2)
library(rgdal)

neigh_map <- readOGR(dsn = "C:/Users/Xing Chiu/Documents/data_TO/neighbourhoods",
                     layer = "neighbourhoods")

require(rgdal)
nm <- readShapePoints("~/neighbourhoods/neighbourhoods.shp")
