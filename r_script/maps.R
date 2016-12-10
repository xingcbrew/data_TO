library(ggplot2)
library(dplyr)
library(reshape2)
library(ggmap)
library(rgdal)
# library(rgeos)
library(maptools)
library(tidyr)
# library(tmap)
# library(raster)
library(sp)

#set working directory
setwd("/Users/xing/Documents/data_TO")

# import data
dat1 <- read.csv("/Users/xing/Documents/data_TO/data/wellbeing_toronto.csv")

# remove unecessary columns
# dat1$X <- NULL

# round decimals
dat1$road_kms <- round(dat1$road_kms, digits = 2)

# add leading zeros to ID number to match map
dat1$neighbourhood_id[1:9] <- paste0("00", dat1$neighbourhood_id[1:9])
dat1$neighbourhood_id[10:99] <- paste0("0", dat1$neighbourhood_id[10:99])

# add new variables to dat1 (with percentages of certain variables)
dat1$percent_low_income <- round((dat1$low_income_population / dat1$total_population)*100, digits = 2)
dat1$percent_recent_imm <- round((dat1$recent_immigrant / dat1$total_population)*100, digits = 2)
dat1$perecent_visible_minority <- round((dat1$visible_minority / dat1$total_population)*100, digits = 2)
dat1$percent_unemployed <- round((dat1$unemployed / dat1$pop_15up)*100, digits = 2)

## MAPS ##
##### read in neighbourhoods shape file #####
map <- shapefile("/Users/xing/Documents/data_TO/maps/NEIGHBORHOODS_WGS84.shp")

## another option to read in shapefile
## map1 <- readOGR(dsn="/Users/xing/Documents/data_TO/maps", layer="NEIGHBORHOODS_UTM6")

# understand shapefile data
head(map) 
plot(map)
map@data
names(map)

# remove numbers, special characters, and spaces in neighbourhood area names
map$AREA_NAME <- gsub("[[:punct:]]", "", map$AREA_NAME)
map$AREA_NAME <- gsub("[[:digit:]]", "", map$AREA_NAME)
map$AREA_NAME <- gsub("[[:space:]]", "", map$AREA_NAME)

# rename neighbourhood_id to match maps in order to join
names(dat1)[names(dat1) == 'neighbourhood_id'] <- "AREA_S_CD"

# left_join dat1 to map 
map@data <- left_join(map@data, dat1)

### to change map data into data.frame, use fortify() eg., map <- fortify(map)
map_f <- fortify(map)

# create a row with row ids in map so that can join with map_f (data.frame)
map$id <- row.names(map)

# join fortified map_f with map
map_f <- left_join(map_f, map@data)

###
###

# load ttc map and fortify to make data.frame
ttc <- shapefile("/Users/xing/Documents/data_TO/ttc/subway_wgs84.shp")
ttc_f <- fortify(ttc)

# load NIAs and fortify to make data.frame
nia <- shapefile("/Users/xing/Documents/data_TO/maps/nia.shp")
nia_f <- fortify(nia)

###
###

######## plot map data using ggplot2 ##########
plot <- ggplot(map@data, aes(traffic_collisions, walk_score))
plot + geom_point()

# resize and recolour each point by different variables
plot + geom_point(aes(colour=home_price, size=total_population)) +
  ggtitle("Walk Score vs. Traffic Collisions by Neighbourhood") 

# adding text to the plot
plot + geom_point(aes(colour = home_price, size = total_population)) +
  geom_text(size = 2, aes(label = AREA_NAME))

###
###

###### making a map using ggplot2 #####

map3 <- ggplot(map_f, aes(long, lat, group = group, fill = total_population)) +
  geom_polygon() +
  theme_nothing(legend = T) + # if you want to have just the map with nothing else, plus legend
  scale_fill_distiller(name = "Population", trans = "reverse") +
  coord_equal() +
  ggtitle("Neigbhourhoods by Population") 

# type map3 to show map

### to add text to the map ###
cnames <- aggregate(cbind(long, lat) ~ neighbourhood, data = map_f, FUN=function(x) mean(range(x)))

map4 <- ggplot() +
  geom_polygon(data = map_f, 
               aes(x = long, y = lat, group = group, fill = total_population), 
               color = "transparent", size = 0.25) + 
  coord_map() +
  scale_fill_distiller(name = "Population", trans = "reverse") +
  theme_nothing(legend = TRUE) +
  labs(title="Neighbourhoods by Population") +
  geom_text(data=cnames, aes(long, lat, label = neighbourhood), size=2)

# map showing population choropleth, highlighting NIAs, with ttc line 
ggplot() +
  geom_polygon(data = map_f, 
               aes(x = long, y = lat, group = group, fill = total_population), 
               color = "transparent", size = 0.25) + 
  geom_polygon(data = nia_f, 
               aes(x = long, y = lat, group = group),
               color = "transparent", alpha = 0.5) +
  geom_path(data = ttc_f,
               aes(x = long, y = lat, group = group)) +
  coord_map() +
  scale_fill_distiller(name = "Population", trans = "reverse") +
  theme_nothing(legend = TRUE) +
  labs(title="Neighbourhoods by Population")

# same thing but using other variables

ggplot() +
  geom_polygon(data = map_f, 
               aes(x = long, y = lat, group = group, fill = low_income_population), # just change the fill
               color = "transparent", size = 0.25) + 
  geom_polygon(data = nia_f, 
               aes(x = long, y = lat, group = group),
               color = "grey", alpha = 0.3) +
  geom_path(data = ttc_f,
            aes(x = long, y = lat, group = group)) +
  coord_map() +
  scale_fill_distiller(name = "Low Income", 
                       trans = "reverse") +
  theme_nothing(legend = TRUE) +
  labs(title="Low Income")

# to save map, use ggsave
ggsave("map3.png")

# adding other points to the map
places <- read.csv("/Users/xing/Documents/data_TO/data/places.csv")

ggplot() +
  geom_polygon(data = map_f, 
               aes(x = long, y = lat, group = group, fill = lone_parent_families), # just change the fill
               color = "transparent", size = 0.25) + 
  geom_polygon(data = nia_f, 
               aes(x = long, y = lat, group = group),
               color = "yellow", fill = "transparent") +
  geom_path(data = ttc_f,
            aes(x = long, y = lat, group = group)) +
  geom_point(data = places,
             aes(x = long, y = lat), 
             color = "orange", alpha = 0.6, size = 3) +
  geom_text(data = places, aes(long, lat, label = place_name), size = 4) +
  coord_map() +
  scale_fill_distiller(name = "Lone Parent Families", palette = 2, # choose colour scheme using palette
                       trans = "reverse") +
  theme_nothing(legend = TRUE) +
  ggtitle("Plotting Points on Map")