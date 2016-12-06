library(ggplot2)
library(dplyr)
library(reshape2)
library(ggmap)
library(rgdal)
library(rgeos)
library(maptools)
library(tidyr)
library(tmap)
library(raster)
library(tmap)

#set working directory
setwd("/Users/xing/Documents/data_TO")

# import data
dat1 <- read.csv("/Users/xing/Documents/data_TO/data/wellbeing_toronto.csv")

# remove unecessary columns
dat1$X <- NULL

# round decimals
dat1$road_kms <- round(dat1$road_kms, digits = 2)

# add leading zeros to ID number to match map
dat1$neighbourhood_id[1:9] <- paste0("00", dat1$neighbourhood_id[1:9])
dat1$neighbourhood_id[10:99] <- paste0("0", dat1$neighbourhood_id[10:99])

### simple regression with population and traffic collisions using ggplot2 ###
ggplot(dat1, aes(x=total_population, y=traffic_collisions)) +
  geom_point(alpha=0.6) +
  geom_smooth(method=lm) + 
  xlab("Neighbourhood Population") +
  ylab("Traffic Collisions") +
  ggtitle("Traffic Collisions vs. Total Population in Toronto Neighbourhoods")

### see outlier neighbourhoods with high traffic collisions  
dat1$neighbourhood[dat1$traffic_collisions > 600]

### population vs pedestrian collisions
ggplot(dat1, aes(x=total_population, y=pedes_colissions)) +
  geom_point(alpha=0.6) +
  geom_smooth(method=lm) + 
  xlab("Neighbourhood Population") +
  ylab("Pedestrian and Other Collisions") +
  ggtitle("Pedestrian and other Collisions vs. Total Population in Toronto Neighbourhoods")

dat1$Neighbourhood[dat1$pedes_colissions > 700]

## road km vs traffic kms
ggplot(dat1, aes(x=road_kms, y=traffic_collisions)) +
  geom_point(alpha=0.6) +
  geom_smooth(method=lm) +
  xlab("Road Kms") +
  ylab('Traffic Collisions') + 
  ggtitle("Road Kilometers vs. Traffic Collisions")

## MAPS ##
##### read in neighbourhoods shape file #####
map <- shapefile("/Users/xing/Documents/data_TO/maps/NEIGHBORHOODS_WGS84.shp")

## another option to read in shapefile
## map1 <- readOGR(dsn="/Users/xing/Documents/data_TO/maps", layer="NEIGHBORHOODS_WGS84")

# understand shapefile data
head(map) 
plot(map)
map@data
names(map)

# remove numbers, special characters, and spaces in neighbourhood area names
map$AREA_NAME <- gsub("[[:punct:]]", "", map$AREA_NAME)
map$AREA_NAME <- gsub("[[:digit:]]", "", map$AREA_NAME)
map$AREA_NAME <- gsub("[[:space:]]", "", map$AREA_NAME)

#sort alphabetically
# dat1 <- dat1[order(dat1$Neighbourhood), ]
# map$AREA_NAME <- map[order(map$AREA_NAME), ]
# large_pop$Neighbourhood <- large_pop[order(large_pop$Neighbourhood), ]

# rename neighbourhood_id to match maps in order to join
names(dat1)[names(dat1) == 'neighbourhood_id'] <- "AREA_S_CD"

# left_join dat1 to map 
map@data <- left_join(map@data, dat1)

# create maps of population, housing prices, etc
qtm(shp = map, fill = "total_population", fill.palette = "Blues")
qtm(shp = map, fill = "home_price", fill.palette = "Reds")
qtm(shp = map, fill = "traffic_collisions", fill.palette = "Greens")
