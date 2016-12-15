### map a leaflet map of all the places I've lived, with circles showing number of months there

library(ggplot2)
library(ggmap)
library(dplyr)
library(reshape2)
library(ggsn)
library(tmap)
library(rgeos)
library(leaflet)

# set working directory and read in file
setwd("/Users/xing/Documents/data_TO")
p <- read.csv("/Users/xing/Documents/data_TO/data/places_lived.csv")

# pop up label
lab <- paste(p$city_name, p$reason_there, sep = " | ")

# make the map
m <- leaflet(data = p) %>% setView(lng = 0, lat= 0, 2)

places_map <- m %>% addProviderTiles("Stamen.Toner", options = providerTileOptions(opacity = 0.35)) %>%
  addCircleMarkers(~long, ~lat, popup = ~as.character(lab),
                   radius = ~ifelse(total_months > 0 & total_months < 5, 5, 
                                    ifelse(total_months >= 5 & total_months < 10, 8, 10)),
                   stroke = FALSE, fillOpacity = 0.5)