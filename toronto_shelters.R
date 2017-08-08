
library(tidyr)
library(leaflet)

shelters <- read.csv("/Users/xing/Documents/data_TO/data/shelters.csv")

# make colour palette to indicate type of shelter
pal <- colorFactor(c("blue", "red", "purple", "orange", "yellow"), domain = c("Single Men", "Single Women", 
                                                                              "Mixed Adult", "Family", "Youth"))

# information to show on pop-up when a marker is clicked
lab <- paste(shelters$name, shelters$phone, shelters$address, sep = " | ")

# get map of Toronto
m <- leaflet(data = shelters) %>% setView(lng = -79.38318, lat= 43.65323, zoom = 12)

# create the map with markers sized by shelter capacity and coloured by type of shelter

shelters_map <- m %>% addProviderTiles("Stamen.Toner", options = providerTileOptions(opacity = 0.35)) %>%
  addCircleMarkers(~long, ~lat, popup = ~as.character(lab),
                   radius = ~ifelse(capacity > 0 & capacity < 30, 5, 
                                    ifelse(capacity > 31 & capacity < 60, 8, 10)),
                   stroke = FALSE, fillOpacity = 0.5,
                   color = ~pal(type)) %>%
  addLegend("bottomright", pal = pal, values = shelters$type,
            title = "Type of Shelter",
            opacity = 0.5)

shelters_map
