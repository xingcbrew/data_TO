library(ggplot2)
library(dplyr)
library(reshape2)
library(ggmap)
library(rgdal)
library(rgeos)
library(maptools)
library(tidyr)
library(tmap)
library(raster) #to read in shapefile
library(sp)
library(ggthemes)

#set working directory
setwd("/Users/xing/Documents/data_TO")

# import data
dat1 <- read.csv("/Users/xing/Documents/data_TO/data/housing_prices.csv")
dat1$X <- NULL

# rm cols not needed for melting
dat1$neighbourhood_ids <- NULL
dat1$change_2015_2016 <- NULL
dat1$change_2012_2016 <- NULL

# remove rows that have at least na
dat1 <- dat1[complete.cases(dat1),]

# melt into long form by district
temp <- melt(dat1, id.vars = c('district'))

#plot housing price change by year
ggplot(data = temp, aes(x=variable, y=value, group=district, colour=district)) +
  geom_smooth(se=F) +
  xlab("Month/Year") +
  ylab("House Price") +
  ggtitle("Toronto House Prices by MLS District") +
  scale_color_discrete(name = "Districts") +
  scale_x_discrete(breaks=c("june_2012", "dec_2012", "june_2013", "dec_2013", "june_2014", "dec_2014",
                            "june_2015", "dec_2015", "june_2016", "nov_2016"),
                   labels=c("06/2012", "12/2012", "06/2013", "12/2013", "06/2014", "12/2014", "06/2015", 
                            "12/2015", "06/2016", "11/2016")) +
  theme_minimal()
  
#make bar graph showing comparison between percentage increase 2012-2016
ggplot(data = dat1, aes(x=reorder(district, change_2012_2016), y=change_2012_2016)) +
  geom_bar(stat = "identity") +
  xlab("District") +
  ylab("Percentage Increase in Price") +
  ggtitle("Percent of Increase in Average House Prices, 2012-2016") +
  theme_economist_white()

# read in neighbourhood data

n <- read.csv("/Users/xing/Documents/data_TO/data/wellbeing_toronto.csv")

# add leading zeros to ID number to match map
n$neighbourhood_id[1:9] <- paste0("00", n$neighbourhood_id[1:9])
n$neighbourhood_id[10:99] <- paste0("0", n$neighbourhood_id[10:99])

# read in map file
map <- shapefile("/Users/xing/Documents/data_TO/maps/NEIGHBORHOODS_WGS84.shp")

# rename neighbourhood_id to match maps in order to join
names(n)[names(n) == 'neighbourhood_id'] <- "AREA_S_CD"

# left_join dat1 to map
map@data <- left_join(map@data, n)

### to change map data into data.frame, use fortify() eg., map <- fortify(map)
map_f <- fortify(map)

# create a col with row ids in map so that can join with map_f (data.frame)
map$id <- row.names(n)

# join fortified map_f with map
map_f <- left_join(map_f, map@data)

##

###### dissolve neighbourhoods into districts #####
dis <- raster::aggregate(map, 'district')

# join dat1 to map
dis@data <- left_join(dis@data, dat1)

# fortify dis map
dis_f <- fortify(dis)

# create col with row ids to join 
dis$id <- row.names(dat1)

# join dis_f with dis
dis_f <- left_join(dis_f, dis@data)

# make thematic map of housing prices by district
ggplot() + 
  geom_polygon(data = dis_f,
               aes(x=long, y = lat, group = group, fill = dec_2012),
               color="transparent", size=0.25) +
  scale_fill_distiller(name = "House Prices", palette = 1, trans = "reverse") +
  theme_nothing(legend=T) 

