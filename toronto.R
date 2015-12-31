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


###### Trends for John Tory vs. Rob Ford ##################
library(gtrendsR)
library(dplyr)

## Set up google trends
usr <- "xingchiu2@gmail.com"
psw <- "h3yxing24"
ch <- gconnect(usr, psw) 

##  mayor <- gtrends(c("data is", "data are"), res="day")
mayor <- gtrends(query = 'rob ford',  
                  res="week")

##### Get the trend into a dataframe
trend <- mayor$trend

##### Make a date object from start
trend$date <- as.Date(trend$start)

##### Get a year column
trend$year <- format(trend$date, '%Y')

##### Group by year and replot
temp <- 
  trend %>%
  group_by(year) %>%
  summarise(avg = mean(rob.ford))

## Make year numeric
temp$year <- as.numeric(temp$year)

## Plot temp
ggplot(data = temp,
       aes(x = year, y = avg)) +
  geom_line(color = "blue") +
  geom_point(alpha = 0.5) +
  xlab("Years") + ylab("Search Volume") +
  ylim(0,15) +
  ggtitle("Google Searches for Rob Ford 2004-Present")


####John Tory

tory <- gtrends(query = 'john tory',  
                 res="week")

##### Get the trend into a dataframeag
trend <- tory$trend

##### Make a date object from start
trend$date <- as.Date(trend$start)

##### Get a year column
trend$year <- format(trend$date, '%Y')

##### Group by year and replot
temp_tory <- 
  trend %>%
  group_by(year) %>%
  summarise(avg = mean(john.tory))

## Make year numeric
temp_tory$year <- as.numeric(temp$year)

## Plot temp_tory
ggplot(data = temp_tory,
       aes(x = year, y = avg)) +
  geom_line(color = "red") +
  geom_point(alpha = 0.5) +
  xlab("Years") + ylab("Search Volume") +
  ylim(0,15) +
  ggtitle("Google Searches for John Tory 2004-Present")

## Merge datasets for temp and temp_tory
m=merge(temp, temp_tory, by="year")

## Plot in one chart
library(reshape2)

m1 <- melt(m, id="year")  # convert to long format
search <- ggplot(data=m1,
       aes(x=year, y=value, color = variable)) +
  geom_line() +
  geom_point(alpha = 0.5) +
  xlab("Year") + ylab("Search Volume") +
  ggtitle("Google Trends for Rob Ford vs John Tory")

### Change legend title and names
search
search + scale_colour_discrete(name  ="Search Term",
                               breaks=c("avg.x", "avg.y"),
                               labels=c("Rob Ford", "John Tory")) +
  scale_shape_discrete(name  ="Search Term",
                       breaks=c("avg.x", "avg.y"),
                       labels=c("Rob Ford", "John Tory"))  

