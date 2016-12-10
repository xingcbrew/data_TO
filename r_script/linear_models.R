library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyr)

# set working directory
setwd("/Users/xing/Documents/data_TO")

# import data
dat1 <- read.csv("/Users/xing/Documents/data_TO/data/wellbeing_toronto.csv")

#### clean data.frame ####

# round decimals
dat1$road_kms <- round(dat1$road_kms, digits = 2)

# make column for percent of people that are visible minority, recent immigrants, low income population
dat1$percent_low_income <- round((dat1$low_income_population / dat1$total_population)*100, digits = 2)
dat1$percent_recent_imm <- round((dat1$recent_immigrant / dat1$total_population)*100, digits = 2)
dat1$perecent_visible_minority <- round((dat1$visible_minority / dat1$total_population)*100, digits = 2)
dat1$percent_unemployed <- round((dat1$unemployed / dat1$pop_15up)*100, digits = 2)

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

# multivariate lineaar regressions using lm function
mod1 <- lm(traffic_collisions ~ road_kms + total_population + lone_parent_families + 
            vehicle_thefts + walk_score, data = dat1)
