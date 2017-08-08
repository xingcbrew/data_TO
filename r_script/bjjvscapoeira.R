library(dplyr)
library(ggplot2)
library(ggthemes)
library(reshape2)

### Compare online searches for BJJ and Capoeira over the past 5 years ### 

setwd("/Users/xing/Documents/bjjvscapoeira")

dat1 <- read.csv("searches.csv", stringsAsFactors = F)
dat2 <- read.csv("bycountry.csv", stringsAsFactors = F)

dat1$week <- as.Date(dat1$week, "%Y-%m-%d")

# melt by week
melt <- melt(dat1, "week")

# plot searches over time
ggplot(melt, aes(x=week, y=value, group=variable, colour=variable)) +
  geom_line(size=.8) +
  geom_smooth() +
  xlab("Date") +
  ylab("Search Volume") + 
  ggtitle("Google Searches for Brazilian Jiujitsu (BJJ) vs Capoeira") +
  scale_colour_manual(name="Search Term", labels=c("BJJ", "Capoeira"), values=c("#006C9A", "#00BEBE")) +
  theme_minimal()

# read in csvs for search volumbe by country
dat3 <- read.csv("bjj_popularity.csv")
dat4 <- read.csv("capoeira_popularity.csv")

# select only the top 10 countries
dat3 <- dat3[1:10,]
dat4 <- dat4[1:10,]

# relevel countries by search volume so that barplot in descending order
dat3$country <- factor(dat3$country, levels = c("Finland", "Australia", "South Korea", "Ireland",
                                                "New Zealand", "Sweden", "United States", "Canada", 
                                                "Singapore", "Brazil"))

dat4$country <- factor(dat4$country, levels = c("Angola", "Mozambique", "Brazil", "Israel", "Portugal",
                                                "Bolivia", "Ecuador", "Switzerland", "France", "Czechia"))

# make the bar plots of search volume in top ten countries
ggplot(dat3, aes(x=country, y=bjj_popularity)) +
  geom_bar(stat="identity") +
  xlab("Country") +
  ylab("Search Volume") +
  ggtitle("BJJ Search Volume in Top 10 Countries") +
  theme_minimal()

ggplot(dat4, aes(x=country, y=capoeira.popularity)) +
  geom_bar(stat="identity") +
  xlab("Country") +
  ylab("Search Volume") +
  ggtitle("Capoeira Search Volume in Top 10 Countries") +
  theme_minimal()


