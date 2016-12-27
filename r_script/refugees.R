library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggthemes)

# read in syrian refugee data
dat1 <- read.csv("/Users/xing/Documents/data_TO/data/syrian_refugees.csv")

# make bar graphs showing number of refugees by province
temp <- dat1 %>%
  select(province, gender, total) %>%
  filter(province != "CAN" & province != "unstated")

ggplot(temp, aes(x=province, y=total, fill=gender)) +
  geom_bar(stat='identity') +
  xlab("Province") +
  ylab("Number of Refugees") +
  ggtitle("Syrian Refugees Admitted to Canada\nby Province between Nov 2015-Sept 2016") +
  scale_fill_manual(name="Gender", labels=c("Female", "Male"), values=c("#FF5733", "#FFC300")) +
  theme_fivethirtyeight()

 temp2 <- dat1 %>%
   select(province, blended, government, private, unstated) %>%
   filter(province != "CAN" & province !="unstated")
 
 temp2_m <- melt(temp2, id.vars = "province")
 
 ggplot(temp2_m, aes(x=province, y=value, fill=variable)) +
   geom_bar(stat="identity") +
   xlab("Province") +
   ylab("Number of Refugees") +
   ggtitle("Syrian Refugees Admitted to Canada between\nNov 2015-Sept 2016 by Type of Sponsorship") +
   scale_fill_manual(name="Type of Sponsorship", labels=c("Blended", "Government-Assisted", "Privately Sponsored", "Unstated"), 
                     values=c("#A9DFBF", "#73C6B6", "#ABB2B9", "#85929E")) +
   theme_fivethirtyeight()

### Notes about data ###   
# Data are preliminary estimates and are subject to change.
# Syrian refugees include persons processed under Canada’s Syrian refugee resettlement commitment.
# Source: IRCC, September 30, 2016 Data
# http://open.canada.ca/data/en/dataset/ca243c40-a6d3-4a46-a578-b4fad4369df0

# read in data about refugee education levels  
dat2 <- read.csv("/Users/xing/Documents/data_TO/data/refugee_education.csv")

# plot cumulative sum of refugees by year
dat2_m <- melt(dat2, id.vars="education")

dat_year <- dat2_m %>%
  group_by(variable) %>%
  summarise(total = sum(value))

dat_year$cumsum <- cumsum(dat_year$total)
dat_year$variable <- gsub("X", "", dat_year$variable)
dat_year$variable <- as.numeric(dat_year$variable)

ggplot(dat_year, aes(x=variable, y=cumsum)) +
  geom_point() +
  geom_line() +
  xlab("Year") +
  ylab("Number of Refugees") +
  ggtitle("Cumulative Number of Refugees Admitted to Canada\nbetween 2011-2016") +
  theme_fivethirtyeight()

# make bar plot showing level of education of refugees by year
ggplot(data = dat2_m, aes(x=variable, y=value, fill=education)) +
  geom_bar(stat="identity") +
  xlab("Year") +
  ylab("Number of Refugees Admitted") +
  ggtitle("Education Level of Refugees Admitted into Canada\nbetween 2011 and Sept 2016") +
  scale_fill_hue(name="Education Level") +
  scale_x_discrete(labels=c("X2011" = "2011", "X2012" = "2012", "X2013" = "2013", "X2014" = "2014",
                            "X2015" = "2015", "X2016" = "Jan-Sept 2016")) +
  theme_fivethirtyeight()

# compare government resettled refugees vs refugee claims
dat3 <- read.csv("/Users/xing/Documents/data_TO/data/refugee_claims.csv")
##Source:  IRCC (EDW) of May 08, 2016
##* "Other" includes: other countries, stateless, unknown, missing and/or invalid data

# make df with Total refugee claims
dat3_tot <- dat3[dat3$Category == "Total",]

dat3_m <- melt(dat3_tot, id.vars = "Category")

dat3_m$variable <- gsub("X", "", dat3_m$variable)

dat3_m <- dat3_m[-c(6,7), ]
dat3_m$variable <- as.numeric(dat3_m$variable)

dat3_m$cumsum <- cumsum(dat3_m$value)

r_tot <- left_join(dat_year, dat3_m, by = "variable")
r_tot$total <- NULL
r_tot$Category <- NULL
r_tot$value <- NULL

r_tot_m <- melt(r_tot, id.vars = "variable")
colnames(r_tot_m)[1] <- "year"

ggplot(r_tot_m, aes(year, value, color=variable)) +
  geom_line() +
  xlab("Year") +
  ylab("Number of Refugees") +
  ggtitle("Government Supported vs. Refugee Claimants") +
  scale_color_manual(name="Type of Refugee", labels=c("Government Admitted", "Refugee Claimants"), 
                                                      values=c("#73C6B6", "#ABB2B9")) +
  theme_fivethirtyeight()