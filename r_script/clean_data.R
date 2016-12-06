if(Sys.info()['user'] == 'Xing Chiu'){
  data <- "C:/Users/Xing Chiu/Documents/data_TO"
} else if(Sys.info()['user'] == 'benbrew'){
  data <- '/home/benbrew/Documents/data_TO'
}

setwd(data)

dat <- read.csv('toronto_data.csv')


## delete unecessary columns
dat$Neighbourhood.Id <- NULL
dat$Combined.Indicators <- NULL 

## make column names lower case
colnames(dat) <- tolower(colnames(dat))

## clean col names more
colnames(dat) <- gsub(".","_",colnames(dat), fixed = TRUE)
colnames(dat)[6] <- "unemployed"

## check if there are any NAs
any(is.na(dat))

## make histogram 
hist(dat$low_income_families)
summary(dat$low_income_families)

hist(dat$low_income_families, breaks = 20,
    col = "green",
    xlab = "Number of Low Income Families",
    main = "Distribution of Low Income Fmilies")
abline(v = dat$low_income_families[which(dat$neighbourhood == "Malvern")],
       col = "red")
summary(dat$low_income_families)

## make new variable of percent of total population that is low income families
dat$per_low_family <- (dat$low_income_families/dat$total_population)*100

# round to nearest 10th
dat$per_low_family <- round(dat$per_low_family, 2)

## create new categorial variable based on dat$per_low_family
## cut off at median

dat$cat_low_family <- ifelse(dat$per_low_family > 13.06, "poor", 
                             ifelse(dat$per_low_family > 8.235 & 
                                      dat$per_low_family <= 13.06, "medium", "rich"))

## make variable for percentage of total population that is low income
dat$per_low_population <- (dat$low_income_population/dat$total_population)*100
dat$per_low_population <- round(dat$per_low_population, 2)

## create new categorical variable based on dat$per_low_population
dat$cat_low_population <- ifelse(dat$per_low_population > 27.10, "poor",
                                 ifelse(dat$per_low_population > 14.16 &
                                      dat$per_low_population <= 27.10, "medium","rich"))

## make variable for percentage pf visible minority
dat$per_vis_min <- (dat$visible_minority_category/dat$total_population)*100
dat$per_vis_min <- round(dat$per_vis_min, 2) 

## create category for visible minority in neighbourhoods
dat$cat_vis_min <- ifelse(dat$per_vis_min > 63.50, "high",
                          ifelse(dat$per_vis_min > 25.21 & 
                                   dat$per_vis_min <= 63.5, "medium", "low"))

## make variable for assaults per capita in each neighbourhood
dat$assaults_per_cap <- (dat$assaults/dat$total_population)
dat$assaults_per_cap <- round(dat$assaults_per_cap, 4)

## explore average income
summary(dat$average_family_income)
hist(dat$average_family_income)

## create categories of income by neighbourhood
dat$cat_avg_income <- ifelse(dat$average_family_income > 92800, "high",
                             ifelse(dat$average_family_income > 65840 &
                                      dat$average_family_income <= 92800,"medium",
                                    "low"))


## see if there is pattern between percentage visible minority and average income
library(ggplot2)
library(scales)

ggplot(dat, aes(x=dat$per_vis_min, y=dat$average_family_income)) +
  geom_point(shape = 1, alpha=1/4) +
  scale_colour_hue(l=50) +
  geom_smooth(method=lm, color = "orange") +
  xlab("Percentage Visible Minority") +
  ylab("Average Family Income") +
  ggtitle("Percentage of Visible Minorities vs Family Income by Toronto Neighbourhoods") +
  scale_y_continuous(labels = comma)

## determine which neighbourhoods have highest assaults per capita
dat$neighbourhood[dat$assaults_per_cap == max(dat$assaults_per_cap)]
# Highest is Bay Street Corridor

## determine unemployment rate by neighbourhood
dat$per_unemployed <- (dat$unemployed/dat$total_population)*100
dat$per_unemployed <- round(dat$per_unemployed, 2)

## explore unemployment rates
summary(dat$per_unemployed)
dat$neighbourhood[dat$per_unemployed == max(dat$per_unemployed)]
dat$neighbourhood[dat$per_unemployed < 3]



