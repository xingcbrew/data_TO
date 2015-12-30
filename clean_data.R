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

## make new variable of percent of total population that is low income
dat$per_low_family <- (dat$low_income_families/dat$total_population)*100

# round to nearest 10th
dat$per_low_family <- round(dat$per_low_family, 2)

## create new categorial variable based on dat$per_low_family
## cut off at median

dat$cat_low_family <- ifelse(dat$per_low_family > 13.06, "poor", 
                             ifelse(dat$per_low_family > 8.235 & 
                                      dat$per_low_family <= 13.06, "medium", "rich"))

