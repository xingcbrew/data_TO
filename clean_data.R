if(Sys.info()['user'] == 'Xing Chiu'){
  data <- "C:/Users/Xing Chiu/Documents/data_TO"
} else if(Sys.info()['user'] == 'benbrew'){
  data <- '/home/benbrew/Documents/data_TO'
}

setwd(data)

dat <- read.csv('toronto_data.csv')


