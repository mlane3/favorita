library(lubridate)
library(data.table)
library(dplyr)
#read in data

train <- fread("../input/train.csv", select = c("date", "store_nbr", "item_nbr", "unit_sales"))

#find the first date a product had sales
min_date <- train %>% group_by(store_nbr, item_nbr) %>% summarise(min_date = min(date)) 

#convert this to a date
min_date$min_date <- ymd(min_date$min_date)

#create dataframe for all possible dates for all products
all_dates <- data.frame()
for (i in 1:nrow(min_date)){
  temp <- data.frame()
  item_nbr = min_date$item_nbr[i]   
  store_nbr = min_date$store_nbr[i]
  xdate <- seq(min_date$min_date[i], as.Date("2017-08-15"), by = "days")
  
  temp <- data.frame(store_nbr = store_nbr, item_nbr = item_nbr, date = xdate)
  all_dates <- rbind(all_dates, temp)             
}

#join the unit_sales and and all of the dates
all_dates <- right_join(train, all_dates)