library(forecast)
library(dplyr)
library(sweep)
library(tidyr)
library(purrr)
library(timetk)
library(broom)
library(lubridate)
library(data.table)
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

#change NAs to 0 for products that do not have sales
all_dates$unit_sales <- ifelse(is.na(all_dates$unit_sales) == TRUE, 0, all_dates$unit_sales)

#let's try to de-seasonalize the data

####################### THIS CODE WORKS

#We remove products having less than 15 sales data points because we
#need at least two periods to de-seasonalize and if a product has less than 15
#data points it hasn't been on the market for more than 2 weeks.
new_products <- train %>% group_by(store_nbr, item_nbr) %>% tally() %>% filter(n < 15) %>% ungroup() 

#create a nested data frame
#filter out items in that were in new_products
unit_sales_nest <- all_dates %>%
  filter(!item_nbr %in% new_products$item_nbr) %>%
  group_by(store_nbr, item_nbr) %>%
  nest(.key = "data.tbl") 


unit_sales_ts <- unit_sales_nest %>%
  mutate(data.ts = map(.x = data.tbl,
                       .f = tk_ts,
                       select = -date, 
                       freq = 7))

#de-seasonalize the data
unit_sales_ts_decomp <- unit_sales_ts %>%
  mutate(decomp = map(data.ts, decompose))


##this for loop pulls the relevant data out of the decompose nested column
decomp_df <- data.frame()
for (i in 1:nrow(unit_sales_ts_decomp)){
  temp <- unit_sales_ts_decomp$decomp[i]
  seasonal_temp <- temp[[1]]$seasonal
  trend_temp <- temp[[1]]$trend
  random_temp <- temp[[1]]$random
  temp_df <- data.frame(cbind(seasonal_temp, trend_temp, random_temp))
  decomp_df <- bind_rows(decomp_df, temp_df)
  
}
#filter out the products who didn't have at least 2 weeks worth of data
df2 <- all_dates %>% 
  filter(!item_nbr %in% new_products$item_nbr) %>% 
  ungroup()

#bind together with the old dataframe to create a dataframe with de-seasonalized data
df2 <- cbind(df2, decomp_df)

