#This code assumes your data is starting in a dataframe called df

#Creates feature which tells you how long it has been since an item last sold
df <- df %>%
  group_by(store_nbr, item_nbr) %>%
  mutate(how_long = (date - (lag(date))-1))

df$how_long <- gsub(" days", "", df$how_long)
df$how_long <- as.numeric(df$how_long)

#Break out the date variable into month as an integer, day of the month and day of the week
#as an integer
df <- df %>%
  group_by(store_nbr, item_nbr) %>%
  mutate(month_num = month(date), month_day = mday(date), day_num = wday(date))


#create pay day variable
df <- df %>%
  group_by(month_num) %>%
  mutate(payday = ifelse(month_day == 15, 1,
                         ifelse(month_day == max(month_day), 1, 0)))

#This variable calculates the day to day changes in sales for a product
df <- df %>%
  group_by(store_nbr, item_nbr) %>%
  mutate(differencing = unit_sales - lag(unit_sales))