

##########
##***Important note***: Replace df with your data, df$'Unit Sale'
###with your unit_sales, and lubridate mdy with the mdy you need.
#########

#Pick Data to Load ----

repos <- 'https://mirrors.nics.utk.edu/cran/'

if("lubridate" %in% rownames(installed.packages())==FALSE){install.packages("lubridate",repos = repos,dependencies = TRUE)}
if("lmtest" %in% rownames(installed.packages())==FALSE){install.packages("lmtest",repos = repos,dependencies = TRUE)}
if("smooth" %in% rownames(installed.packages())==FALSE){install.packages("smooth",repos = repos,dependencies = TRUE)}
if("forecast" %in% rownames(installed.packages())==FALSE){install.packages("forecast",repos = repos,dependencies = TRUE)}
if("data.table" %in% rownames(installed.packages())==FALSE){install.packages("data.table",repos = repos,dependencies = TRUE)}
if("dplyr" %in% rownames(installed.packages())==FALSE){install.packages("dplyr",repos = repos,dependencies = TRUE)}
if("googleAuthR" %in% rownames(installed.packages())==FALSE){install.packages("googleAuthR",repos = repos,dependencies = TRUE)}
if("googleComputeEngineR" %in% rownames(installed.packages())==FALSE){install.packages("googleComputeEngineR",repos = repos,dependencies = TRUE)}
if("googleCloudStorageR" %in% rownames(installed.packages())==FALSE){install.packages("googleCloudStorageR",repos = repos,dependencies = TRUE)}
if("caret" %in% rownames(installed.packages())==FALSE){install.packages("caret",repos = repos,dependencies = TRUE)}
if("R6" %in% rownames(installed.packages())==FALSE){install.packages("R6",repos = repos,dependencies = TRUE)}
if("jsonlite" %in% rownames(installed.packages())==FALSE){install.packages("jsonlite",repos = repos,dependencies = TRUE)}
if("SparkR" %in% rownames(installed.packages())==FALSE){install.packages("SparkR",repos = repos,dependencies = TRUE)}


library(lubridate)
library(data.table)
library(dplyr)
library(lmtest)
library(smooth)
library(forecast)

# library(smooth) #Need to figure out which library smooth or TTS kaggle prefers
# library(TTR)

##read in a document of all the data (in this case just 2016 to 2017)
# df <- fread("train_X.csv") #2016-2017 data that has been subset by Tableau Engine
#df <- fread("train+.csv") #just the 2017 data
df <- fread("~/R/sandbox2/train2016.csv") #just the 2016 data
##replace "train+.csv" with the data you wanted to load below 4.00 Gb
  #Tableau specific clean up 
    df[,`Number of Records`:=NULL]
    # df[,`Onpromotion (has nulls)`:=NULL]
# colnames(df) <- c("class, date, family, id, item_nbr_items, item_nbr, onpromotion, perishable, store_nbr
df$Date <- mdy(df$Date) #Reminder: Pick the rigth lubradate function for you data
    
#Pick Forecasting ----
PeriodstoForecast <- 2;
myfrequency <- 7; #some of the the examples I used to make this use periods. other use frequency default is 12
n<-length(df);
startyear <- shiftedyear <- year(df$Date[1]); #shifted and start year are the same here default is 2016s
# shiftedyear <- unique(year(df$Date))[2] #if more than one year

#====Outlier Search ----
library(mvoutlier)
outlierrows <- sign2(cbind(df))$wfinal01 #The most simple way to find outliers
# plot(outlierrows,df$)
rm(outlierrows)


#==== Basic Forcasting 02 ----
smoothma <- sma(df$`Unit Sales`) #smooth average from smooth library; for some odd reason this function runs out of memory too often
movingave <- ma(df$`Unit Sales`,order = myfrequency) #moving average
#
time <- ts(df$unitsales,start=c(startyear,1), frequency=myfrequency);
fcast <- forecast(ma, h=PeriodstoForecast[1]);
n <-length(df);
# append(df[(PeriodstoForecast[1]+1):n],fcast$mean, after = n-PeriodstoForecast[1])

##Fit Lower 95%
fcast$lower[,2]
# append(df[(PeriodstoForecast[1]+1):n],fcast$lower[,2], after = n-PeriodstoForecast[1])
##Fit Upper 95%
fcast$upper[,2]
# append(df[(PeriodstoForecast[1]+1):n],fcast$upper[,2], after = n-PeriodstoForecast[1])
rm(time,model,smoothma,movingave) #cleanup


##==== ARIMA forecast---- 
##library(forecast) 
# n <-length(df);
smoothma <- sma(df$`Unit Sales`) #smooth average
movingave <- ma(df$`Unit Sales`,order = myfrequency) #moving average
#
time <- ts(df$`Unit Sales`,start=c(shiftedyear,1), frequency=myfrequency);
model <- auto.arima(time)  #auto.arima(time) 
fcast <- forecast(time, h=PeriodstoForecast[1]); # forecast(time, h=PeriodstoForecast[1]); time
# append(df[(.arg2[1]+1):n],fcast$mean, after = n-.arg2[1])
rm(time,model)

#==== TBats forcast ----
#library(forecast)
time <- ts(df$unitsales,start=c(shiftedyear,1), frequency=myfrequency);
model <- tbats(time)
fcast <- forecast(time, h=PeriodstoForecast[1]);
# append(df[(PeriodstoForecast[1]+1):n],fcast$mean, after = n-PeriodstoForecast[1])",

#====Combined forcast ----
#Combine in both past and future stuff in forecast
#In pure R this is not much different form regular forecast
l<-length(df);
n<-length(df);
u<-df[1:(l-PeriodstoForecast[1])]; #We have to make the shifted date in R to combine future and past.
#THen you will need to use a date field in the opposing axis that shifts the last date you have N periods.
#This data field is what Complete Order date does
n<-length(u);
u[n]=df[l]; #Deals with the null of last date
time <- ts(u,start=c(shiftedyear,1), deltat=1/PeriodstoForecast);
fcast <- forecast(time, h=PeriodstoForecast[1]);
#append(u,fcast$mean, after = n) #Combined Both Past and Futre
#append(df[(PeriodstoForecast[1]+1):n],fcast$mean, after = n-PeriodstoForecast[1])

#====FitR Tbats -----
#library(forecast)
time <- ts(df,start=c(shiftedyear,1), frequency=myfrequency);
model <- tbats(time)
fcast <- forecast(time, h=PeriodstoForecast[1]);
n <-length(df);
# append(df[(PeriodstoForecast[1]+1):n],fcast$mean, after = n-PeriodstoForecast[1])

#==== Lm Basic ----
#library(data.table) #uncomment for faster data analysis
#ID = .arg1 #Wish I could pull in order ID to the R caculation
#Also I wish there was a pull all numerical variables for both R and Python

# df0 <- data.table(sales = df$sales, date = df$date, profit = df$profit,
#                   margin = df$margin, quantity = df$quantity);
# write.csv2(dat,file ='Tabtest.csv')

#Data Clean up--
#This is a very ultra light clean up using only correllation
#Its not realistic to real data but gives you an idea of the concept
#Its mostly to teach colinearity and homodesacity (spelling?). You want to do ANOVA, PCA, SVG, Turkey and
#other analysis methods potentionally as well.
dfCorr <- data.frame(cor(select_if(df, is.numeric), use='pairwise.complete.obs', method='pearson'))
HighCorrVariables <- rownames(dfCorr[abs(dfCorr$`Unit Sales`)>0.50,]);
df1 <- df[,colnames(df)%in%HighCorrVariables];
#Data Modeling--
fit <- lm(`Unit Sales` ~ ., data=df); #replace data=df with data=df1 for full automatic method
plot(fit)
fit$fitted.values #paste the actual 'expected' y-values of the curve
paste(test$coefficients) #past just the values

# ans <- fit$residuals[2] #the residuals
# ans <- fit$fitted.values #paste the actual 'expected' y-values of the curve
# ans <- fit$coefficients[2] #the coefficents
summary(fit);  #This funcction won't work direclty in Tableau (talk about vectors)


#==== Linear Regression ----
# R CAPSTONE PROJECT Automated Process #
# Setting Working Directory                                            #
# Rule of thumb: Keep R Script & flat data files in the same directory #
setwd("~/R/sandbox2")
# Preparing Data (Data Transformation) for Linear Regression #
# Unique Indicators in csv file
colnames(df)
# Determine NAs per variable(Indicator)
na_count <- sapply(df, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
# How Many NAs for depedent variable?
n<-na_count[rownames(na_count) == c("CO2 emissions (kt)"),]
#n<-na_count[rownames(na_count) == c("CO2 emissions (metric tons per capita)"),]
# Select variables with minimum NAs
df2 <- df[,which(na_count<=n)]
## Removing variables with "of.GDP", "LCU", "2005","of.total", "etc", "capita", #
## and "CO2 Indicators" other than "CO2 emissions (kt)" in Indicator Name       #
## df5 <- subset(df4,select = !grepl("of.GDP",colnames(df4))) #remove
## Variable selection procedure based on correlations:                          #
## Step1:Independent variable should be higly correlated with dependent variable#
## Step2:Independen variables cannot be highly correlated                       #
## Step3: While omitting variables, independent variable with higher correlation# 
## with dependent variable will have the priority, assuming no concern with     #
## this omission for the Business#

dfCorr <- cor(df, use="pairwise.complete.obs", method="pearson")
print <- as.data.frame(dfCorr)
CorrSorted <-  Corr[with(Corr,order(-abs(dfCorr$Unit.Sales),na.last = TRUE)),]
CorrSorted <- CorrSorted[,make.names(rownames(CorrSorted))]
CTD <- 0.50 # Correlation threshold with dependent variable


# index<- abs(CorrSorted[,1])>CTD
# CorrCTD <- CorrSorted[index,index]
# CTI <- 0.70 # Correlation threshold for independent variables
# CorrCTI <- CorrCTD[-1,-1]
# variables<-rownames(CorrCTD)[1]
while(max(CorrCTI)>CTI){
  variables<-rbind(variables,rownames(CorrCTI)[1])
  index<-abs(CorrCTI[,1])<CTI
  CorrCTI<<-CorrCTI[index,index]
}
CorrMatrixFinalVariables<-CorrSorted[variables,make.names(variables)]
df1<-df12[,variables]
df11 <- df1
# Let's investigate df.csv data in rattle          #
# (alternatively select df11 in rattle under R Dataset #

# library(rattle)
# rattle()

# Handling remaining multicollinearities/Singularities #
colnames(df11)

# Excluding variables causing singularities / keeping higher level variables #
# For example: if there is male/female breakout keeping just total           #

# df12 <- df11[-c(2,3)] #Remove by column

# Final Model for kaggle based on rattle log #
# Fitting model in R (No Data Partition)

kaggleFinalModelData <- df12[c(input,target)]
fit <- lm(`Unit Sales` ~ ., data=df)
summary(fit)

# Granger Causality Test
# The Granger causality test is a statistical hypothesis test for determining 
# whether one time series is useful in forecasting another
if("lmtest" %in% rownames(installed.packages())==FALSE){install.packages("lmtest")}
library(lmtest)
# do "Arms.imports..SIPRI.trend.indicator.values." granger cause "CO2 emmission"
# at lag t?
# Try order 1 to 5
grangertest(df$`Unit Sales` ~ df$Date, 
            order = 5, data = df12)

# do "Gross.national.expenditure..current.US.." granger cause "CO2 emmission"
# at lag t?
# Try order 1 to 5
grangertest(df$`Unit Sales` ~ df$Family, 
            order = 5, data = df12)

# do "Population..total" granger cause "CO2 emmission"
# at lag t?
# Try order 1 to 5
grangertest(df$`Unit Sales` ~ df$`Perishable (weight)`, 
            order = 1, data = df12)

#Source Code ----
#====Forecasting Spin-up
# # # n<-length(df);
# # timedomain<-df[1:(n-PeriodstoForecast)];
# # end<-length(timedomain);
# # #timedomain[end]=df[n]; #Deals with the null of last date
# # jjearnts <- complete.cases(ts(timedomain,deltat=1/12,start=c(startyear,1)));
# # fcast <- forecast(jjearnts, h=PeriodstoForecast[1]);
# # summary(fcast)
# #====Past+Future forecast
# #library(forecast)
# # n<-length(df);
# timedomain<-df[1:(n-PeriodstoForecast[1])]; #We have to make the shifted date in R to combine future and past.
# #THen you will need to use a date field in the opposing axis that shifts the last date you have N periods.
# #This data field is wat domain completion does.
# end<-length(timedomain);
# timedomain[end]=df[n]; #Deals with the null of last date
# jjearnts <- ts(timedomain,deltat=1/4,start=c(startyear,1));
# fcast <- forecast(jjearnts, h=PeriodstoForecast[1]);
# summary(fcast)
# print(fcast$mean)
# #append(u,fcast$mean, after = end)
# rm(end,jjearnts,timedomain)
# 
# #====Past+Future forecast
# #library(forecast)
# # n<-length(df);
# timedomain<-df[1:(n-PeriodstoForecast[1])]; #We have to make the shifted date in R to combine future and past.
# #THen you will need to use a date field in the opposing axis that shifts the last date you have N periods.
# #This data field is wat domain completion does.
# end<-length(timedomain);
# timedomain[end]=df[n]; #Deals with the null of last date
# time <- ts(timedomain,start=c(shiftedyear,1), frequency=myfrequency);
# fcast <- forecast(time, h=PeriodstoForecast[1]);
# append(u,fcast$mean, after = end)
# #==== Basic
# 
# #library(forecast)
# l<-length(df);
# u<-df[1:(l-PeriodstoForecast[1])];
# n<-length(u);
# u[n]=df[l];
# jjearnts <- ts(u,deltat=1/4,start=c(startyear,1));
# fcast <- forecast(jjearnts, h=PeriodstoForecast[1]);
# append(u,fcast$mean, after = n)
# 
# #==== Sum Things
# 
# # WINDOW_AVG(SCRIPT_REAL("
# #             outliers <- df
# #             totaloutliers <- unlist(lapply(X = outliers,FUN = sum))
# #             ",Sum([Profit])))
# 
# # SCRIPT_STR("
# #             outliers <- df
# #             totaloutliers <- unlist(lapply(X = outliers,FUN = sum))
# #             n <- unlist(lapply(X = outliers,FUN = length))
# #             total <- sum(totaloutliers)/sum(n)
# #             #ANSWER <- paste(total)
# #             ANSWER <- paste(total, collapse = ' ')
# #             ",[Outlier])

# # Author: Erinc Sonmezer                                   #
# # Edited: M Lane
# # Reason: To learn how to integrate R with =better
# # Setting Working Directory                                            #
# # Rule of thumb: Keep R Script & flat data files in the same directory #
# setwd("~/R")
# 
# # Investigating Data before reading and assigning to a data frame #
# 
# # Checking line count (number of rows)
# length(readLines('WDI_Data.csv'))
# 
# # Checking column names
# columns <- readLines('WDI_Data.csv',n = 1)
# dfColumns <- as.data.frame(strsplit(columns,","))
# colnames(dfColumns) <- "WDI Columns"
# 
# # Reading from csv file and assigning to a data frame #
# df1 <- read.csv(file='WDI_Data.csv',header=TRUE)
# 
# # Unique Indicators in csv file
# Indicators <- as.data.frame(unique(df1$Indicator.Name))
# colnames(Indicators) <- "Indicator Name"
# # These are potential variables for modeling
# # Supervised Learning calls for selection of dependent variable(s)
# # Let's try to predict CO2 emission for a given country
# # Which Indicators has CO2 in Indicator.Name?
# CO2_Indicators <- as.data.frame(Indicators[grep('CO2',Indicators$`Indicator Name`),1])
# # Dependent Variable: CO2 Emissions in kilotons
# df2 <- subset(df1, Indicator.Name %in% c('CO2 emissions (kt)'))
# # Observe df2 visually - Up to which year we have observations? 2011
# # Can we determine the 2011 programmatically? Yes - below is one way
# # An example: Is all values in column-3 NA?
# all(is.na(df2[,3]))
# # Now let's run this in a for loop for all columns
# # creating object to assign values - initiating
# df3 <- NA
# for(i in 1:length(df2)){df3[i] <- all(is.na(df2[,i]))}
# df3 <- as.data.frame(df3)
# colnames(df3) <- "All NA"
# df4 <- cbind(dfColumns,df3)
# subset(df4,df4[,2]==TRUE)
# 
# # Which country has High CO2 emmission as of 2011?
# # Let's sort data High to Low based on 2011
# df2 <- df2[with(df2,order(-df2$X2011,na.last = TRUE)),]
# # View(df2)
# # Please observe that data has clusters of countries
# # Among just countries China has the highest CO2 emission in 2011
# # Select all data for single country: China
# # Please observe that we switched back to original data df1!
# China <- subset(df1, Country.Name %in% c('China'))
# # How much memory r session uses? 8GB dual core, ~600 MB, for my laptop
# # At this point, if you are planning to work on a model just for China you can 
# # delete all other objects to free up some memory
# 
# # Deciding on Independent Variables #
# # We have 1420 variables (CO2 emissions (kt)--> Dependent)
# # How to decide on independent variables?
# # We can limit the variables based on number of valid observations
# # View(China)
# # Current dataframe Years as columns and Indicators on rows
# # Transpose dataframe so variables on columns and years on rows
# ChinaT <- data.frame(t(China[,5:56]))
# colnames(ChinaT) <- China$Indicator.Name
# 
# # Determine NAs per variable(Indicator)
# na_count <- sapply(ChinaT, function(y) sum(length(which(is.na(y)))))
# na_count <- data.frame(na_count)
# 
# # How Many NAs for depedent variable?
# na_count[rownames(na_count) == c("CO2 emissions (kt)"),]
# #None
# # Select variables with minimum NAs
# ChinaRattle <- ChinaT[,which(na_count<1)]
# 
# # Saving Cleaned China data (as csv) for rattle
# # write.csv(ChinaRattle,"China.csv")
# 
# # Removing variables with "of.GDP", "LCU", "2005","of.total" in Indicator Name #
# df5 <- subset(ChinaRattle,select = !grepl("of.GDP",colnames(ChinaRattle)))
# df6 <- subset(df5,select = !grepl("LCU",colnames(df5)))
# df7 <- subset(df6,select = !grepl("2005",colnames(df6)))
# df8 <- subset(df7,select = !grepl("of.total",colnames(df7)))
# df9 <- subset(df8,select = !grepl("etc",colnames(df8)))
# df10 <- subset(df9,select = !grepl("capita",colnames(df9)))
# 
# ChinaCorr <- data.frame(cor(df10, use="pairwise.complete.obs", method="pearson")) 
# # Which variables are highly correlated with dependent variable?
# colnames(ChinaCorr)
# HighCorrVariables <- rownames(ChinaCorr[abs(ChinaCorr$df$`Unit Sales`)>0.50,])
# # Creating dataframe for those variables
# df11 <- df10[,colnames(df10)%in%HighCorrVariables] 
# 
# # Saving further cleaned China data (as csv) for rattle
# write.csv(df11,"df.csv")
# 
# # Let's plot the final model with ggplot
# library(ggplot2)
# ggplot(ChinaRattle, aes(x=`Aquaculture production (metric tons)`, y=`CO2 emissions (kt)`)) +
#   geom_point(shape=1) + # Use hollow circles
#   geom_smooth(method=lm)# Add linear regression line
# 
# # Fitting model in R (No Data Partition)
# fit <- lm(`CO2 emissions (kt)` ~ `Aquaculture production (metric tons)`, data = ChinaRattle)
# summary(fit)
# 
# # Granger Causality Test
# # The Granger causality test is a statistical hypothesis test for determining 
# # whether one time series is useful in forecasting another
# if("lmtest" %in% rownames(installed.packages())==FALSE){install.packages("lmtest")}
# library(lmtest)
# # do Aquaculture production granger cause CO2 emmission, at lag t?
# # Try order 1 to 4
# grangertest(`CO2 emissions (kt)` ~ `Aquaculture production (metric tons)`, order = 4, data = ChinaRattle)
# 
# # Quick time-series plot 
# plot(ChinaRattle$`Aquaculture production (metric tons)`,type="l")
# lines(ChinaRattle$`CO2 emissions (kt)`,col="red")
# 
# df12 <- df11[-c(2,3,4,5,9,10,13,14,15,18,20,21,23,24,29,30,31,34,35,36,37,38,41,42,43,45)]
# write.csv(df12,"df.csv",row.names = FALSE)
# # Final Model for China based on rattle log #
# # Fitting model in R (No Data Partition)
# input <- c("Arms imports (SIPRI trend indicator values)", "GDP at market prices (current US$)", "Population, total")
# target  <- "CO2 emissions (kt)"
# ChinaFinalModelData <- df12[c(input,target)]
# # Fitting model in R (No Data Partition)
# fit <- lm(`CO2 emissions (kt)` ~ ., data=ChinaFinalModelData)
# summary(fit)
# 
# # Granger Causality Test
# # The Granger causality test is a statistical hypothesis test for determining 
# # whether one time series is useful in forecasting another
# if("lmtest" %in% rownames(installed.packages())==FALSE){install.packages("lmtest")}
# library(lmtest)
# # do "Arms.imports..SIPRI.trend.indicator.values." granger cause "CO2 emmission"
# # at lag t?
# # Try order 1 to 5
# grangertest(df$`Unit Sales` ~ Arms.imports..SIPRI.trend.indicator.values., 
#             order = 5, data = df12)
# 
# # do "Gross.national.expenditure..current.US.." granger cause "CO2 emmission"
# # at lag t?
# # Try order 1 to 5
# grangertest(df$`Unit Sales` ~ Gross.national.expenditure..current.US.., 
#             order = 5, data = df12)
# 
# # do "Population..total" granger cause "CO2 emmission"
# # at lag t?
# # Try order 1 to 5
# grangertest(df$`Unit Sales` ~ Population..total, 
#             order = 1, data = df12)

