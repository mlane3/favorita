#NOTE: Code is for demonstration purposes only.  Code is not yet reproducible and is not stable
#       A cleaner version will be available in the near future.

buildbatch3 <- function()
{
    require(caret)
    
    dateSeq <-seq(as.Date("2013-01-01", "%Y-%m-%d"), as.Date("2017-08-15", "%Y-%m-%d"), by = "days")
    dVec <- rep(dateSeq , 53 )       
    dVec <- sort(dVec)
    
    df <- data.frame(date=dVec, store_nbr=rep(1:53, length(dVec)/53))
    
    rm(dateSeq, dVec)
    gc()
    
    df$seasonal_ID <- apply(as.data.frame(df$date), 1, function(x) getSeasonalIndex(as.Date(x)))
    df$end_of_month <- apply(as.data.frame(df$date), 1, function(x) ifelse(as.numeric(format(as.Date(x), "%d")) > 15, TRUE, FALSE))
    df$payday <- apply(as.data.frame(df$date), 1, function(x) ifelse(as.numeric(format(as.Date(x), "%d")) 
                                                                     %in% c(1, 2, 15, 16), TRUE, FALSE))
    
    fileList <- list.files("rawdata/itemNbr/", full.names = TRUE)
    #finalDF <- vector()
    for(file in fileList) #TODO: Needs to be replaced with a way to find a particular file
    {
        #gets the item_nbr from the filename
        df$item_nbr<-unlist(strsplit(basename(file), "\\."))[1]
        
        df$unit_sales <- 0
        
        dataDF <- read.csv(file, sep=",", header=TRUE)
        dataDF$date <- as.Date(dataDF$date)
        
        for(i in 1:nrow(dataDF))
        { 
            store <- dataDF$store_nbr[i]
            day <- dataDF$date[i]
            df$unit_sales[df$store_nbr == store & df$date == day] <- dataDF$unit_sales[i]
        }
        
        
        # fitModel(df, "gaussian")
        # fitModel(df, "poisson")
        # fitModel(df, "nnet")
        
        return() 
    }
}

fitModel <- function(df, modelType)
{
    df$seasonal_ID <- as.factor(df$seasonal_ID)
    df$end_of_month <- as.logical(df$end_of_month)
    df$payday <- as.logical(df$payday)
        
    if(modelType == 'poisson')
    {
        #make all -1 unit_sales (returns) equal to 0
        df$unit_sales[df$unit_sales<0] <- 0
    }
    if(modelType == 'gaussian')
    {
        df$store_nbr <- as.factor(df$store_nbr) #this seems to improve the model
    }
    
    if(modelType == 'nnet')
    {
        model.fit <- train(unit_sales ~ date+seasonal_ID+end_of_month+payday+store_nbr, data=df, method='nnet', trace=FALSE, linout=1)
    }
    else
    {
        model.fit <- glm(unit_sales ~ date+seasonal_ID+end_of_month+payday+store_nbr, data=df, family=modelType)
    }
    print(summary(model.fit))
    
    model.predictions <- predict(model.fit) 
    model.rmse <- sqrt(mean((model.predictions - df$unit_sales)^2))
    print(model.rmse)
    
    #Takes up ~ 40 MB per model
    #filename <- paste0("model/", df$item_nbr[1], "_", modelType, ".rds")
    #save(model.fit, file=filename)
}

getSeasonalIndex <- function(vec)
{
    vec <- as.numeric(format(vec, "%m"))
    
    ifelse(vec %in% c(1, 2, 12), "SUMMER", ifelse(vec %in% c(3, 4, 5), "FALL", ifelse(vec %in% c(6, 7, 8), "WINTER", "SPRING")))
}

getEndofMonthFlag <- function(vec)

buildbatch2 <- function()
{
    library(dplyr)
    fileList <- list.files("rawdata/itemNbr/", full.names = TRUE)
    finalDF <- vector()
    for(file in fileList) #TODO: Needs to be replaced with a way to find a particular file
    {
        file="data/itemNbr/103665.csv"
        #open file to dataframe
        df <- read.csv(file, sep=",", header=TRUE)
        
        #convert $date to date object
        df$date <- as.Date(df$date)
        
        dfNew <- mutate(df, seasonalID=getSID(df$date), endOfMonth=getEndOfMonth(df$date), payDay=getPayDay(df$date))
        dfNew <- dfNew[c("id", "date", "seasonalID", "endOfMonth", "payDay", "store_nbr", "item_nbr", "unit_sales")]
        
        #dfNew$endOfMonth <- as.logical(dfNew$endOfMonth)
        #dfNew$payDay <- as.logical(dfNew$payDay)
        #dfNew$seasonalID <- as.factor(dfNew$seasonalID)
        
        model.data <- dfNew
        library(caret)
        #prepare training
        #trainIndex <- createDataPartition(model.data$id, p=.7, list=TRUE)
        trainIndex <- sample(1:nrow(model.data), round(0.7*nrow(model.data)))
        
        model.train <- model.data[trainIndex, ]
        model.test <- model.data[-trainIndex, ]
        
        print(model.train)
        #gPlot <- ggplot(model.train, aes(x=model.train$date, y=model.train$unit_sales, color=model.train$seasonalID)) 
        #gPlot <- ggplot(model.train, aes(x=date, y=unit_sales, group=store_nbr, color=store_nbr)) 
        #gPlot <- gPlot + geom_line() + geom_point(size=5)
        gPlot <- ggplot(model.train, aes(x=date, y=unit_sales, color=model.train$seasonalID)) 
        gPlot <- gPlot + facet_grid(store_nbr ~.) + geom_line() 
        print(gPlot)
        
        #boxplot(unit_sales ~ store_nbr, data=model.train)
        
        #model.fit <- glm(unit_sales ~ date+seasonalID+endOfMonth+payDay+store_nbr, data=model.train)
        model.fit <- glm(unit_sales ~ date+seasonalID+endOfMonth+store_nbr, data=model.train)
        print(summary(model.fit))
        #model.predictions <- predict(model.fit)
        model.predictions <- predict(model.fit, newdata = model.test)
        
        #model.fit <- train(unit_sales ~ seasonalID+endOfMonth+payDay+store_nbr, data=model.train, method='nnet', trace=FALSE, linout=TRUE)
        #print(model.fit)
        #model.predictions <- predict(model.fit, newdata = model.test)
        
        model.rmse <- sqrt(mean((model.predictions - model.test$unit_sales)^2))
        print(model.rmse)
        
        #nn
        model.fit <- train(unit_sales ~ seasonalID+endOfMonth+payDay+store_nbr, data=model.train, method='nnet', trace=FALSE, linout=TRUE)
        print(model.fit)
        model.predictions <- predict(model.fit, newdata = model.test)
        
        model.rmse <- sqrt(mean((model.predictions - model.test$unit_sales)^2))
        print(model.rmse)
        return()
    }
}

getPayDay <- function(dateVec)
{
   pdFlag <- vector() 
   for(i in 1:length(dateVec))
   {
       d <- as.numeric(format(dateVec[i], "%d"))
       if(d %in% c(1, 15))
       {
           pdFlag[i] <- TRUE
       }
       else
           pdFlag[i] <- FALSE
   }
   
   pdFlag
}

getEndOfMonth <- function(dateVec)
{
    eomFlag <- vector()
    for(i in 1:length(dateVec))
    {
        d <- as.numeric(format(dateVec[i], "%d"))
        if(d > 15)
        {
            eomFlag[i] <- TRUE
        }
        else
            eomFlag[i] <- FALSE
    }
    
    eomFlag
}

getSID <- function(dateVec)
{
    sID <- vector()
    for(i in 1:length(dateVec))
    {
        m <- as.numeric(format(dateVec[i], "%m"))
        if(m %in% c(1, 2, 12)) #SUMMER
        {
            sID[i] <- "SUMMER"
        }
        if(m %in% c(3, 4, 5)) #FALL
        {
            sID[i] = "FALL"
        }
        if(m %in% c(6, 7, 8)) #WINTER
        {
            sID[i] = "WINTER"
        }
        if(m %in% c(9, 10, 11)) #SPRING
        {
            sID[i] = "SPRING"
        }
    }
    
    sID
}


buildbatch <- function() #does not generate the required model data will remove
{
    fileList <- list.files("data/itemNbr/", full.names = TRUE)
    finalDF <- vector()
    for(file in fileList)
    {
        #open file to dataframe
        df <- read.csv(file, sep=",", header=TRUE)
        
        #convert $date to date object
        df$date <- as.Date(df$date)
        
        #break into years
        
        #for the first year
        year = 2013:2017
        for(y in year)
        {
            month = 1:12
            recDF <- vector()
            for(m in month)
            {
                day = 1:15
                for(d in day)
                {
                    rec <- df[as.numeric(format(df$date, "%d"))==d & as.numeric(format(df$date, "%Y")) == y & as.numeric(format(df$date, "%m")) == m ,  ]
                    if(nrow(rec) == 0)
                    {
                        next
                    }
                
                    recDF <- rbind(recDF, rec)
            
                    if(m %in% c(1, 2, 12)) #WINTER
                    {
                        sID = 1
                    }
                    if(m %in% c(3, 4, 5)) #SPRING
                    {
                        sID = 2
                    }
                    if(m %in% c(6, 7, 8)) #SUMMER
                    {
                        sID = 3
                    }
                    if(m %in% c(9, 10, 11)) #FALL
                    {
                        sID = 4
                    }
                    
                
                    storeArry <- tapply(recDF$unit_sales, recDF$store_nbr, FUN=sum)
                    storeSalesVec <- vector()
                    for(name in unlist(dimnames(storeArry)))
                    {
                        name <- as.numeric(name)
                        storeSalesVec <- rbind(storeSalesVec, name)
                    }
                    storeItemVec <- vector()
                    for(item in storeArry)
                    {
                        storeItemVec <- rbind(storeItemVec, item) 
                    }
                
                    storeSalesVec <- cbind(storeSalesVec, storeItemVec)
                    rownames(storeSalesVec) <- c()
                    storeSalesVec <- as.data.frame(storeSalesVec)
                    colnames(storeSalesVec) <- c("store_nbr", "unit_sales_sum")
                
                    for(j in 1:nrow(storeSalesVec))
                    {
                        recSum <- cbind(df$id, df$date, df$item_nbr[1], storeSalesVec$store_nbr[j], y, sID, FALSE, storeSalesVec$unit_sales_sum[j])
                        finalDF <- rbind(finalDF, recSum)
                    }
                    
                    return(finalDF)
            
                }
            
                day = 16:31
                for(d in day)
                {
                    rec <- df[as.numeric(format(df$date, "%d"))==d & as.numeric(format(df$date, "%Y")) == y & as.numeric(format(df$date, "%m")) == m ,  ]
                
                    recDF <- rbind(recDF, rec)
                }
            
                recSum <- cbind(df$item_nbr[1], y, sID, TRUE, sum(recDF$unit_sales))
            
                finalDF <- rbind(finalDF, recSum)
            }
        
        }
        
        colnames(finalDF) <- c("item_nbr", "year", "seasonalIndex", "end_of_month", "unit_sales_sum")
        finalDF <- as.data.frame(finalDF)
        return(finalDF)
    }
}

build <- function()
{
    df <- read.csv("rawdata/itemNbr/96995.csv", sep=",", header=TRUE)
    df$date <- as.Date(df$date)
    
    month = 5:8
    subDF <- vector()
    for(i in month)
    {
        sub <- df[as.numeric(format(df$date, "%m"))==i, ]
        
        subDF <- rbind(subDF, sub)
    }
    
    day= 16:31
    finalDF <- vector()
    for(i in day)
    {
        sub <- subDF[as.numeric(format(subDF$date, "%d"))==i, ]
        
        finalDF <- rbind(finalDF, sub) 
    }
    
    return(finalDF)
    
    model.data <- finalDF
    
    library(caret)
    #prepare training
    #trainIndex <- createDataPartition(model.data$id, p=.7, list=TRUE)
    trainIndex <- sample(1:nrow(model.data), round(0.7*nrow(model.data)))
    
    model.train <- model.data[trainIndex, ]
    model.test <- model.data[-trainIndex, ]
    
    model.fit <- train(unit_sales ~ store_nbr, data=model.train, method='nnet', trace=FALSE, linout=1)
    
    model.predictions <- predict(model.fit, newdata = model.test)
    
    #just to see (maybe log later)
    #model.rmse <- sqrt(mean((model.predictions - model.test$sum_of_unit_sales)^2))
    #model.rmse <- computeRMSE(model.predictions, model.test$unit_sales)
}
