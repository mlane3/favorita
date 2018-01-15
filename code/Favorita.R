#Author: Rashaad Jones
instanceRunNN <- function(num=1)
{
    testFile <- paste0("data/", "testData", num, ".rds")
    outFile <- paste0("data/out/nnet/testData", num, ".csv")
    mainFavorita(readRDS(testFile), readRDS("data/newItempredictions.rds"), outFile, model="nnet")
}

combineOutputNN <- function()
{
    combineOutputG(outpath = "data/out/nnet", submitFile="data/out/nnet/avg.csv")
}

instanceRunG <- function(num=1)
{
    testFile <- paste0("data/", "testData", num, ".rds")
    outFile <- paste0("data/out/gaussian/testData", num, ".csv")
    mainFavorita(readRDS(testFile), readRDS("data/newItempredictions.rds"), outFile)
}

combineOutputG <- function(outpath="data/out/gaussian", submitFile="data/out/gaussian/avg.csv")
{
    files <- list.files(outpath, pattern="^testData\\d.*", full.names = TRUE)      
    
    #merge all outfiles
    for(file in files)
    {
        dataDF<-read.csv(file, header = TRUE, sep=",")
        if(file.exists(submitFile))
        {
            write.table(dataDF, file=submitFile, append=TRUE, sep=",", col.names=TRUE, row.names=FALSE, quote=FALSE)
        }
        else
        {
            write.table(dataDF, file=submitFile, append=FALSE, sep=",", col.names=TRUE, row.names=FALSE, quote=FALSE)
        }
    } 
}

mainFavoritaWrapper <- function()
{
    testDataDF <- readRDS("data/testData.rds")
    newItemsDF <- readRDS("data/newpItempredictions.rds")
    
    mainFavorita(testDataDF, newItemsDF)
}

mainFavorita <- function(testDataDF = NULL, newItemsDF = NULL, outfile=NULL, model="gaussian")
{
    require(data.table)
    require(foreach)
    
    #testDataDF <- readRDS("data/testData.rds")
    
    require(doParallel)
    noCores <- detectCores() - 1
    cl <- makeCluster(noCores)
    registerDoParallel(cl)
    #foreach(i=1:nrow(testDataDF)) %dopar%
    
    exportFun <- c("getTrainingData", "prepareTrainingData", "getSeasonalIndex", "fitModel", "forecastP")
    
    out.data <- foreach(i=1:nrow(testDataDF), .packages=c("caret"), .verbose=TRUE,
    #out.data <- foreach(i=1:10, .packages=c("caret"), .verbose=TRUE,
                             .combine=rbind,
                             .export=exportFun) %dopar%
    {
        testSID <- getSeasonalIndex(testDataDF$date[i])
        model.trainData <- getTrainingData(testDataDF$item_nbr[i], testDataDF$store_nbr[i], testSID)
        if(is.null(model.trainData))
        {
            if(is.null(newItemsDF)) #no unit_sales for this season
            {
                #newItemsDF <- readRDS("data/newpItempredictions.rds")
            }
            
            #data.frame(id=testDataDF$id[i], unit_sales=newItemsDF[which(newItemsDF$id == testDataDF$id[i]), "avg"])
            data.frame(id=testDataDF$id[i], unit_sales=0)
        }
        else
        {
            model.fit <- fitModel(model.trainData, model)
        
            forecastP(model.fit, testDataDF[i, ], model)
        }
        
    }
    
    stopCluster(cl)  
    
    #remove rows with -9999
    out.data <- out.data[out.data$unit_sales >= 0, ]
    
    #write to output
    if(is.null(outfile))
    {
        outfile <- paste0("data/out/gaussian/avg.csv")
    }
    
    write.table(out.data, file=outfile, append=FALSE, sep=",", col.names=TRUE, row.names=FALSE, quote=FALSE)
}

getTrainingData <- function(itemNbr, storeNbr, testSID)
{
    fileNo <- itemNbr
    filename <- paste0("data/itemNbr/", itemNbr, ".csv")
    
    if(file.exists(filename) == TRUE)
    {
        model.train <- read.csv(filename[1], sep=",", header=TRUE) 
        model.train$date <- as.Date(model.train$date)
        #model.train <- model.train[model.train$store_nbr == storeNbr, ]
        model.train$seasonal_ID <- getSeasonalIndex(model.train$date)
        model.trainSub <- model.train[model.train$store_nbr == storeNbr & model.train$seasonal_ID==testSID, ]
        
        #no unit_sales for this season
        if(nrow(model.trainSub) == 0)
        {
            return(NULL)
            #return(data.frame(id=-9999, date=-9999, store_nbr=storeNbr, item_nbr=itemNbr, unit_sales=0, onpromotion=NA))
        }
        
        model.train <- prepareTrainingData(model.trainSub)
        
        return(model.train)
    }
    else
    {
        return(NULL)
    }
}

prepareTrainingData <- function(dataDF)
{
    #dateSeq <-seq(as.Date("2013-01-01", "%Y-%m-%d"), as.Date("2017-08-15", "%Y-%m-%d"), by = "days")
    
    #df <- data.frame(date=dateSeq)
    df <- dataDF
    
    #rm(dateSeq)
    #gc()
    
    #df$seasonal_ID <- getSeasonalIndex(df$date)
    df$end_of_month <- ifelse(as.numeric(format(df$date, "%d")) > 15, TRUE, FALSE)
    df$payday <-  ifelse(as.numeric(format(df$date, "%d"))  %in% c(1, 2, 15, 16), TRUE, FALSE) 
    
    df$unit_sales <- 0
    for(i in 1:nrow(dataDF))
    { 
        day <- dataDF$date[i]
        df$unit_sales[df$date == day] <- dataDF$unit_sales[i]
    }
    
    df
}

fitModel <- function(df, modelType)
{
    #df$seasonal_ID <- as.factor(df$seasonal_ID)
    
    #make all -1 unit_sales (returns) equal to 0
    df$unit_sales[df$unit_sales<0] <- 0
    
   df$end_of_month <- as.logical(df$end_of_month)
   df$payday <- as.logical(df$payday)
   df$date <- as.Date(df$date)
    
    # code belows improves rsme for gaussian model, but will not be included at this time
    #if(modelType == 'gaussian')
    #{
    #    df$store_nbr <- as.factor(df$store_nbr) #this seems to improve the model
    #}
    
    if(modelType == 'nnet')
    {
        #df$date <- c(scale(df$date))
        df$end_of_month <- c(scale(df$end_of_month))
        df$payday <- c(scale(df$payday))
        df$date <- as.numeric(df$date)
        #model.fit <- train(unit_sales ~ date+end_of_month+payday, data=df, method='nnet', trace=FALSE, linout=1)
        model.fit <- train(unit_sales ~ date, data=df, method='nnet', trace=FALSE, linout=1)
    }
    else
    {
        model.fit <- glm(unit_sales ~ date+end_of_month+payday, data=df, family=modelType)
    }
    
    model.predictions <- predict(model.fit) 
    model.rmse <- sqrt(mean((model.predictions - df$unit_sales)^2))
    
    model.fit
    
    #Takes up ~ 40 MB per model
    #filename <- paste0("model/", df$item_nbr[1], "_", modelType, ".rds")
    #save(model.fit, file=filename)
}

forecastModel <- function(model.fit, testData, modelType)
{
    model.test <- testData[1, c(2, 7, 8)]    #reorder to meet structure of training data
        

    
    model.prediction <- predict(model.fit, newdata=model.test)
    
    if(model.prediction < 0)
    {
        model.prediction <- 0
    }
    
    out.data <- data.frame(id=testData[1, 1], unit_sales=model.prediction)
    outfile <- paste0("data/out/", modelType, "/gRun1.csv")
    #write.csv(out.data, file=outfile, append=TRUE, row.names = FALSE, quote=FALSE)
    if(file.exists(outfile) == FALSE)
    {
        write.table(out.data, file=outfile, sep=",", row.names = FALSE, quote=FALSE)
    }
    else
    {
        write.table(out.data, file=outfile, append=TRUE, sep=",", col.names = FALSE, row.names = FALSE, quote=FALSE)
    }
}

prepTestFile <- function()
{
    require(data.table)
    
    testDataDF <- fread("data/test.csv", sep = ",", header = TRUE)
    testDataDF$date <- as.Date(testDataDF$date)
    
    testDataDF$seasonal_ID <- getSeasonalIndex(testDataDF$date)
    
    testDataDF$end_of_month <- ifelse(as.numeric(format(testDataDF$date, "%d")) > 15, TRUE, FALSE)
    
    testDataDF$payday <- ifelse(as.numeric(format(testDataDF$date, "%d"))  %in% c(1, 2, 15, 16), TRUE, FALSE)
    
    write.csv(testDataDF, "data/testData.csv", row.names = FALSE, col.names = TRUE)
    saveRDS(testDataDF, "data/testData.rds")
}

runP <- function()
{
    testDataDF <- readRDS("data/testData.rds")
    
    library(foreach)
    library(doSNOW)
    
    cl <- makeCluster(3)
    registerDoSNOW(cl)
    
    numSplits <- 3
    
    test.split <- sort(rank(1:nrow(testDataDF))%%numSplits)
    
    exportFun <- c("createTrainDataP", "prepareTrainDataP", "getSeasonalIndex")
    
    predictionsDF <- foreach(i=unique(test.split), .packages=c("caret", "foreach"), .verbose=TRUE,
                             .combine=c,
                             .export=exportFun) %dopar%
    {
        model.trainData <- createTrainDataP(testDataDF$item_nbr[test.split==i])
        #if(is.null(model.trainData))
        #{
            #next 
        #}
        #model.fit <- fit(model.trainData, "gaussian")
        
        #data.frame(id=i, unit_sales=as.numeric(forecastP(model.fit, testDataDF[i, ], "gaussian")))
        #stop(model.trainData)
        model.trainData
    }
    
    stopCluster(cl)
    predictionsDF
}


run <- function()
{
    require(data.table)
    
    #testDataDF <- fread("data/test.csv", sep = ",", header = TRUE)
    #testDataDF$date <- as.Date(testDataDF$date)
    #
    #testDataDF$seasonal_ID <- getSeasonalIndex(testDataDF$date)
    #
    #testDataDF$end_of_month <- ifelse(as.numeric(format(testDataDF$date, "%d")) > 15, TRUE, FALSE)
    
    #testDataDF$payday <- ifelse(as.numeric(format(testDataDF$date, "%d"))  %in% c(1, 2, 15, 16), TRUE, FALSE) 
    
    testDataDF <- readRDS("data/testData.rds")
    
    for(i in 1:nrow(testDataDF))
    {
       print(i)
       model.trainData <- createTrainData(testDataDF$item_nbr[i])
       if(is.null(model.trainData))
       {
          next 
       }
       model.fit <- fit(model.trainData, "gaussian")
       #model.fit <- fitModel(model.trainData, c("gaussian", "poisson", "nnet"))
       
       forecast(model.fit, testDataDF[i, ], "gaussian")
       
       #return()
    }
    
    return(testDataDF)
}

forecastP <- function(model.fit, testData, modelType)
{
    model.test <- testData[1, c(2, 3, 6, 7, 8)]    #reorder to meet structure of training data
    
    model.test$end_of_month <- as.logical(model.test$end_of_month)
    model.test$payday <- as.logical(model.test$payday)
    #model.test$date <- as.Date(model.test$date)
    
    #model.test$date <- c(scale(model.test$date))
    #model.test$end_of_month <- c(scale(model.test$end_of_month))
    #model.test$payday <- c(scale(model.test$payday))
    
    if(modelType == "nnet")
    {
        model.test$date <- as.numeric(model.test$date)
    }
    
    model.prediction <- predict(model.fit, newdata=model.test)
    
    if(model.prediction < 0)
    {
        model.prediction <- 0
    }
    
    data.frame(id=testData$id, unit_sales=model.prediction)
}

forecast <- function(model.fit, testData, modelType)
{
    model.test <- testData[1, c(2, 3, 6, 7, 8)]    #reorder to meet structure of training data
    
    model.prediction <- predict(model.fit, newdata=model.test)
    
    if(model.prediction < 0)
    {
        model.prediction <- 0
    }
    
    out.data <- data.frame(id=testData[1, 1], unit_sales=model.prediction)
    outfile <- paste0("data/out/", modelType, "/gRun1.csv")
    #write.csv(out.data, file=outfile, append=TRUE, row.names = FALSE, quote=FALSE)
    if(file.exists(outfile) == FALSE)
    {
        write.table(out.data, file=outfile, sep=",", row.names = FALSE, quote=FALSE)
    }
    else
    {
        write.table(out.data, file=outfile, append=TRUE, sep=",", col.names = FALSE, row.names = FALSE, quote=FALSE)
    }
}

fit <- function(df, modelType)
{
    df$seasonal_ID <- as.factor(df$seasonal_ID)
    df$end_of_month <- as.logical(df$end_of_month)
    df$payday <- as.logical(df$payday)
    
    #if(modelType == 'poisson')
    #{
        #make all -1 unit_sales (returns) equal to 0
        df$unit_sales[df$unit_sales<0] <- 0
    #}
    
    # code belows improves rsme for gaussian model, but will not be included at this time
    #if(modelType == 'gaussian')
    #{
    #    df$store_nbr <- as.factor(df$store_nbr) #this seems to improve the model
    #}
    
    if(modelType == 'nnet')
    {
        model.fit <- train(unit_sales ~ date+seasonal_ID+end_of_month+payday+store_nbr, data=df, method='nnet', trace=FALSE, linout=1)
    }
    else
    {
        model.fit <- glm(unit_sales ~ date+seasonal_ID+end_of_month+payday+store_nbr, data=df, family=modelType)
    }
    #print(summary(model.fit))
    
    model.predictions <- predict(model.fit) 
    model.rmse <- sqrt(mean((model.predictions - df$unit_sales)^2))
    #print(model.rmse)
    
    model.fit
    
    #Takes up ~ 40 MB per model
    #filename <- paste0("model/", df$item_nbr[1], "_", modelType, ".rds")
    #save(model.fit, file=filename)
}

createTrainDataP <- function(itemNbr)
{
    fileNo <- itemNbr
    filename <- paste0("data/itemNbr/", itemNbr, ".csv")
    
    
    #model.trainCombined <- foreach(file=filename, .combine=rbind) %do% 
    model.trainCombined <- vector()
    for(file in filename) 
    {
        if(file.exists(file) == TRUE)
        {
            fullData <- read.csv(file, sep=",", header=TRUE) 
            for(store_nbr in 1:53)
            {
                model.train <- fullData[fullData$store_nbr == store_nbr, ]
                model.train$date <- as.Date(model.train$date)
                model.train <- prepareTrainDataP(model.train)
                
                model.trainCombined <- rbind(model.trainCombined, model.train)
            }
        
        }
    }
    
    return (model.trainCombined)
}

createTrainData <- function(itemNbr)
{
    fileNo <- itemNbr
    filename <- paste0("data/itemNbr/", itemNbr, ".csv")
    
    if(file.exists(filename) == TRUE)
    {
        model.train <- read.csv(filename[1], sep=",", header=TRUE) 
        #model.train <- model.train[model.train$seasonal_ID == "WINTER", ]
        model.train$date <- as.Date(model.train$date)
        model.train <- prepareTrainData(model.train)
        
        return(model.train)
    }
    else
    {
       return(NULL)
    }
}

prepareTrainDataP <- function(dataDF)
{
    dateSeq <-seq(as.Date("2013-01-01", "%Y-%m-%d"), as.Date("2017-08-15", "%Y-%m-%d"), by = "days")
    #dVec <- rep(dateSeq , 53 )       
    #dVec <- sort(dVec)
    
    #df <- data.frame(date=dVec, store_nbr=rep(1:53, length(dVec)/53))
    df <- data.frame(date=dateSeq) 
    
    df$seasonal_ID <- getSeasonalIndex(df$date)
    df$end_of_month <- ifelse(as.numeric(format(df$date, "%d")) > 15, TRUE, FALSE)
    df$payday <-  ifelse(as.numeric(format(df$date, "%d"))  %in% c(1, 2, 15, 16), TRUE, FALSE) 
    
    df$unit_sales <- 0
    for(i in 1:nrow(dataDF))
    { 
        day <- dataDF$date[i]
        df$unit_sales[df$date == day] <- dataDF$unit_sales[i]
    }
    
    df
}

prepareTrainData <- function(dataDF)
{
    dateSeq <-seq(as.Date("2013-01-01", "%Y-%m-%d"), as.Date("2017-08-15", "%Y-%m-%d"), by = "days")
    dVec <- rep(dateSeq , 53 )       
    dVec <- sort(dVec)
    
    df <- data.frame(date=dVec, store_nbr=rep(1:53, length(dVec)/53))
    
    rm(dateSeq, dVec)
    gc()
    
    df$seasonal_ID <- getSeasonalIndex(df$date)
    df$end_of_month <- ifelse(as.numeric(format(df$date, "%d")) > 15, TRUE, FALSE)
    df$payday <-  ifelse(as.numeric(format(df$date, "%d"))  %in% c(1, 2, 15, 16), TRUE, FALSE) 
    
    df$unit_sales <- 0
    for(i in 1:nrow(dataDF))
    { 
        store <- dataDF$store_nbr[i]
        day <- dataDF$date[i]
        df$unit_sales[df$store_nbr == store & df$date == day] <- dataDF$unit_sales[i]
    }
    
    df
}

getSeasonalIndex <- function(vec)
{
    vec <- as.numeric(format(vec, "%m"))
    
    ifelse(vec %in% c(1, 2, 12), "SUMMER", ifelse(vec %in% c(3, 4, 5), "FALL", ifelse(vec %in% c(6, 7, 8), "WINTER", "SPRING")))
}