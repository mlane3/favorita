#Author: Rashaad Jones

instanceRun <- function(num=1, method="runAvgP")
{
    testFile <- paste0("data/", "testData", num, ".rds")
    outFile <- paste0("data/out/average/testData", num, "csv")
    runAvgP(readRDS(testFile), readRDS("data/newItempredictions.rds"), outFile)
}

combineOutput <- function(outpath="data/out/average", submitFile="data/out/average/avg.csv")
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

mainRun <- function(numSplits=4, outpath="data/out/average/", submitFile="data/out/average/avg.csv")
{
    files <- list.files("data/", pattern="^testData\\d.*", full.names = TRUE)      
    outfileVec <- vector()
    for(file in files)
    {
       outfile <- unlist(strsplit(file, "\\."))[1]
       outfile <- unlist(strsplit(outfile, "/"))[2]
       outfile <- paste0(outpath, outfile, ".csv")
       runAvgP(readRDS(file), outfile) 
       outfileVec <- c(outfileVec, outfile)
    }
    
    #merge all outfiles
    dataVec <- vector()
    for(file in outfileVec)
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

splitTestData <- function(numSplits=4)
{
    testDataDF <- readRDS("data/testData.rds")
    
    for(i in 1:numSplits)
    {
        start <- nrow(testDataDF) * (i-1)/numSplits + 1
        end <- start + nrow(testDataDF)/numSplits - 1
        testData <- testDataDF[start:end, ]
        outfile <- paste0("data/testData", i, ".rds")
        saveRDS(testData, outfile)
    }
}

runAvgP <- function(testDataDF = NULL, newItemDF = NULL, outfile=NULL)
{
    require(foreach)
    require(doParallel)
    
    noCores <- detectCores()
    cl <- makeCluster(noCores-1)
    registerDoParallel(cl)
    
    if(is.null(testDataDF))
    {
        testDataDF <- readRDS("data/testData.rds")
    }
    
    out.data <- foreach(x = 1:nrow[testDataDF], .combine = rbind, .export=c("computeAvgP"), .verbose = TRUE) %dopar%
            {
                 computeAvgP(testDataDF$id[x], as.character(testDataDF$date[x]), testDataDF$item_nbr[x], testDataDF$store_nbr[x], newItemsDF)
            }
    
    stopCluster(cl)
    
    #remove rows with -9999
    out.data <- out.data[out.data$unit_sales >= 0, ]
    
    #write to output
    if(is.null(outfile))
    {
        outfile <- paste0("data/out/average/avg.csv")
    }
    
    write.table(out.data, file=outfile, append=FALSE, sep=",", col.names=TRUE, row.names=FALSE, quote=FALSE)
}

runAvg <- function()
{
    testDataDF <- readRDS("data/testData.rds")
    testDataDF$date <- as.Date(testDataDF$date)
    
    avgDF <- vector()
    for(i in 1:nrow(testDataDF))
    {
        store <- testDataDF$store_nbr[i]
        item <- testDataDF$item_nbr[i]
        date <- testDataDF$date[i]
        
        average <- computeAvg(date, item, store)
        if(is.null(average))
        {
            next 
        }
        
        tmpDF <- data.frame(testDataDF[1, 1], average)
        avgDF <- rbind(avgDF, tmpDF)
        
        cat(i, ": ", average, "\n", sep="")
    }
    
    out.data <- avgDF
    colnames(out.data) <- c("id", "unit_sales")
    
    #write to output
    outfile <- paste0("data/out/average/avg.csv")
    write.table(out.data, file=outfile, append=FALSE, sep=",", col.names=TRUE, row.names=FALSE, quote=FALSE)
}

computeAvgP <- function(rowID, date, itemNbr, storeNbr, newItemsDF)
{
    date <- as.Date(date)
    
    fileNo <- itemNbr
    filename <- paste0("data/itemNbr/", itemNbr, ".csv")
    
    if(file.exists(filename) == TRUE)
    {
        model.train <- read.csv(filename[1], sep=",", header=TRUE) 
        model.train$date <- as.Date(model.train$date)
        model.train <- model.train[model.train$store_nbr == storeNbr, ]
        
        if(nrow(model.train) == 0)
        {
            return(data.frame(id=rowID, unit_sales=-9999))
        }
        
        month <- as.numeric(format(date, "%m"))
        day <- as.numeric(format(date, "%d"))
        
        model.train <- model.train[as.numeric(format(model.train$date, "%m")) == month &
                                       as.numeric(format(model.train$date, "%d")) == day, ]
        
        if(nrow(model.train) == 0)
        {
            return(data.frame(id=rowID, unit_sales=-9999))
        }
        
        model.train$unit_sales[model.train$unit_sales < 0] <- 0
        
        return(data.frame(id=rowID, unit_sales=mean(model.train$unit_sales)))
    }
    else #for new_items
    {
        #newItemsDF <- read.csv("data/newpItempredictions.csv", header = TRUE, sep=",")
        if(is.null(newItemsDF))
        {
            newItemsDF <- readRDS("data/newpItempredictions.rds")
        }
        
        unitSales <- newItemsDF[newItemsDF$id == rowID, "avg"]
        return(data.frame(id=rowID, unit_sales=unitSales))
    }
    
}

computeAvg <- function(date, itemNbr, storeNbr)
{
    date <- as.Date(date)
    
    fileNo <- itemNbr
    filename <- paste0("data/itemNbr/", itemNbr, ".csv")
    
    if(file.exists(filename) == TRUE)
    {
        model.train <- read.csv(filename[1], sep=",", header=TRUE) 
        model.train$date <- as.Date(model.train$date)
        model.train <- model.train[model.train$store_nbr == storeNbr, ]
        
        if(nrow(model.train) == 0)
        {
            return(0)
        }
        
        month <- as.numeric(format(date, "%m"))
        day <- as.numeric(format(date, "%d"))
        
        model.train <- model.train[as.numeric(format(model.train$date, "%m")) == month &
                                                  as.numeric(format(model.train$date, "%d")) == day, ]
        
        if(nrow(model.train) == 0)
        {
            return(0)
        }
        
        model.train$unit_sales[model.train$unit_sales < 0] <- 0
        
        return(mean(model.train$unit_sales))
    }
    else
    {
        return(NULL)
    }
}

runMA <- function()
{
    trainingData <- read.csv("data/itemNbr/96995.csv", sep=",", header=TRUE)

    trainingData$date <- as.Date(trainingData$date)

    trainingData$unit_sales[trainingData$unit_sales<0] <- 0

    trainTS <- ts(trainingData$unit_sales, frequency=1, start=c(2013, 1))

    sma <- data.frame(date=trainingData$date, unit_sales=SMA(trainTS))
    
    sma
}