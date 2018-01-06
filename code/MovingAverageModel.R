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

computeAvg <- function(date, itemNbr, storeNbr)
{
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