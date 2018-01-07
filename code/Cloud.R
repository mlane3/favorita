runWrapper <- function()
{
    models <- c("gaussian", "poisson", "nnet")    
    
    for(model in models)
    {
        run(model)
    }
}

run <- function(model="gaussian")
{
    require(data.table)
 
    testDataDF <- readRDS("data/testData.rds")
    
    for(i in 1:nrow(testDataDF))
    {
        print(i)
        testSID <- getSeasonalIndex(testDataDF$date[i])
        model.trainData <- getTrainingData(testDataDF$item_nbr[i], testDataDF$store_nbr[i], testSID)
        if(is.null(model.trainData))
        {
            next 
        }
        model.fit <- fitModel(model.trainData, model)
        
        forecastModel(model.fit, testDataDF[i, ], model)
    }
    
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
        model.train <- model.train[model.train$store_nbr == storeNbr & model.train$seasonal_ID==testSID, ]
        
        if(nrow(model.train) == 0)
        {
            return(NULL)
        }
        
        model.train <- prepareTrainingData(model.train)
        
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
    df$end_of_month <- as.logical(df$end_of_month)
    df$payday <- as.logical(df$payday)
    
    #make all -1 unit_sales (returns) equal to 0
    df$unit_sales[df$unit_sales<0] <- 0
    
    # code belows improves rsme for gaussian model, but will not be included at this time
    #if(modelType == 'gaussian')
    #{
    #    df$store_nbr <- as.factor(df$store_nbr) #this seems to improve the model
    #}
    
    if(modelType == 'nnet')
    {
        model.fit <- train(unit_sales ~ date+end_of_month+payday, data=df, method='nnet', trace=FALSE, linout=1)
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
    model.test <- testData[1, c(2, 3, 7, 8)]    #reorder to meet structure of training data
    
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

getSeasonalIndex <- function(vec)
{
    vec <- as.numeric(format(vec, "%m"))
    
    ifelse(vec %in% c(1, 2, 12), "SUMMER", ifelse(vec %in% c(3, 4, 5), "FALL", ifelse(vec %in% c(6, 7, 8), "WINTER", "SPRING")))
}