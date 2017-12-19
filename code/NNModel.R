
library(caret)

#main caller function
run <- function()
{
    runNN()
}

trainNN <- function(trainDataFile, modelOutFile, trainingType="none", predictors=NULL)
{
    model.data <- read.csv(trainDataFile, sep=",", header=TRUE) #todo: Error handling
    
    if(identical(trainingType, "none"))
    {
        model.train <- model.data
    }
    
    if(is.null(predictors) == TRUE)
    {
        model.fit <- train(sum_of_unit_sales ~ id + seasonalIndex, data=model.train, method='nnet', trace=FALSE, linout=1)
    }
    
    saveRDS(model.fit, modelOutFile)
}

predictNN <- function(modelFile, testFile, outFile, seasonalIndex=1)
{
    model.fit <- readRDS(modelFile)
    
    data <- read.csv(testFile, sep=",", header=TRUE)
    
    #create dataframe with relevant predictors 
    seasonalIndexVector <- rep(seasonalIndex, nrow(data))
    model.test <- data.frame(data$ID, seasonalIndexVector)
    
    model.predictions <- predict(model.fit, newdata=model.test)
    
    submit.predictions <- ifelse(submit.predictions<=0, 0, submit.predictions)
    submit.out <- data.frame(submit.data$id, submit.predictions)
    names(submit.out) <- c("id", "unit_sales")
    
    write.csv(submit.out, file=outFile, sep="," )
}

runNN <- function()
{
    #read data
    model.data <- read.csv("../data/Model.csv", sep=",", header=TRUE)
    
    #prepare training
    trainIndex <- createDataPartition(model.data$id, p=.7, list=TRUE)
    
    model.train <- model[trainIndex, ]
    model.test <- model[-trainIndex, ]
    
    #FUTURE: Use cross validation (e.g., k-folds)
    
    #model.fit <- train(sum_of_unit_sales ~ id, data=model.train, method='nnet', trace=FALSE, linout=1)
    
    #FUTURE: Get the seasonalIndex from the id
    model.fit <- train(sum_of_unit_sales ~ id + seasonalIndex, data=model.train, method='nnet', trace=FALSE, linout=1)
    
    model.predictions <- predict(model.fit, newdata = model.test)

    #just to see (maybe log later)
    #model.rmse <- sqrt(mean((model.predictions - model.test$sum_of_unit_sales)^2))
    model.rmse <- computeRMSE(model.predictions, model.test$sum_of_unit_sales)
    cat("RMSE for training.csv:", model.rmse, sep=" ")
    
    #USING THE MODEL ON THE TEST DATA
    submit.data <- read.csv("../data/test.csv", sep=",", header=TRUE)
    submit.predictions <- predict(model.fit, newdata=submit.data)
    submit.rmse <- computeRMSE(model.predictions, model.test$sum_of_unit_sales)
    cat("RMSE for testing.csv:", submit.rmse, sep=" ")
    
    submit.predictions <- ifelse(submit.predictions<=0, 0, submit.predictions)
    submit.out <- data.frame(submit.data$id, submit.predictions)
    names(submit.out) <- c("id", "unit_sales")
    
    write.csv(submit.out, file="../data/out/submission_nnBasic.csv", sep="," )
}

computeRMSE <- function(predictions, observations)
{
    sqrt(mean((predictions - observations)^2))
}
