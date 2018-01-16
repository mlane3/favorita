## from LOAD.R ----
#Because I could not figure out how to set up the docker/github side of things
#I have a github repo of my code though at
# instance is http://35.203.163.187/
repos <- 'https://mirrors.nics.utk.edu/cran/'
# if("lubridate" %in% rownames(installed.packages())==FALSE){install.packages("lubridate",repos = repos,dependencies = TRUE)}
# if("lmtest" %in% rownames(installed.packages())==FALSE){install.packages("lmtest",repos = repos,dependencies = TRUE)}
# if("smooth" %in% rownames(installed.packages())==FALSE){install.packages("smooth",repos = repos,dependencies = TRUE)}
if("forecast" %in% rownames(installed.packages())==FALSE){install.packages("forecast",repos = repos,dependencies = TRUE)}
if("data.table" %in% rownames(installed.packages())==FALSE){install.packages("data.table",repos = repos,dependencies = TRUE)}
if("dplyr" %in% rownames(installed.packages())==FALSE){install.packages("dplyr",repos = repos,dependencies = TRUE)}
# if("googleAuthR" %in% rownames(installed.packages())==FALSE){install.packages("googleAuthR",repos = repos,dependencies = TRUE)}
# if("googleComputeEngineR" %in% rownames(installed.packages())==FALSE){install.packages("googleComputeEngineR",repos = repos,dependencies = TRUE)}
# if("googleCloudStorageR" %in% rownames(installed.packages())==FALSE){install.packages("googleCloudStorageR",repos = repos,dependencies = TRUE)}
#if("caret" %in% rownames(installed.packages())==FALSE){install.packages("caret",repos = repos,dependencies = TRUE)}
# if("R6" %in% rownames(installed.packages())==FALSE){install.packages("R6",repos = repos,dependencies = TRUE)}
if("doParallel" %in% rownames(installed.packages())==FALSE){install.packages("doParallel",repos = repos,dependencies = TRUE)}
#if("SparkR" %in% rownames(installed.packages())==FALSE){install.packages("SparkR",repos = repos,dependencies = TRUE)}

library(data.table)
library(readr)
library(doParallel)
library(foreach)

testDataDF <- readRDS("data/testData.rds")
## NEW CODE ----
runAvgP <- function()
{
  require(foreach)
  require(doParallel)
  require(data.table)
  noCores <- detectCores()
  cl <- makeCluster(noCores - 1)
  registerDoParallel(cl)
  # summary(cl)
  testDataDF <- chunk1 #readRDS("data/testData.rds")
  # nrow(testDataDF)
  out.data <- foreach(x = 1:nrow(testDataDF), .combine = rbind, .export=c("computeAvgP","testDataDF"), .packages = 'data.table', .verbose = TRUE) %dopar%
  {
    computeAvgP(testDataDF$id[x], as.character(testDataDF$date[x]), testDataDF$item_nbr[x], testDataDF$store_nbr[x])
  }
  
  stopCluster(cl)
  out.dataall <- out.data #for testing
  
  #remove rows with -9999
  out.data <- out.data[out.data$unit_sales >= 0, ]
  
  #write to output
  outfile <- paste0("data/submission/average/avg.csv")
  write.table(out.data, file=outfile, append=FALSE, sep=",", col.names=TRUE, row.names=FALSE, quote=FALSE)
  return(out.dataall)
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
  outfile <- paste0("data/submission/average/avg.csv")
  write.table(out.data, file=outfile, append=FALSE, sep=",", col.names=TRUE, row.names=FALSE, quote=FALSE)
}

computeAvgP <- function(rowID, date, itemNbr, storeNbr)
{
  #rowID = testDataDF$id[x]
  #mydate = testDataDF$date[x]
  #itemNbr = testDataDF$item_nbr[x]
  #storeNbr = testDataDF$store_nbr[x]
  #date <- as.Date(date)
  
  fileNo <- itemNbr
  filename <- paste0("data/itemNbr/", itemNbr, ".csv")
  
  if(file.exists(filename) == TRUE)
  {
    model.train <- fread(filename[1], sep=",", header=TRUE,
                         verbose = FALSE) 
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
    return(data.frame(id=rowID, unit_sales=-9999))
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


#### RUN ----
# ptm <- proc.time()
# myMA <- runMA()
# print(myMA)
# A <- proc.time() - ptm; print(A);
ptm <- proc.time()
myAvg <- runAvgP()
B <- proc.time() - ptm; print(B)
# 