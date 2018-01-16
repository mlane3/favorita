## from LOAD.R ----
#Because I could not figure out how to set up the docker/github side of things
#I have a github repo of my code though at instance that was http://35.203.163.187/ 
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
if("doParallel" %in% rownames(installed.packages())==FALSE){install.packages("doParallel",repos = repos,dependencies = TRUE)}
#if("SparkR" %in% rownames(installed.packages())==FALSE){install.packages("SparkR",repos = repos,dependencies = TRUE)}


# if("jsonlite" %in% rownames(installed.packages())==FALSE){install.packages("jsonlite",repos = repos,dependencies = TRUE)}
# if("ggplot2" %in% rownames(installed.packages())==FALSE){install.packages("ggplot2",repos = repos,dependencies = TRUE)}
# if("scales" %in% rownames(installed.packages())==FALSE){install.packages("scales",repos = repos,dependencies = TRUE)}
# if("grid" %in% rownames(installed.packages())==FALSE){install.packages("grid",repos = repos,dependencies = TRUE)}
# if("gridExtra" %in% rownames(installed.packages())==FALSE){install.packages("gridExtra",repos = repos,dependencies = TRUE)}
# if("RColorBrewer" %in% rownames(installed.packages())==FALSE){install.packages("RColorBrewer",repos = repos,dependencies = TRUE)}
# if("corrplot" %in% rownames(installed.packages())==FALSE){install.packages("corrplot",repos = repos,dependencies = TRUE)}
# if("tibble" %in% rownames(installed.packages())==FALSE){install.packages("tibble",repos = repos,dependencies = TRUE)}
# if("tidyr" %in% rownames(installed.packages())==FALSE){install.packages("tidyr",repos = repos,dependencies = TRUE)}
# if("stringr" %in% rownames(installed.packages())==FALSE){install.packages("stringr",repos = repos,dependencies = TRUE)}
# if("forcats" %in% rownames(installed.packages())==FALSE){install.packages("forcats",repos = repos,dependencies = TRUE)}
# if("ggfortify" %in% rownames(installed.packages())==FALSE){install.packages("ggfortify",repos = repos,dependencies = TRUE)}
# if("ggrepel" %in% rownames(installed.packages())==FALSE){install.packages("SparkR",repos = repos,dependencies = TRUE)}
# if("treemapify" %in% rownames(installed.packages())==FALSE){install.packages("treemapify",repos = repos,dependencies = TRUE)}
# if("ggforce" %in% rownames(installed.packages())==FALSE){install.packages("ggforce",repos = repos,dependencies = TRUE)}
# if("ggridges" %in% rownames(installed.packages())==FALSE){install.packages("ggridges",repos = repos,dependencies = TRUE)}
# if("broom" %in% rownames(installed.packages())==FALSE){install.packages("broom",repos = repos,dependencies = TRUE)}
# if("purrr" %in% rownames(installed.packages())==FALSE){install.packages("purrr",repos = repos,dependencies = TRUE)}
# if("lubridate" %in% rownames(installed.packages())==FALSE){install.packages("lubridate",repos = repos,dependencies = TRUE)}
# if("timeDate" %in% rownames(installed.packages())==FALSE){install.packages("timeDate",repos = repos,dependencies = TRUE)}
# if("xgboost" %in% rownames(installed.packages())==FALSE){install.packages("sparklyr",repos = repos,dependencies = TRUE)}

## LOAD any missing data ----
## Do this after you have loaded the packages (which takes 15-30 minutes)
## I usse /submission/ instead of /out/ to match other people's code (like lukes)
setwd("~/kagglefavorita")
library(googleAuthR)
library(googleCloudStorageR)
library(googleComputeEngineR)
library(data.table); library(doParallel); library(foreach)
library(readr); 

gcs_global_bucket("favoritadata")
options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/cloud-platform")
# googleAuthR::gar_gce_auth() #I could not get automated authorization to work 100%
gar_auth_service("Favorita-auth.json")


objects <- gcs_list_objects(bucket = "favoritadata")
objects <- subset(objects, grepl("favoritadata/itemNbrfixed/", id))
itemNbrlist <- gsub("itemNbrfixed/","",objects$name)


# gcs_get_object(objects$name[[1]], saveToDisk = paste("data/itemNbr/",itemNbr,sep=""), overwrite = TRUE)
lapply(1:nrow(objects),gcs_itemNbr <- function(i){
  itemNbr <- itemNbrlist[i]
  gcs_get_object(objects$name[[i]], saveToDisk = paste("data/itemNbr/",itemNbr,sep=""), overwrite = TRUE)
})
#Load Test.rds
gcs_get_object("data/testData.rds", saveToDisk = "data/testData.rds")
testDataDF <- readRDS("data/testData.rds")
## ACTUAL CODE ----
## MovingAverageModel.R
runAvg <- function()
{
  # testDataDF <- readRDS("data/testData.rds")
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
    
    #cat(i, ": ", average, "\n", sep="")
  }
  
  out.data <- avgDF
  colnames(out.data) <- c("id", "unit_sales")
  
  #write to output
  outfile <- paste0("data/submission/average/avg.csv")
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
  testDataDF <- readRDS("data/testData.rds")
  bugged <- testDataDF[c(94700,947100,947200,1052200,1052300,1052400,1052500,1101400,1101500,1101600,1101700,1183600,1183700,1183800,1183900,1378600,1378700,1568700,1746200,1746300,1746400)]
  # x = 1:2000
  out.data <- foreach(x = 1:10, .combine = rbind, .export=c("computeAvgP"), .packages = 'data.table', .verbose = TRUE) %dopar%
  {
    computeAvgP(bugged$id[x], as.character(bugged$date[x]), bugged$item_nbr[x], bugged$store_nbr[x])
  }
  
  stopCluster(cl)
  
  #remove rows with -9999
  out.data <- out.data[out.data$unit_sales >= 0, ]
  
  #write to output
  outfile <- paste0("data/submission/average/avg.csv")
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
  outfile <- paste0("data/submission/average/avg.csv")
  write.table(out.data, file=outfile, append=FALSE, sep=",", col.names=TRUE, row.names=FALSE, quote=FALSE)
}

computeAvgP <- function(rowID, date, itemNbr, storeNbr)
{

  #rowID = testDataDF$id[x];
  #mydate = testDataDF$date[x];
  #itemNbr = testDataDF$item_nbr[x];
  #storeNbr = testDataDF$store_nbr[x]
  date <- as.Date(date)
  
  fileNo <- itemNbr
  filename <- paste0("data/itemNbrfixed/", itemNbr, ".csv")
  
  if(file.exists(filename) == TRUE)
  {
    model.train <- fread(filename[1], sep=",", header=TRUE, verbose = FALSE) 
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