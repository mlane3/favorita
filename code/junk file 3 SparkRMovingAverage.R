### Spark R script
## Load ----
# 
# Run $ sudo R --vanilla
dir.create(Sys.getenv("R_LIBS_USER"), showWarnings = FALSE, recursive = TRUE)
if("lubridate" %in% rownames(installed.packages())==FALSE){install.packages("lubridate",repos = repos,dependencies = TRUE)}
repos <- 'https://mirrors.nics.utk.edu/cran/'
Sys.getenv("R_LIBS_USER")
mypackages = c("lubridate","smooth","forecast","data.table","dplyr","googleAuthR","googleComputeEngineR","caret","doParallel","caret","TTR")
if(mypackages[i] %in% rownames(installed.packages())==FALSE)
{
  install.packages(mypackages[i],repos = repos,dependencies = TRUE)
}



# spark.read.format("csv").options(header='true', inferSchema='true').load("gs://favoritadata/test.csv"
temp <- read.df("gs://favoritadata/test.csv",source = "csv", header="true", inferSchema = "true")


## execute smooth moving average ----



runMA <- function()
{
  # require(TTR)
  trainingData <- read.df("gs://favoritadata/test.csv",source = "csv", header="true", inferSchema = "true")
  
  trainingData$date <- as.Date(trainingData$date)
  
  trainingData$unit_sales[trainingData$unit_sales<0] <- 0
  
  trainTS <- ts(trainingData$unit_sales, frequency=1, start=c(2013, 1))
  
  sma <- data.frame(date=trainingData$date, unit_sales=SMA(trainTS))
  
  sma
}




### Merge ----