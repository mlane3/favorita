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

## I usse /submission/ instead of /out/ to match other people's code (like lukes)
# setwd("~/kagglefavorita")
library(googleAuthR)
library(googleCloudStorageR)
library(googleComputeEngineR)
library(data.table)
library(readr)
library(doParallel)
library(foreach)


#LOAD ----
# gcs_global_bucket("favoritadata")
# options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/cloud-platform")
# googleAuthR::gar_gce_auth() #I could not get automated authorization to work 100%
# gar_auth_service("Favorita-auth.json")


objects <- gcs_list_objects(bucket = "favoritadata")
objects <- subset(objects, grepl("favoritadata/itemNbrfixed/", id))
itemNbrlist <- gsub("itemNbrfixed/","",objects$name)


# gcs_get_object(objects$name[[1]], saveToDisk = paste("data/itemNbr/",itemNbr,sep=""), overwrite = TRUE)
# lapply(1:nrow(objects),gcs_itemNbr <- function(i){
#   itemNbr <- itemNbrlist[i]
#   gcs_get_object(objects$name[[i]], saveToDisk = paste("data/itemNbr/",itemNbr,sep=""), overwrite = TRUE)
# })
# 
# gcs_get_object("data/testData1.rds", saveToDisk = "data/testData1.rds")
# gcs_get_object("data/testData2.rds", saveToDisk = "data/testData2.rds")
# gcs_get_object("data/testData3.rds", saveToDisk = "data/testData3.rds")
# gcs_get_object("data/testData4.rds", saveToDisk = "data/testData4.rds")
# gcs_get_object("newItempredictions.rds", saveToDisk = "data/newItempredictions.rds")
testDataDF <- readRDS("data/testData.rds")
testDataDF1 <- readRDS("data/testData1.rds")
testDataDF2 <- readRDS("data/testData2.rds")
testDataDF3 <- readRDS("data/testData3.rds")
testDataDF4 <- readRDS("data/testData4.rds")
newItemsDF <- readRDS("data/newItempredictions.rds")