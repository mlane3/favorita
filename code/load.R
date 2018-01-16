### Do NOT POST PUBLICALLY!
## install libraries -----
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
if("jsonlite" %in% rownames(installed.packages())==FALSE){install.packages("jsonlite",repos = repos,dependencies = TRUE)}
if("SparkR" %in% rownames(installed.packages())==FALSE){install.packages("SparkR",repos = repos,dependencies = TRUE)}
library(googleComputeEngineR)
library(googleCloudStorageR)
library(googleAuthR)
## to load -----

#Define storage location

## Authorize vm
options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/cloud-platform")
# googleAuthR::gar_gce_auth() #I could not get automated authorization to work 100%
gce_auth() #This will open a sign in page where you just need to sign in to authorize
gar_auth("Favorita-auth.json")
## use the storage to download the auth files for your API
auth_file <- "Favorita-auth.json"
gcs_get_object(auth_file, bucket = gcs_get_global_bucket(), saveToDisk = "Favorita-auth.json", overwrite = TRUE)

gce_global_zone("us-west1-b")
gce_global_zone("us-central1-c")
gce_get_project()
#This is creates the virtual instance
vm <- gce_vm(template = "rstudio",
             name = "my-rstudio", username = "kaggle", password = "data",
             predefined_type = "n1-standard-2")
vm <- gce_vm(project = "favorita-190722", template = "rstudio",
                    name = "gpu3", username = "kaggle", password = "data",
                    predefined_type = "n1-standard-8", scheduling = list(preemptible = TRUE))
##To Create: after running go to the ip address it lists
#image_family="ubuntu-1704-zesty-v20171208", image_project = "favorita-190722"
# gcs_load(bucket = "")
# gce_set_metadata(list(GCS_SESSION_BUCKET = "favoritadata"), vm)

##THE ONLY RULE:HOW TO STOP ----
gce_vm_stop("name of virtual machine")

gce_vm_stop("test")
## To relaunch ----- 

#FIRST: rerun the install libraries code
library(googleComputeEngineR)
library(googleCloudStorageR)
library(googleAuthR)
library(data.table)
library(readr)
#Define storage location
gcs_global_bucket("favoritadata")
options(googleAuthR.scopes.selected = "https://www.googleapis.com/auth/cloud-platform")
gar_auth_service("Favorita-auth.json") #I could not get automated authorization to work 100%
# gcs_get_object(auth_file, bucket = gcs_get_global_bucket(), saveToDisk = "Favorita-auth.json", overwrite = TRUE)
vm <- gce_vm("test")
# gce_rstudio_adduser(vm, username = "bill", password = "flowerpot") #never word sigh
#vm <- gce_vm("test-1")
gce_vm_start(vm)
# gce_vm_start("test-1")
gce_get_external_ip(vm)



## Do in the virtual console: ----

# gcs_get_object("train.csv",saveToDisk = "data/train.csv", overwrite = TRUE)
# train <- fread("data/train.csv")
oil <- as.data.table(read_csv("data/oil.csv"))
items <- as.data.table(read_csv("data/items.csv"))
holidays_events <- as.data.table(read_csv("data/holidays_events.csv"))
stores <-  as.data.table(read_csv("data/stores.csv"))
df <- train

##IMPORTANT!!!: When you are done ----
gcs_upload(fit, bucket = "gs://favoritadata", name = "my_results.csv") #send
gce_vm_start("stop")




# Junk code ----
# vm <- gce_vm("slave-1")
# vm <- gce_ssh_setup(vm, username = "master", ssh_overwrite = TRUE)
# rm(dfCorr,fcast,fit,kaggledata,model,myfrequency,n,PeriodstoForecast,shiftedyear,test,time)
