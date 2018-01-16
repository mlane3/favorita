all.files <- list.files(path = "data/itemNbr/",pattern = ".csv")
## Read data using fread
mydata <- do.call('rbind',mylist)

ptm <- proc.time()
l <- lapply(all.files, fread, sep=",")
dt <- rbindlist( l )
proc.time() - ptm
dt2 <- unique( dt[ , 2:7 ] )


###
d <- testDataDF$id
chunk <- split.data.frame(testDataDF, 1:5)

chunk1 <- chunks$`1`
chunk2 <- chunks$`2`
chunk3 <- chunks$`3`
chunk4 <- chunks$`4`
chunk5 <- chunks$`5`
chunk6 <- chunks$`6`
chunk7 <- chunks$`7`
chunk8 <- chunks$`8`
chunk9 <- chunks$`9`
chunk10 <- chunks$`10`
chunk11 <- chunks$`11`
chunk12 <- chunks$`12`