##########################################################################################################################
## Generate train script for model run
## 
## Update Log
## 12/01/16  JAF - Created script
## 01/11/18  JAF - Updated script
##########################################################################################################################

########################## set parameters here

user <- Sys.getenv("LOGNAME")
ex_num <- 'M1'

########################## list of all avaiable values

# airports <- c("EWR","JFK","LGA","BOS","PHL","ATL","DFW","ORD","LAX","SFO")
# horizons <- c("H1","H2","H3","H4","H5","H6","H7","H8","H9","H10","H11","H12")
# rates <- c("ARR_RATE","DEP_RATE")

########################## manually set

airports <- c("SFO","LGA")
horizons <- c("H1","H2","H3","H4","H5","H6","H7","H8","H9","H10","H11","H12")
rates <- c("ARR_RATE","DEP_RATE")
scripts <- c("TRAIN_OVA_BINARY_SVM_RF_RFE.R")

train <- merge(airports,rates)
names(train) <- c("airport","rate")
train <- merge(train,horizons)
names(train) <- c("airport","rate","horizon")
train <- merge(train,scripts)
names(train) <- c("airport","rate","horizon","script")

########################## 
train$loc <- "Rscript"
train$model <- ex_num
train$id <- paste(train$loc,train$script,train$airport,train$rate,train$model,train$horizon,sep=" ")
train$num <- as.numeric(gsub("H","",train$horizon))

train <- train[order(train$script,train$airport,train$rate,train$num),"id"]
output_dir <- file.path(getwd(), '2_train')
write.table(x = train, 
            file = file.path(output_dir, 'TRAIN_BATCH.txt'),
            quote = F,
            col.names = F,
            row.names=FALSE)
Sys.chmod(paths = file.path(output_dir, 'TRAIN_BATCH.txt'), "700")
