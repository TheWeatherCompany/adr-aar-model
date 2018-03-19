##########################################################################################################################
## Dataset preparation for file transfer to AWS
## 
## Update Log
## 12/01/16  JAF - Created script
## 01/15/18  JAF - Updated script
##########################################################################################################################

########################## set parameters here

user <- Sys.getenv("LOGNAME")
ex_num <- 'M3'

##########################

########################## list of all avaiable values

# airports <- c("EWR","JFK","LGA","BOS","PHL","ATL","DFW","ORD","LAX","SFO")
# horizons <- c("H1","H3","H6","H12")
# rates <- c("ARR_RATE","DEP_RATE")

########################## 

# airports <- c("EWR","JFK","LGA","BOS","PHL","ATL","DFW","ORD","LAX","SFO")
airports <- c("LGA","SFO")
# horizons <- "H1 H2 H3 H4 H5 H6 H7 H8 H9 H10 H11 H12"
horizons <- "H1"
rates <- c("ARR_RATE","DEP_RATE") #

prep <- merge(airports,rates)
names(prep) <- c("airport","rate")
prep$horizon <- paste(horizons)

## manually set these
# prep$loc <- "/usr/local/bin/Rscript"
prep$loc <- "Rscript"
prep$script <- "dataset_prep.R"
prep$user <- user
prep$model <- ex_num

prep$id <- paste(prep$loc,prep$script,prep$user,prep$airport,prep$rate,prep$model,prep$horizon,sep=" ")
prep <- prep[order(prep$id),"id"]
output_dir <- file.path(getwd())
write.table(x = prep, 
            file = file.path(output_dir, 'dataset_prep.txt'),
            quote = F,
            col.names = F,
            row.names=FALSE)
Sys.chmod(paths = file.path(output_dir, 'dataset_prep.txt'), "700")
