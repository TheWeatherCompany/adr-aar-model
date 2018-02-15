## Update log
## 9/20/16  JAF   Added runway delta (DT) variable
## 9/27/16  CM    Updated minimum hours list
## 9/29/16  JAF   Minor updates to minimum hour summary
## 12/7/16  JAF   Added new closure variables - rate (CLOSED_RATE)
## 12/14/16 JAF   Updated runway closure dataset - includes issue time; calculate horizons

print("----------------------------- BEGIN RUNWAY CLOSURE PREP -----------------------------")

##########################################################################################################################
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(parallel))
suppressPackageStartupMessages(library(doParallel))
suppressPackageStartupMessages(library(tidyverse))


################### specify variables here

user <- Sys.getenv("LOGNAME")
airport <- "SFO"

###################
Sys.setenv(TZ="America/New_York")

################### import raw dataset saved in shared Google drive folder
data_dir <- file.path('/Users',user,'Google Drive/2016 AAR ADR and Rate Advocator')

file_1 <- file.path(data_dir,'runwayclosure2014-2016.csv')
file_1 <- read_csv(file = file_1)
file_1$X1 <- NULL
names(file_1) <- c("rwy","Issue_Time","Effect_Time","Expire_Time")

file_2 <- file.path(data_dir,'runwayclosure2016.csv')
file_2 <- read_csv(file = file_2)

file_3 <- file.path(data_dir,'runwayclosure2017.csv')
file_3 <- read_csv(file = file_3)

master_dat <- rbind(file_1, file_2, file_3)
master_dat <- unique(master_dat)
names(master_dat) <- c("rwy","issuetime","starttime","endtime")

################### 

## select alerts from top 10 airports
runway <- master_dat[which(grepl(airport,master_dat$rwy) == TRUE),]
## extract airport ID
runway$locid <- sapply(strsplit(as.character(runway$rwy)," "), "[", 1)
## remove "K"
runway$locid <- substr(x = runway$locid,start = 2,stop = 5)
## extract runway information
runway$runway <- sapply(strsplit(as.character(runway$rwy)," "), "[", 3)
runway$runway <- gsub(" ","",runway$runway) # remove any extra spaces
runway$runway <- gsub("\n","",runway$runway) # remove any extra spaces
runway$n <- nchar(runway$runway)

## remove non-runway closure information based on length of "rwy" variable
## note - there may be a better way to extract just runway closures in production
runway <- runway[which(runway$n <= 7),]

## create NA for initial closure statements
runway$init_closure <- ifelse(runway$endtime == max(runway$endtime), 1, 0)
runway$endtime <- as.character(runway$endtime)
runway$starttime <- as.character(runway$starttime)
runway$endtime <- ifelse(runway$init_closure == 1, runway$starttime, runway$endtime)

runway$start_dt <- strptime(runway$starttime,"%Y-%m-%d %H:%M:%S")
runway$start_dt <- as.POSIXct(runway$start_dt,tz = "UTC")
runway$end_dt <- strptime(runway$endtime,"%Y-%m-%d %H:%M:%S")
runway$end_dt <- as.POSIXct(runway$end_dt,tz = "UTC")
runway$issue_dt <- strptime(runway$issuetime,"%Y-%m-%d %H:%M:%S")
runway$issue_dt <- as.POSIXct(runway$issue_dt,tz = "UTC")
runway <- runway[order(runway$end_dt),]

## clean up the runways
## basically - runways are reported differently for each reports (ex: same runway reported as 4L or 04L)
## this code seeks to identify unique runways for each airport and create standard IDs for each
unique(runway$runway)
## split apart take-off / landing, runway number & direction
runway$runway_a <- sapply(strsplit(as.character(runway$runway),"/"), "[", 1)
runway$runway_b <- sapply(strsplit(as.character(runway$runway),"/"), "[", 2)
runway$runway_a_dir <- gsub('[0-9]+','',runway$runway_a)
runway$runway_b_dir <- gsub('[0-9]+','',runway$runway_b)
runway$runway_a <- gsub('[^0-9]+','',runway$runway_a)
runway$runway_b <- gsub('[^0-9]+','',runway$runway_b)
runway$runway_a <- ifelse(nchar(runway$runway_a < 2),str_pad(runway$runway_a, 2, pad = "0"),runway$runway_a) # padding may need to be adjusted...
runway$runway_b <- ifelse(nchar(runway$runway_b < 2),str_pad(runway$runway_b, 2, pad = "0"),runway$runway_b)
runway$runway <- ifelse(is.na(runway$runway_b),paste0(runway$runway_a,runway$runway_a_dir),
                        paste0(runway$runway_a,runway$runway_a_dir,"_",runway$runway_b,runway$runway_b_dir)) # combine cleaned-up runway config
unique(runway$runway) # check unique runway configs
runway$runway_id <- paste0(runway$locid,"_",runway$runway)

## combine single / double runway config names
## if one part of the runway is closed, assume the entire runway is closed (ex: if 04L is closed, 04L_22R is closed)
# rwy <- unique(runway$RUNWAY)
rwy <- runway$runway_id
rwy <- unique(rwy)
rwy <- rwy[nchar(rwy) > 8]
if(length(rwy) > 0){
  rwy <- as.data.frame(rwy)
  names(rwy) <- "runway_replace"
  rwy$locid <- sapply(strsplit(as.character(rwy$runway_replace),"_"), "[", 1)
  rwy$runway_a <- sapply(strsplit(as.character(rwy$runway_replace),"_"), "[", 2)
  rwy$runway_b <- sapply(strsplit(as.character(rwy$runway_replace),"_"), "[", 3)
  rwy <- melt(data = rwy,id.vars = c("locid","runway_replace"))
  rwy$runway_id <- paste0(rwy$locid,"_",rwy$value)
  rwy$locid <- rwy$variable <- NULL
  ## replace single runways with full runway name
  runway <- merge(x = runway,y = rwy,by = "runway_id",all.x = T)
  runway$runway <- ifelse(!is.na(runway$runway_replace),as.character(runway$runway_replace),runway$runway_id)
}
unique(runway$runway) # check unique runway configs

## round to closest hour
runway$start_dt_num <- as.numeric(runway$start_dt)
runway$end_dt_num <- as.numeric(runway$end_dt)

## pull out permanently closed runways into separate dataset
# rwy <- unique(runway$RUNWAY)
# closed <- runway[which(runway$PERM_CLOSED == 1),]
# runway <- runway[which(runway$PERM_CLOSED != 1),]

## clean up dataset
runway <- runway[,c("locid","runway","issue_dt","start_dt","end_dt","start_dt_num","end_dt_num")]
runway <- unique(runway)
runway <- runway[order(runway$runway,runway$start_dt),]
runway <- runway[order(runway$issue_dt,runway$start_dt),]

## remove / fix inconsistencies
# rwy_rm <- c("BOS_05_23","EWR_00R","ORD_02R_20L","ORD_02L_20R","ORD_10_28","ORD_04_22","PHL_06_24","PHL_15_33")
# runway <- runway[-which(runway$runway %in% rwy_rm),]
rwy <- unique(runway$runway)

## create continuous time series from START and END of dataset - for merging purposes later
start <- min(runway$start_dt)
start <- format(start,"%Y-%m-%d %H")
start <- as.POSIXct(strptime(start,"%Y-%m-%d %H"),tz = "UTC")
end <- max(runway$end_dt)
end <- format(end,"%Y-%m-%d %H")
end <- as.POSIXct(strptime(end,"%Y-%m-%d %H"),tz = "UTC")
ts <- as.data.frame(seq(from = start, to = end, by = "hour"))
names(ts) <- "datetime"

## for each runway at the airport - create a timeseries of runway closure periods
for (r in rwy){
  dts <- runway[which(runway$runway == r),c("issue_dt","start_dt","end_dt","start_dt_num","end_dt_num")]
  dts <- unique(dts)
  dts$hrs <- round((dts$end_dt_num - dts$start_dt_num) / 3600,0)
  
  ## iterate through each runway closure period (row)
  if(nrow(dts) > 0){
    for (i in c(1:nrow(dts))){
      ## create timeseries of each closure period (long)
      start_dt <- as.POSIXct(dts[i,"start_dt"])
      end_dt <- as.POSIXct(dts[i,"end_dt"])
      hrs <- dts[i,"hrs"]
    
      dat <- as.data.frame(seq(from = as.POSIXct(dts[i,"start_dt"]), to = as.POSIXct(dts[i,"end_dt"] + 3600), by = "hour"))
      names(dat) <- "datetime"
      
      ## convert date/time to hourly format
      dat$datetime <- format(dat$datetime,"%Y-%m-%d %H")
      dat$datetime <- as.POSIXct(strptime(dat$datetime,"%Y-%m-%d %H"),tz = "UTC")
      
      ## calculate rate (% of hour a runway is closed)
      dat$n <- row.names(dat)
      ## default value - runway is closed for 100% of hour
      dat$closed_rate <- 1
      dat$closed_rate <- ifelse(dat$n == 1,
                                ## first hour of runway closure
                                1 - (as.numeric(format(start_dt,"%M")) / 60),
                                ## remove any extra hours that were added to timeseries (rounding)
                                ifelse(dat$datetime > end_dt, 0, 
                                       ## last hour of runway closure
                                       ifelse(as.POSIXct(strptime(format(end_dt,"%Y-%m-%d %H"),"%Y-%m-%d %H"),tz = "UTC") == dat$datetime,
                                              as.numeric(format(end_dt,"%M")) / 60,
                                              dat$closed_rate)))
      dat$n <- NULL
      dat$closed <- 1
      dat$closed <- ifelse(dat$closed_rate == 0, 0, dat$closed)
      dat$issue_dt <- dts[i,"issue_dt"]
      dat <- dat[,c("issue_dt","datetime","closed_rate")]
      
      if(i == 1){rwy.dat <- dat}
      if(i != 1){rwy.dat <- rbind(rwy.dat,dat)}
      rm(dat,start_dt,end_dt,hrs)
    }
    ## add in runway
    rwy.dat$runway <- r
    ## remove duplicates (account for re-issues)
    rwy.dat <- unique(rwy.dat)
    rwy.dat <- rwy.dat[which(rwy.dat$closed_rate > 0),]
    
    # rwy.dat$RUNWAY <- paste0(rwy.dat$RUNWAY,"_H1")
    if(!exists("cls.ts")){cls.ts <- rwy.dat}
    if(exists("cls.ts")){cls.ts <- rbind(cls.ts,rwy.dat)}
  }
  rm(dts,rwy.dat)
}

## remove duplicate values from timeseries
dts.all <- unique(cls.ts)
## determine horizon for each time period within each forecast
dts.all$horizon <- (dts.all$datetime - strptime(format(x = dts.all$issue_dt,"%Y-%m-%d %H"),"%Y-%m-%d %H",tz = "UTC")) / 3600
dts.all$horizon <- as.numeric(dts.all$horizon)
dts.all <- dts.all[order(dts.all$runway,dts.all$datetime,dts.all$horizon),]

## create dataset of runway closures (issue_time) for each forecast horizon
horizons <- 1:12

## iterate through H 1 -12 and select valid forecasts for each horizon
cl <- makeCluster(detectCores() - 1, type='SOCK')
registerDoParallel(cl)
horz.dat <- foreach(h = horizons,.combine = 'cbind',.packages = c('plyr','data.table')) %dopar%
{
  h.dat <- dts.all
  ## select time periods we have information for at the horizon or further out
  h.dat <- h.dat[which(h.dat$horizon >= h),]
  h.dat <- unique(h.dat)
  h.dat <- h.dat[order(h.dat$issue_dt,h.dat$datetime),]
  
  ## reshape - look for lags and determine true continuity?
  # h.dat.wide <- reshape(data = h.dat,timevar = "horizon",idvar = c("DATETIME","RUNWAY"))
  
  ## for each time period, select the most recently issued runway closure information
  # h.dat <- ddply(h.dat,.(RUNWAY,DATETIME), head,1)
  
  ## select each time period - then create timeseries of all dates / times
  h.dat.unq <- h.dat[,c("runway","datetime","closed_rate")]
  h.dat.unq <- ddply(h.dat.unq,.(runway,datetime), summarize,
                     closed_rate = max(closed_rate,na.rm = T))
  h.dat.unq <- unique(h.dat.unq)
  h.dat.unq$closed <- 1
  h.dat.unq <- reshape(data = h.dat.unq,timevar = "runway",idvar = c("datetime"),direction = "wide")
  h.dat.unq <- merge(x = h.dat.unq,y = ts,by = "datetime",all.y = T)
  h.dat.unq[is.na(h.dat.unq)] <- 0
  
  closed.vars <- names(h.dat.unq)
  closed <- closed.vars[grepl(pattern = "datetime",closed.vars) == F & grepl(pattern = "rate",closed.vars) == F]
  closed.rates <- closed.vars[grepl(pattern = "datetime",closed.vars) == F & grepl(pattern = "rate",closed.vars) == T]
  for(i in c(1:length(closed))){
    c.bin <- closed[i]
    c.rate <- closed.rates[i]
    h.dat.unq$var <- h.dat.unq[,c.bin]
    h.dat.unq[,c.bin] <- with(h.dat.unq, ave(var, cumsum(var == 0), FUN = cumsum))
    h.dat.unq$var <- NULL

    m.dat <- h.dat.unq[which(h.dat.unq[,c.bin] >= 1),c("datetime",c.bin,c.rate)]
    if(nrow(m.dat) > 0){
      m.dat[,c.bin] <- 1
      m.dat <- merge(x = m.dat,y = ts,by = "datetime",all.y = T)
      m.dat[is.na(m.dat)] <- 0
    }
    if(nrow(m.dat) == 0){
      m.dat <- ts
      m.dat[,c.bin] <- 0
      m.dat[,c.rate] <- 0
    }
    m.dat <- as.data.frame(m.dat[,c(c.bin,c.rate)])
    id <- gsub("closed.","",c.bin)
    id <- paste0("clsd_",id,"_h",h)
    names(m.dat) <- gsub(c.bin,id,names(m.dat))
    names(m.dat) <- gsub(c.rate,paste0(id,"_rate"),names(m.dat))
    
    if(c.bin == closed[1]){h.dat.bin <- m.dat}
    if(c.bin != closed[1]){h.dat.bin <- cbind(h.dat.bin,m.dat)}
  }
  temp <- h.dat.bin
}
stopCluster(cl)

rwy_closures <- cbind(ts,horz.dat)
rwy_closures$dt <- as.character(strptime(rwy_closures$datetime,format = "%Y-%m-%d %H:%M",tz = "UTC"))
rwy_closures$datetime <- NULL

## save final dataset
output_dir <- '/Users/jfinn/Google Drive/ADR AAR Model Build/2017 Model Re-Train/notam_data'
output_file <- file.path(output_dir, 'processed', paste0(airport, '_NOTAM_HR_201401_201712.Rdata'))
save(rwy_closures,file = output_file)
