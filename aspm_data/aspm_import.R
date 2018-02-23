#####################################################################################################################################
## Update Log:
## 10/19/16   JAF   Updated to include TAFF data; removed "shift" function
## 10/26/16   JAF   Added level shift / interruption variables; Max / min entry error and training
## 12/7/16    JAF   Created binaries for runway configuration varibles
## 1/6/17     JAF   Updated and removed extra code
## 1/2/18     JAF   New version - model re-train
#####################################################################################################################################

print("----------------------------- BEGIN DATA PREP -----------------------------")
Sys.setenv(TZ="America/New_York")
source('aspm_functions.R')

################### input variables here

user <- Sys.getenv("LOGNAME")
airport <- "SFO"
icao <- paste0("K", airport)

suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(splitstackshape))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))

raw_data_dir <- file.path('/Users/jfinn/Google Drive/ADR AAR Model Build/Datasets/Raw Data')
processed_data_dir <- file.path('/Users/jfinn/Google Drive/ADR AAR Model Build/2017 Model Re-Train/raw_datasets')

################### raw dataset saved in shared Google drive folder - no changes made from csv exports
file <- file.path(processed_data_dir, 'ASPM_raw.Rdata')
load(file)

# clean up location ID variable
hrly_dat$LOCID <- as.character(hrly_dat$LOCID)
hrly_dat$LOCID <- gsub(" ","",hrly_dat$LOCID)

################### only select variables available in real-time
dat <- hrly_dat[which(hrly_dat$LOCID == airport),]
keep <- c('LOCID'
          ,'YYYYMM'
          ,'DAYNUM'
          ,'HR_LOCAL'
          ,'OAG_DEP'
          ,'OAG_ARR'
          ,'ETMS_DEP'
          ,'ETMS_ARR'
          ,'RUNWAY'
          ,'DEP_DEMAND'
          ,'ARR_DEMAND'
          ,'DEP_RATE'
          ,'ARR_RATE')
dat <- dat[which(names(dat) %in% c(keep))]
names(dat) <- tolower(names(dat))
# str(dat)

## get the local time zone
airport_tz <- suppressWarnings(read_csv(file = "airport_data.csv"))
airport_tz <- airport_tz[which(airport_tz$icao == icao),]
# airport_tz_cat <- as.character(airport_tz$tz_timezone)
airport_tz_adj <- as.numeric(airport_tz$timezone)

## convert datetime, given in local time, to UTC (avoid DST issues)
dat$datetime <- paste0(dat$yyyymm,str_pad(dat$daynum, 2, pad = "0")," ",str_pad(dat$hr_local, 2, pad = "0"))
dat$datetime <- as.POSIXct(strptime(x = dat$datetime,format = "%Y%m%d %H", tz = "UTC"))
dat$datetime_num <- as.numeric(dat$datetime)
## add in adjustment to convert from local time to UTC
dat$datetime_num <- dat$datetime_num + (3600 * airport_tz_adj)
dat$dt <- as.POSIXct(dat$datetime_num,origin = "1970-01-01", tz = "UTC")

dat <- dat[order(dat$dt),]
dat <- unique(dat)

################### arrival & departure estimated vs. demand ratio
dat <- within(dat,{
  arr_dm_et_ratio <- dat$etms_arr / dat$arr_demand
  arr_dm_et_ratio[dat$etms_arr == 0 & dat$arr_demand == 0] <- 0
  arr_dm_et_ratio[dat$etms_arr == 0 & dat$arr_demand != 0] <- 1
  arr_dm_et_ratio[dat$etms_arr != 0 & dat$arr_demand == 0] <- 0
})
dat <- within(dat,{
  dep_dm_et_ratio <- dat$etms_dep / dat$dep_demand
  dep_dm_et_ratio[dat$etms_dep == 0 & dat$dep_demand == 0] <- 0
  dep_dm_et_ratio[dat$etms_dep == 0 & dat$dep_demand != 0] <- 1
  dep_dm_et_ratio[dat$etms_dep != 0 & dat$dep_demand == 0] <- 0
})

################### clean up runway variable - create list of unique runways
dat <- runway_config(dat, airport)

################### create lags, etc for relevant variables
# rwy_features <- names(dat)[which(grepl("rwycnfg",names(dat)) == T)]
# aspm_fcst_vars <- c(rwy_features,
#                     "oag_dep","oag_arr","etms_dep","etms_arr","dep_demand","arr_demand","dep_rate","arr_rate")
# for(var in aspm_fcst_vars){
#   temp_dat <- forecast_calcs(dat, var)
#   lag_vars <- names(temp_dat)[!names(temp_dat) %in% c("datetime")]
#   for(l_var in lag_vars){
#     temp_dat_lag <- forecast_lags(temp_dat, l_var)
#     temp_dat_lag[,l_var] <- NULL
#     temp_dat <- left_join(temp_dat, temp_dat_lag, by = "datetime")
#   }
#   temp_dat[,var] <- NULL
#   dat <- left_join(dat, temp_dat, by = "datetime")
#   rm(temp_dat, var)
# }

################### save ASPM dataset
dat$dt <- as.character(dat$dt)
dat[,c("yyyymm","daynum","hr_local","datetime", "datetime_num")] <- list(NULL)  # remove extraneous variables

output_file <- file.path("processed", paste0(airport,'_ASPM_HR_201401_201709.Rdata'))
save(dat,file = output_file)
