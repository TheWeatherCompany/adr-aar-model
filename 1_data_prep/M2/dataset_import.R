################################################################################
## Name: dataset_import.R
## Description: Functions to import and format various datasets
## Date: Jan 25, 2018
## Author: jaf
################################################################################

suppressPackageStartupMessages(library(tidyverse))
options(warn=-1)

import_notam <- function(user, airport){
  dataset_dir <- 'notam_data/processed'
  notam_files <- list.files(file.path(getwd(), dataset_dir), full.names = T)
  notam_files <- notam_files[grepl(airport, notam_files) == T]
  notam_data <- c()
  for(file in notam_files){
    load(file)
    notam_data <- rbind(notam_data, rwy_closures)
    rm(rwy_closures, file)
  }
  return(notam_data)
}

format_notam <- function(notam_data, horizon){
  ## runway closure variables -- select closures and rates for appropriate horizon
  # notam_features <- names(notam_data)
  # rnwylist <- notam_features[grepl(pattern = "clsd_",x = notam_features) == TRUE &
  #                              grepl(pattern = "rate",x = notam_features) == TRUE]
  # rnwylist <- rnwylist[which(grepl(pattern = paste0("_",tolower(horizon),"_"),x = rnwylist) == TRUE)]
  # notam_data_sub <- notam_data[,which(names(notam_data) %in% c("dt",rnwylist))]
  
  notam_data_sub <- notam_data
  return(notam_data_sub)
}

import_metar <- function(user, airport){
  dataset_dir <- 'metar_data/processed'
  metar_files <- list.files(file.path(getwd(), dataset_dir), full.names = T)
  metar_files <- metar_files[grepl(airport, metar_files) == T]
  metar_data <- c()
  for(file in metar_files){
    load(file)
    metar_data <- rbind(metar_data, data_proc)
    rm(data_proc, file)
  }
  return(metar_data)
}
format_metar <- function(metar_data, horizon){
  h_num <- as.numeric(gsub("H","",horizon))
  ## keep metar lag features (no real-time)
  names(metar_data) <- tolower(names(metar_data))
  lag_features <- names(metar_data)
  # lag_features <- lag_features[(grepl("_lag", lag_features) == F)]
  # lag_features <- lag_features[(grepl(paste0("lag", h_num), lag_features) == T)]
  # if(horizon == "H1"){lag_features <- lag_features[(grepl(paste(c("_lag10","_lag11","_lag12"), collapse = "|"), lag_features) == F)]}
  
  ## only keep weather variables available in TAF forecast
  # wx_keep <- paste0(c("wspd","wdir","vis","ceiling","clds_pct","obscur","precip","convec","lifr","ifr","mvfr","vfr","flightrule"), collapse = "|")
  # lag_features <- lag_features[(grepl(wx_keep, lag_features) == T)]
  wx_rem <- c("station","qpf","rh","dewpt","temp")
  lag_features <- lag_features[!lag_features %in% wx_rem]
  
  ## remove the max / min / avg
  # calc_rem <- paste0(c("_min","_max","_avg","_daily"), collapse = "|") 
  # lag_features <- lag_features[(grepl(calc_rem, lag_features) == T)]
  lag_features <- unique(lag_features)

  # metar_data_sub <- metar_data[,c("dt",lag_features)]
  metar_data_sub <- metar_data[,c(lag_features)]
  return(metar_data_sub)
}

import_aspm <- function(user, airport){
  dataset_dir <- 'aspm_data/processed'
  aspm_files <- list.files(file.path(getwd(), dataset_dir), full.names = T)
  aspm_files <- aspm_files[grepl(airport, aspm_files) == T]
  aspm_data <- c()
  for(file in aspm_files){
    load(file)
    aspm_data <- rbind(aspm_data, dat)
    rm(dat, file)
  }
  return(aspm_data)
}

format_aspm <- function(aspm_data, horizon, response){
  h_num <- as.numeric(gsub("H","",horizon))
  ## for aspm -- remove lags that we will not have in real-time (less than horizon hour)
  features <- names(aspm_data)
  temp_data <- aspm_data[order(aspm_data$dt),]
  
  ## create lags
  lag_features <- features[!features %in% c("locid","dt","oag_dep","oag_arr")]
  for(var in lag_features){
    if(var %in% c("arr_demand", "dep_demand")){
      if(horizon == "H1"){
        lag_val <- temp_data[,var]
        lag_var <- var
      }
      if(horizon != "H1"){
        lag_val <- shift(x = temp_data[,var], n = (h_num - 1), type ="lag")
        lag_var <- paste0(var,"_lag", (h_num - 1))
      }
    }
    if(!var %in% c("arr_demand", "dep_demand")){
      lag_val <- shift(x = temp_data[,var], n = h_num, type ="lag")
      lag_var <- paste0(var,"_lag", h_num)
    }
    temp_data[,lag_var] <- lag_val
    features <- c(features, lag_var)
    if(var != response){features <- features[features != var]}
  }
  
  aspm_data_sub <- temp_data[,c(features)]
  aspm_data_sub$locid <- NULL
  return(aspm_data_sub)
}

# forecast_calcs <- function(dataset, var){
#   var_val <- dataset[order(dataset$datetime), c("datetime",var)] %>% data.frame()
#   var_val_rev <- dataset[order(desc(dataset$datetime)), c("datetime", var)] %>% data.frame()
#   
#   for(hr in c(1, seq(from = 3, to = 24, by = 3))){
#     # hr_extra <- hr + 1
#     ## determine the minimum in the past n hours (inclusive of current hour)
#     min_val <- roll_min(x = var_val_rev[,var], n = (hr))
#     min_var <- paste0(var,"_min",abs(hr),'hr')
#     max_val <- roll_max(x = var_val_rev[,var], n = (hr))
#     max_var <- paste0(var,"_max",abs(hr),'hr')
#     avg_val <- round(roll_mean(x = var_val_rev[,var], n = (hr)), 1)
#     avg_var <- paste0(var,"_avg",abs(hr),'hr')
#     
#     var_val_rev_hr <- var_val_rev[1:(nrow(var_val_rev) - (hr - 1)),]
#     var_val_rev_hr[,min_var] <- min_val
#     var_val_rev_hr[,max_var] <- max_val
#     var_val_rev_hr[,avg_var] <- avg_val
#     var_val_rev_hr[,var] <- NULL
#     
#     var_val <- left_join(var_val, var_val_rev_hr, by = c("datetime"))
#   }
#   return(var_val)
# }
# 
# forecast_lags <- function(dataset, var){
#   var_val <- dataset[order(dataset$datetime), c("datetime",var)] %>% data.frame()
#   
#   for(hr in c(seq(from = 1, to = 12, by = 1))){
#     ## calculate the lag
#     lag_val <- shift(x = var_val[,var], n = hr, type ="lag")
#     lag_var <- paste0(var,"_lag",abs(hr))
#     
#     ## calculate the n hour change
#     # delta_val <- var_val[,var] - lag_val
#     # delta_var <- paste0(var,"_ch",abs(hr))
#     
#     var_val[,lag_var] <- lag_val
#     # var_val[,delta_var] <- delta_val
#   }
#   return(var_val)
# }