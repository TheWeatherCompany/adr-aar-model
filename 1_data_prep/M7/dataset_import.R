################################################################################
## Name: dataset_import.R
## Description: Functions to import and format various datasets
## Date: Jan 25, 2018
## Author: jaf
################################################################################

suppressPackageStartupMessages(library(tidyverse))
options(warn=-1)

## import raw notam data
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

## format notam data
format_notam <- function(notam_data, horizon){
  notam_data_sub <- notam_data
  return(notam_data_sub)
}

## import raw aspm data
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

## format aspm data
format_aspm <- function(aspm_data, horizon, response){
  h_num <- as.numeric(gsub("H","",horizon))
  ## for aspm -- remove lags that we will not have in real-time (less than horizon hour)
  features <- names(aspm_data)
  temp_data <- aspm_data[order(aspm_data$dt),]
  
  ## just select response and dt
  aspm_data_sub <- temp_data[,c("dt", response, "oag_dep", "oag_arr")]
  
  for(var in c("oag_dep", "oag_arr")){
    ## calculate lead values
    temp_leads <- forecast_leads(aspm_data_sub, var, 3)
    temp_leads[,var] <- NULL
    temp_lags <- forecast_lags(aspm_data_sub, var, 3)
    temp_lags[,var] <- NULL
    
    ## calculate moving averages
    temp_fcst <- forecast_calcs(aspm_data_sub, var)
    
    aspm_data_sub <- left_join(x = aspm_data_sub, y = temp_leads, by = "dt")
    aspm_data_sub <- left_join(x = aspm_data_sub, y = temp_lags, by = "dt")
    aspm_data_sub <- left_join(x = aspm_data_sub, y = temp_fcst, by = "dt")
    rm(temp_leads, temp_fcst)
  }
  
  aspm_data_sub$locid <- NULL
  return(aspm_data_sub)
}

## import metar data
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

## format metar data
format_metar <- function(metar_data, horizon){
  h_num <- as.numeric(gsub("H","",horizon))
  ## keep metar lag features (no real-time)
  metar_data$wdir_cat <- factor(metar_data$wdir_cat)
  names(metar_data) <- tolower(names(metar_data))
  ## calculate u and v wind components
  metar_data$wdir_u <- -metar_data$wspd * sin(metar_data$wdir * (pi / 180))
  metar_data$wdir_v <- -metar_data$wspd * cos(metar_data$wdir * (pi / 180))
  
  features <- names(metar_data)
  wx_keep <- c(
    "clds_pct","wspd","vis","ceiling_ln"
    , "wdir_u", "wdir_v"
    # ,"wdir_sin", "wdir_cat", "wdir"
    # ,"ceilingflightrule","visflightrule","flightrule"
    , "temp", "dewpt", "rh"       ## PFP features
    , "wx_obscur", "wx_precip", "wx_convec"
    , "lifr", "ifr", "mvfr", "vfr"
  )
  features <- features[features %in% wx_keep]
  features <- unique(features)
  metar_data_sub <- metar_data[,c("dt", features)]
  
  for(var in features){
    ## calculate lead values
    temp_leads <- forecast_leads(metar_data_sub, var, 3)
    temp_leads[,var] <- NULL
    temp_lags <- forecast_lags(metar_data_sub, var, 3)
    temp_lags[,var] <- NULL

    ## calculate max / min / avg for numeric variables
    temp_fcst <- forecast_calcs(metar_data_sub, var)

    metar_data_sub <- left_join(x = metar_data_sub, y = temp_leads, by = "dt")
    metar_data_sub <- left_join(x = metar_data_sub, y = temp_lags, by = "dt")
    metar_data_sub <- left_join(x = metar_data_sub, y = temp_fcst, by = "dt")
    rm(temp_leads, temp_fcst)
  }
  
  return(metar_data_sub)
}


###################### subfunctions
forecast_calcs <- function(dataset, var){
  var_val <- dataset[order(dataset$dt), c("dt",var)] %>% data.frame()
  
  # for(hr in c(seq(from = 2, to = 6, by = 2))){
  for(hr in c(3)){
    ## calculate the rolling average over three hours
    avg_val_C <- round(roll_mean(x = var_val[,var], n = (hr), fill = NA, align = "center"), 1)
    avg_val_L <- round(roll_mean(x = var_val[,var], n = (hr), fill = NA, align = "left"), 1)
    avg_val_R <- round(roll_mean(x = var_val[,var], n = (hr), fill = NA, align = "right"), 1)
    avg_var <- paste0(var, "_avg_", hr, "hr")

    var_val <- cbind(var_val, avg_val_C, avg_val_L, avg_val_R)
    names(var_val) <- gsub("avg_val_", avg_var, names(var_val))
  }
  var_val[,var] <- NULL
  return(var_val)
}

forecast_leads <- function(dataset, var, n){
  var_val <- dataset[order(dataset$dt), c("dt",var)] %>% data.frame()
  for(hr in c(seq(from = 1, to = n, by = 1))){
    ## calculate the lead
    lead_val <- shift(x = var_val[,var], n = hr, type ="lead")
    lead_var <- paste0(var,"_lead",abs(hr))
    var_val[,lead_var] <- lead_val
  }
  return(var_val)
}

forecast_lags <- function(dataset, var, n){
  var_val <- dataset[order(dataset$dt), c("dt",var)] %>% data.frame()
  for(hr in c(seq(from = 1, to = n, by = 1))){
    ## calculate the lag
    lead_val <- shift(x = var_val[,var], n = hr, type ="lag")
    lead_var <- paste0(var,"_lag",abs(hr))
    var_val[,lead_var] <- lead_val
  }
  return(var_val)
}
