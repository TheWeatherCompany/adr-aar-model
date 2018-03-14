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
  aspm_data_sub <- temp_data[,c("dt", response)]
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
  features <- names(metar_data)
  wx_keep <- c("clds_pct","wdir_sin","wspd","vis","ceiling","ceiling_ln"
               # ,"wdir", "wdir_cat"
               ,"ceilingflightrule","visflightrule","flightrule"
               )
  features <- features[features %in% wx_keep]
  features <- unique(features)
  metar_data_sub <- metar_data[,c("dt", features)]
  
  ## dummy factor variables
  cat_vars <- sapply(metar_data_sub,FUN = is.numeric)
  cat_vars <- names(cat_vars[which(cat_vars == FALSE)])
  cat_vars <- cat_vars[!cat_vars %in% c('dt','datetime')] ## need to remove this variable
  dum_create <- dummyVars(dt ~ ., data = metar_data_sub[,names(metar_data_sub) %in% c("dt",cat_vars)])
  dum_features <- data.frame(predict(dum_create, newdata = metar_data_sub))
  names(dum_features) <- gsub("\\.","_",names(dum_features))
  metar_data_sub <- data.frame(metar_data_sub, dum_features)
  metar_data_sub <- metar_data_sub[-which(names(metar_data_sub) %in% c(cat_vars,"Y"))]
  features <- names(metar_data_sub)[-which(names(metar_data_sub) %in% c("dt"))]
  
  for(var in features){
    ## calculate lead values
    temp_leads <- forecast_leads(metar_data_sub, var)
    temp_leads[,var] <- NULL
    
    ## calculate max / min / avg for numeric variables
    temp_fcst <- forecast_calcs(metar_data_sub, var)
    temp_leads <- left_join(x = temp_leads, y = temp_fcst, by = "dt")

    metar_data_sub <- left_join(x = metar_data_sub, y = temp_leads, by = "dt")
    rm(temp_leads, temp_fcst)
  }
  return(metar_data_sub)
}


###################### subfunctions
forecast_calcs <- function(dataset, var){
  var_val <- dataset[order(dataset$dt), c("dt",var)] %>% data.frame()
  
  for(hr in c(seq(from = 2, to = 6, by = 2))){
    # hr_extra <- hr + 1
    ## determine the minimum in the past n hours (inclusive of current hour)
    min_val <- roll_min(x = var_val[,var], n = (hr))
    min_var <- paste0(var,"_min",abs(hr),'hr')
    max_val <- roll_max(x = var_val[,var], n = (hr))
    max_var <- paste0(var,"_max",abs(hr),'hr')
    avg_val <- round(roll_mean(x = var_val[,var], n = (hr)), 1)
    avg_var <- paste0(var,"_avg",abs(hr),'hr')

    var_val_hr <- data.frame(var_val[1:(nrow(var_val) - (hr - 1)), c("dt")])
    names(var_val_hr) <- "dt"
    var_val_hr[,min_var] <- min_val
    var_val_hr[,max_var] <- max_val
    var_val_hr[,avg_var] <- avg_val

    var_val <- left_join(var_val, var_val_hr, by = c("dt"))
  }
  var_val[,var] <- NULL
  return(var_val)
}

forecast_leads <- function(dataset, var){
  var_val <- dataset[order(dataset$dt), c("dt",var)] %>% data.frame()
  for(hr in c(seq(from = 1, to = 6, by = 1))){
    ## calculate the lead
    lead_val <- shift(x = var_val[,var], n = hr, type ="lead")
    lead_var <- paste0(var,"_lead",abs(hr))
    var_val[,lead_var] <- lead_val
  }
  return(var_val)
}
