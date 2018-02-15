suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(splitstackshape))
suppressPackageStartupMessages(library(RcppRoll))
suppressPackageStartupMessages(library(data.table))

runway_config <- function(dataset, airport){
  rwy <- dataset[,c("locid","runway")]
  rwy <- unique(rwy)
  
  rwy <- cSplit(rwy,"runway", "|")
  rwy_split <- names(rwy)
  rwy_split <- rwy_split[rwy_split != "locid"]
  for(var in rwy_split){
    rwy <- cSplit(rwy,var, ",")
  }
  rwy <- as.data.frame(rwy)
  rwyids <- c()
  for(i in 2:ncol(rwy)){
    ids <- paste0(rwy$locid,"_",as.character(rwy[,i]))
    ids <- unique(ids)
    rwyids <- c(rwyids,ids)
    rm(i,ids)
  }
  rwyids <- unique(rwyids)
  ## remove extraneous values
  rwyids <- rwyids[which(grepl(pattern = "AIRPORT",x = rwyids) == F & 
                           grepl(pattern = "CLOSED",x = rwyids) == F & 
                           grepl(pattern = "NA",x = rwyids) == F)]
  rwyids <- as.character(na.omit(rwyids))
  
  ## create binary - is the runway included in the configuration? 1 = yes, 0 = no
  dataset$runway <- as.character(dataset$runway)
  ids_labels <- rwyids[which(grepl(pattern = airport,x = rwyids) == T)]
  ids_search <- gsub(paste0(airport, "_"), "", ids_labels)
  for(id in ids_search){
    dataset$dummy <- ifelse(dataset$locid != airport, NA,
                        ifelse(grepl(pattern = id,x = dataset$runway) == TRUE,1,0)
    )
    id <- paste0("rwycnfg_", airport, "_",id)
    names(dataset) <- gsub("dummy", id, names(dataset))
  }
  return(dataset)
}

forecast_calcs <- function(dataset, var){
  var_val <- dataset[order(dataset$datetime), c("datetime",var)] %>% data.frame()
  var_val_rev <- dataset[order(desc(dataset$datetime)), c("datetime", var)] %>% data.frame()
  
  for(hr in c(1, seq(from = 3, to = 24, by = 3))){
    # hr_extra <- hr + 1
    ## determine the minimum in the past n hours (inclusive of current hour)
    min_val <- roll_min(x = var_val_rev[,var], n = (hr))
    min_var <- paste0(var,"_min",abs(hr),'hr')
    max_val <- roll_max(x = var_val_rev[,var], n = (hr))
    max_var <- paste0(var,"_max",abs(hr),'hr')
    avg_val <- round(roll_mean(x = var_val_rev[,var], n = (hr)), 1)
    avg_var <- paste0(var,"_avg",abs(hr),'hr')
    
    var_val_rev_hr <- var_val_rev[1:(nrow(var_val_rev) - (hr - 1)),]
    var_val_rev_hr[,min_var] <- min_val
    var_val_rev_hr[,max_var] <- max_val
    var_val_rev_hr[,avg_var] <- avg_val
    var_val_rev_hr[,var] <- NULL
    
    var_val <- left_join(var_val, var_val_rev_hr, by = c("datetime"))
  }
  return(var_val)
}

forecast_lags <- function(dataset, var){
  var_val <- dataset[order(dataset$datetime), c("datetime",var)] %>% data.frame()
  
  for(hr in c(seq(from = 1, to = 12, by = 1))){
    ## calculate the lag
    lag_val <- shift(x = var_val[,var], n = hr, type ="lag")
    lag_var <- paste0(var,"_lag",abs(hr))
    
    ## calculate the n hour change
    delta_val <- var_val[,var] - lag_val
    delta_var <- paste0(var,"_ch",abs(hr))
    
    var_val[,lag_var] <- lag_val
    var_val[,delta_var] <- delta_val
  }
  return(var_val)
}