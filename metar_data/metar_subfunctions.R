################################################################################
## Name: metar_subfunctions.R
## Description: Additional subfunctions to support METAR data processing
## Date: Jan 19, 2018
## Author: jaf
################################################################################

############################################################### define subroutines
clean_clouds <- function(x){
  cld_grd <- data.frame(desc = c("CLR", "FEW","SCT","BKN","OVC","VV ",NA),
                        num =  c(5,25,50,87,100,101,NA),
                        stringsAsFactors = F)
  return(cld_grd$num[match(x,cld_grd$desc)])
}
replace_M <- function(x, m = "M") as.numeric(ifelse(x==m,NA,x))
wdir_to_num <- function(x) sin(pi * x / 180)

wx_calcs <- function(dataset, var){
  var_val <- dataset[order(dataset$datetime), c("datetime",var)] %>% data.frame()
  var_val_rev <- dataset[order(desc(dataset$datetime)), c("datetime", var)] %>% data.frame()
  
  for(hr in c(1, seq(from = 3, to = 24, by = 3))){
    # hr_extra <- hr + 1
    ## determine the minimum in the past n hours (inclusive of current hour)
    var_val$min_dummy <- roll_min(x = var_val_rev[,var], n = (hr), fill = NA)
    min_var <- paste0(var,"_min",abs(hr),'hr')
    
    var_val_rev$max_dummy <- roll_max(x = var_val_rev[,var], n = (hr), fill = NA)
    max_var <- paste0(var,"_max",abs(hr),'hr')
    
    var_val_rev$avg_dummy <- round(roll_mean(x = var_val_rev[,var], n = (hr), fill = NA), 1)
    avg_var <- paste0(var,"_avg",abs(hr),'hr')
    
    names(var_val) <- gsub("min_dummy", min_var, names(var_val))
    names(var_val_rev) <- gsub("max_dummy", max_var, names(var_val_rev))
    names(var_val_rev) <- gsub("avg_dummy", avg_var, names(var_val_rev))
  }
  var_val_rev <- var_val_rev[order(var_val_rev$datetime),]
  var_val_rev$datetime <- NULL
  final <- cbind(var_val, var_val_rev)
  return(final)
}

wx_lags <- function(dataset, var){
  var_val <- dataset[, c("datetime",var)] %>% data.frame()
  
  for(hr in c(seq(from = 1, to = 12, by = 1))){
    ## calculate the lag
    lag_val <- shift(x = var_val[,var], n = hr, type ="lag")
    lag_var <- paste0(var,"_lag",abs(hr))
    
    ## calculate the n hour change
    delta_val <- var_val[,var] - lag_val
    # delta_var <- paste0(var,"_ch",abs(hr))
    
    var_val[,lag_var] <- lag_val
    # var_val[,delta_var] <- delta_val
  }
  return(var_val)
}
