################################################################################
## Name: persistence_model.R
## Description: Generate persistence model for each airport; AAR and ADR
## Date: Mar 15, 2018
## Author: jaf
##
################################################################################
Sys.setenv(TZ="America/New_York")

suppressPackageStartupMessages(library(splitstackshape))

airports <- c("LGA","SFO")

setwd("../../")
dir <- getwd()

################### import dataset
## define persistence model
persistence_model <- function(a, aspm_datasets){
  aspm_file <- aspm_datasets[which(grepl(a, aspm_datasets) == T)]
  load(aspm_file)
  keep_list <- c("dt", "arr_rate", "dep_rate")
  for(i in c(1:12)){
    aar_lag_var <- paste0("arr_rate_lag", i)
    adr_lag_var <- paste0("dep_rate_lag", i)
    dat[,aar_lag_var] <- shift(dat$arr_rate, type = "lag", n = i)
    dat[,adr_lag_var] <- shift(dat$dep_rate, type = "lag", n = i)
    keep_list <- c(keep_list, aar_lag_var, adr_lag_var)
  }
  dat_out <- dat[,keep_list]
  return(dat_out)
}

## generate persistence model file for each airport
aspm_datasets <- list.files(file.path(dir, 'aspm_data', 'processed'), pattern = ".Rdata", full.names = T)
for(a in airports){
  dat_out <- persistence_model(a, aspm_datasets)
  model_file <- file.path(dir, "3_test/persistence_model", paste0(a, ".csv"))
  write.csv(x = dat_out, file = model_file)
  rm(dat_out, model_file)
}
