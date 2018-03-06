################################################################################
## Name: dataset_prep.R
## Description: Merge datasets and prepare data for modeling
## Date: Jan 25, 2018
## Author: jaf
##
## Notes: Test feature selection: 1) all important variables; 2) some up to threshold; 3) remove all HCV, NZV features
## Test 1) Select features with var_imp >= 0.0005
################################################################################
'%!in%' <- function(x,y)!('%in%'(x,y))
Sys.setenv(TZ="America/New_York")
seed <- 11182017

suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(tidyverse))

RunBatch = 0

if(RunBatch == 0){
  user <- 'jfinn'
  airport <- 'LGA'
  response <- 'ARR_RATE'
  model <- 'M1'
  horizons <- paste0("H",c(1:12))
}

if(RunBatch == 1){
  args <- commandArgs(trailingOnly = TRUE)
  user <- args[1]
  airport <- args[2]
  response <- args[3]
  model <- args[4]
  horizons <- args[5:length(args)]
}

###################
source(file.path(model, 'dataset_import.R'))
source(file.path(model, 'horizon_format.R'))

print(user)
print(airport)
print(response)
print(model)

setwd("../")
dir <- getwd()

################### import dataset
notam_data <- import_notam(user, airport)
metar_data <- import_metar(user, airport)
aspm_data <- import_aspm(user, airport)

################### format datasets per horizon & output
response <- tolower(response)
rate <- ifelse(response == "dep_rate","ADR","AAR")

for(horizon in horizons){
  print(horizon)
  ## run through data prep process for given horizon
  horizon_data <- horizon_data_prep(notam_data, aspm_data, metar_data, airport, response, model, horizon)
  
  model_num <- paste0(model,"_",horizon)
  output_dir <- file.path('1_data_prep', model)
  
  # training set
  saveRDS(object = data.frame(horizon_data$DT),file = file.path(output_dir, paste0(airport,"_",rate,'_',model_num,'_DT.Rds')))
  saveRDS(object = data.frame(horizon_data$Y),file = file.path(output_dir, paste0(airport,"_",rate,'_',model_num,'_Y.Rds')))
  saveRDS(object = data.frame(horizon_data$X),file = file.path(output_dir, paste0(airport,"_",rate,'_',model_num,'_X.Rds')))
  # saveRDS(object = data.frame(horizon_data$W),file = file.path(output_dir, paste0(airport,"_",rate,'_',model_num,'_W.Rds')))
  
  # test set - save response and feature sets for evaluation
  saveRDS(object = data.frame(horizon_data$DTt),file = file.path(output_dir, paste0(airport,"_",rate,'_',model_num,'_DTt.Rds')))
  saveRDS(object = data.frame(horizon_data$Yt),file = file.path(output_dir, paste0(airport,"_",rate,'_',model_num,'_Yt.Rds')))
  saveRDS(object = data.frame(horizon_data$Xt),file = file.path(output_dir, paste0(airport,"_",rate,'_',model_num,'_Xt.Rds')))
  # saveRDS(object = data.frame(horizon_data$Wt),file = file.path(output_dir, paste0(airport,"_",rate,'_',model_num,'_Wt.Rds')))
  
  # validation set - save full testing set response and feature sets for evaluation
  saveRDS(object = data.frame(horizon_data$DTv),file = file.path(output_dir, paste0(airport,"_",rate,'_',model_num,'_DTv.Rds')))
  saveRDS(object = data.frame(horizon_data$Yv),file = file.path(output_dir, paste0(airport,"_",rate,'_',model_num,'_Yv.Rds')))
  saveRDS(object = data.frame(horizon_data$Xv),file = file.path(output_dir, paste0(airport,"_",rate,'_',model_num,'_Xv.Rds')))
}
