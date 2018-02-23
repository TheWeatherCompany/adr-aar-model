################################################################################
## Name: TRAIN_BINARY.R
## Description: Train xgb binary model for each high frequency rate
## Date: Feb 7, 2018
## Author: jaf
##
## Notes: 
################################################################################
### load libraries
suppressMessages(library(caret))
suppressMessages(library(plyr))
suppressMessages(library(xgboost))
suppressMessages(library(reshape2))
suppressMessages(library(data.table))
suppressMessages(library(doParallel))
suppressMessages(library(tidyverse))
suppressMessages(library(gridExtra))
suppressMessages(library(mlbench))

Sys.setenv(TZ="America/New_York")

# source("custom_functions.R")
source("TRAIN_FUNCTIONS.R")

################################################
## set model parameters

RunBatch = 0

if(RunBatch == 0){
  user <- 'jfinn'
  airport <- 'SFO'
  response <- 'ARR_RATE'
  model <- 'M1'
  horizon <- 'H1'
}

if(RunBatch == 1){
  args <- commandArgs(trailingOnly = TRUE)
  airport <- args[1]
  response <- args[2]
  model <- args[3]
  horizon <- args[4]
}

print(airport)
print(response)
print(model)
print(horizon)

model_num <- paste0(model,"_",horizon)
rate <- ifelse(response == "DEP_RATE","ADR","AAR")
response <- tolower(response)

seed <- 2187

################################################
## load in model datasets 
setwd("../")
dir <- getwd()

X <- file.path(file.path(dir,'1_data_prep',model),paste0(airport,"_",rate,'_',model_num,'_X.Rds'))
X <- readRDS(file = X)
Xt <- file.path(file.path(dir,'1_data_prep',model),paste0(airport,"_",rate,'_',model_num,'_Xt.Rds'))
Xt <- readRDS(file = Xt)

Y <- file.path(file.path(dir,'1_data_prep',model),paste0(airport,"_",rate,'_',model_num,'_Y.Rds'))
Y <- readRDS(file = Y)
Yt <- file.path(file.path(dir,'1_data_prep',model),paste0(airport,"_",rate,'_',model_num,'_Yt.Rds'))
Yt <- readRDS(file = Yt)

DT <- file.path(file.path(dir,'1_data_prep',model),paste0(airport,"_",rate,'_',model_num,'_DT.Rds'))
DT <- readRDS(file = DT)
DTt <- file.path(file.path(dir,'1_data_prep',model),paste0(airport,"_",rate,'_',model_num,'_DTt.Rds'))
DTt <- readRDS(file = DTt)

rates <- names(Y)[!names(Y) %in% c(response,"rate_lag","rate_dt","rate_dt_pct","rate_change")]

## remove some vars
X$runway_lag1 <- NULL

################################################ xgb - binary:logistic for each rate
# r <- rates[1]
for(r in rates){
  Y_rate <- factor(Y[,r])
  Yt_rate <- factor(Yt[,r])
  
  Y_rate <- Y_rate[1:5000]
  X <- X[1:5000,]
  
  ## feature selection
  fs_control <- rfeControl(functions = rfFuncs, method = "cv", number = 2)
  fs_results <- rfe(x = X, y = Y_rate, sizes = seq(from = 5, to = 50, by = 5), rfeControl = fs_control)
  print(fs_results)
  predictors(fs_results)
  plot(fs_results, type=c("g", "o"))
  length(varImp(fs_results))
  
  ## model training
  training_results <- xgb_train(X, Xt, Y_rate, Yt_rate, featureSelect, 20, 5, 50, "binary:logistic", "auc")
  training_results$diff <- (training_results$test_auc_mean - training_results$train_auc_mean)
  training_results$airport <- airport
  training_results$response <- response
  training_results$model <- model
  training_results$horizon <- horizon
  training_results$rate <- r
  
  ## save final model parameters
  filename <- file.path(dir,'results/xgbBinary_results.csv')
  write.table(training_results
              , file = filename
              , row.names=F
              , eol = "\r"
              , na="NA"
              , append=T
              , quote= FALSE
              , sep=","
              , col.names=F)
  
  ## select final model: results with 1) smallest test / train difference and 2) test error > train error 
  # output <- training_results %>% filter(iter == 10)
  # output <- training_results %>% filter(diff <= 0) %>% filter(diff == max(diff))
  output <- training_results %>% filter(diff <= 0) %>% filter(test_auc_mean == max(test_auc_mean))
  
  ## create and export final model
  xgb_final(X, Xt, Y_rate, Yt_rate, featureSelect, output, 10, "binary:logistic", "auc")
}
