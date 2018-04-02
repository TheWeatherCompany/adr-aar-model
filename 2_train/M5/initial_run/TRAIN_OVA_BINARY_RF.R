################################################################################
## Name: TRAIN_OVA_BINARY_SVM.R
## Description: Train binary model for each high frequency rate (OVA) -- RF
## Date: Mar 12, 2018
## Author: jaf
##
## Notes: No testing data; Manually create folds for cross-validation
################################################################################
### load libraries
suppressMessages(library(caret))
suppressMessages(library(plyr))
suppressMessages(library(reshape2))
suppressMessages(library(data.table))
suppressMessages(library(doParallel))
suppressMessages(library(tidyverse))
suppressMessages(library(mlbench))

Sys.setenv(TZ="America/New_York")

setwd("../")
dir <- getwd()

################################################
## set model parameters

RunBatch = 1

if(RunBatch == 0){
  user <- 'jfinn'
  airport <- 'LGA'
  response <- 'ARR_RATE'
  model <- 'M3'
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
X <- file.path(file.path(dir,'1_data_prep',model),paste0(airport,"_",rate,'_',model_num,'_X.Rds'))
X <- readRDS(file = X)

Y <- file.path(file.path(dir,'1_data_prep',model),paste0(airport,"_",rate,'_',model_num,'_Y.Rds'))
Y <- readRDS(file = Y)

DT <- file.path(file.path(dir,'1_data_prep',model),paste0(airport,"_",rate,'_',model_num,'_DT.Rds'))
DT <- readRDS(file = DT)

rates <- names(Y)[!names(Y) %in% c(response,"rate_lag","rate_dt","rate_dt_pct","rate_change")]
print(rates)

## define train / test -- split dataset into even nfold groups where no partition repeated
index <- as.numeric(rownames(X))
nfold <- 5
groups <- cut_number(index, 5)
## create list of indices
group_folds <- groupKFold(groups, k = nfold)

################################################ SVM -- RFE with random forest
for(r in rates){
  print(r)
  r_num <- as.numeric(gsub("rate_", "", r))
  
  ## set train control -- 5-fold cross validation
  ctrl <- trainControl(method = "cv"
                       , number = nfold
                       , index = group_folds
                       , allowParallel = TRUE
                       , classProbs =  TRUE
                       , savePredictions = "all"
                       # , sampling = "smote"
  )
  
  ## subset predictors to most important
  Y_rate <- factor(Y[,r], levels = c("0", "1"), labels = c("no", "yes"))
  X_rate <- X
  
  ## tuning grid
  mtry <- seq(from = 5, to = 30, by = 5)
  tunegrid <- expand.grid(.mtry=mtry)
  ## train & tune the random forest
  cl <- makeCluster(detectCores() - 1)
  registerDoParallel(cl)
  start_time <- Sys.time()
  fit <- train(
    y = Y_rate
    , x = X_rate
    , method = "rf"
    , tuneGrid = tunegrid
    , trControl = ctrl
    , seed = seed
  )
  stopCluster(cl)
  print(Sys.time() - start_time)
  
  ## review model results
  print(fit)
  fit$results
  fit_varImp <- varImp(fit, scale=FALSE)
  
  ## save variables to file
  var_importance <- fit_varImp$importance
  var_importance_file <- file.path(getwd(), "2_train", model, "rf/variable_importance", paste0(paste(airport, rate, model, horizon, r, sep = "_"), ".txt"))
  write.table(var_importance, file = var_importance_file)
  
  ## save final model parameters
  results_out <- fit$results
  results_out$airport <- airport
  results_out$response <- response
  results_out$model <- model
  results_out$horizon <- horizon
  results_out$rate <- r
  filename <- file.path(dir,"2_train",model,"rf/rf_results.csv")
  write.table(results_out
              , file = filename
              , row.names=F
              , eol = "\r"
              , na="NA"
              , append=T
              , quote= FALSE
              , sep=","
              , col.names=F)

  rf_model_file <- file.path(dir,"2_train",model,"rf",paste0(airport,"_",rate,"_",model_num,"_",r,'_rf_model.rda'))
  saveRDS(fit, file = rf_model_file)
}
