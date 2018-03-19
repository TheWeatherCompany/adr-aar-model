################################################################################
## Name: TRAIN_OVA_BINARY_SVM.R
## Description: Train binary model for each high frequency rate (OVA) -- RF
## Date: Mar 19, 2018
## Author: jaf
##
## Notes: No testing data; Manually create folds for cross-validation
##        Added weights on change observations
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
  model <- 'M4'
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
  
  ## identify changes -- only select those that include the rate being modeled
  Y$change <- ifelse((Y$rate_change == 1 | Y$rate_change == -1) & (Y[,response] == r_num | Y$rate_lag == r_num), 1, 0)
  change <- sum(Y$change)
  no_change <- nrow(Y) - sum(Y$change)
  ratio <- change / nrow(Y)
  Y$change <- factor(Y$change, levels = c(0, 1), labels = c("no_change","change"))
  
  ## weight vector
  model_weights <- ifelse(Y$change == "no_change", 1, 1 / ratio * 10)
  
  ## set train control -- 5-fold cross validation
  ctrl <- trainControl(method = "cv"
                       , number = nfold
                       , index = group_folds
                       , allowParallel = TRUE
                       , classProbs =  TRUE
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
    , weights = model_weights
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
  
  
  ### model results
  final_pred <- data.frame(
    "dt" = DT$horizon_data.DT,
    "pred" = (as.character(predict(fit, X_rate))),
    "act" = (as.character(Y_rate)),
    "rate_change" = Y$change
  )
  # final_pred <- cbind(final_pred, X_probs)
  confusionMatrix(final_pred$pred, final_pred$act)
  ## look at metrics on the change data
  final_pred$dt <- as.POSIXct(strptime(x = final_pred$dt, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  final_pred$dt_lag <- shift(x = final_pred$dt, n = 1, type = "lag")
  final_pred$time_diff <- final_pred$dt - final_pred$dt_lag
  
  ## calculate pred change
  final_pred$pred_lag <- shift(x = final_pred$pred,n = 1L,fill = NA,type = "lag")
  ## identify significant predicted changes
  final_pred$pred_change <- ifelse(final_pred$time_diff != 1, NA,
                                   ifelse(final_pred$pred != final_pred$pred_lag, 1, 0))
  final_pred$pred_change <- factor(final_pred$pred_change, levels = c(0, 1), labels = c("no_change","change"))
  ## determine the direction of predicted change
  confusionMatrix(final_pred$pred_change, final_pred$rate_change)
}
