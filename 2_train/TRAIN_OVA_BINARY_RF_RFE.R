################################################################################
## Name: TRAIN_OVA_BINARY_SVM.R
## Description: Train binary model for each high frequency rate (OVA) -- RF
## Date: Mar 12, 2018
## Author: jaf
##
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

RunBatch = 0

if(RunBatch == 0){
  user <- 'jfinn'
  airport <- 'LGA'
  response <- 'ARR_RATE'
  model <- 'M2'
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
print(rates)
              
################################################ SVM -- RFE with random forest
r <- rates[1]
for(r in rates){
  print(r)
  
  ## prepare data for feature selection
  Y_rfe <- factor(Y[,r], levels = c("0", "1"), labels = c("no", "yes"))
  X_rfe <- X
  
  ## identify changes -- only select those that include the rate being modeled
  r_num <- as.numeric(gsub("rate_","",r))
  Y$change <- ifelse((Y$rate_change == 1 | Y$rate_change == -1) & (Y[,response] == r_num | Y$rate_lag == r_num), 1, 0)
  Yt$change <- ifelse((Yt$rate_change == 1 | Yt$rate_change == -1) & (Yt[,response] == r_num | Yt$rate_lag == r_num), 1, 0)
  change <- sum(Y$change)
  no_change <- nrow(Y) - sum(Y$change)
  ratio <- change / nrow(Y)
  Y$change <- factor(Y$change, levels = c(0, 1), labels = c("no_change","change"))
  Yt$change <- factor(Yt$change, levels = c(0, 1), labels = c("no_change","change"))
  
  ## weight vector
  model_weights <- ifelse(Y$change == "no_change", 1, 1 / ratio * 10)

  ## set rfe controls -- 5-fold cross-validation
  rfe_control <- rfeControl(functions = rfFuncs
                            , method = "cv"
                            , number = 5
                            , allowParallel = TRUE)
  ## recursive feature elimination w/ random forest function
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  start_time <- Sys.time()
  rfe_results <- rfe(x = X_rfe
                     , y = Y_rfe
                     , scale = FALSE
                     , sizes = seq(from = 5, to = 30, by = 5)
                     , rfeControl = rfe_control
                     , weights = model_weights
                     , seed = seed)
  stopCluster(cl)
  print(Sys.time() - start_time)

  print(rfe_results)
  rfe_results$results
  predictors(rfe_results)
  plot(rfe_results, type=c("g", "o"))

  ## select the most important features
  var_importance <- predictors(rfe_results)
  ## if the model selects all features -- just subset the top 30
  if(length(var_importance) == ncol(X_rfe)){
    selectedVars <- rfe_results$variables
    var_importance <- rfe_results$control$functions$selectVar(selectedVars, 15)
  }

  ## save variables to file
  var_importance_file <- file.path(getwd(), "2_train", model, "rf/variable_importance", paste0(paste(airport, rate, model, horizon, r, sep = "_"), ".txt"))
  write.table(var_importance, file = var_importance_file)

  ## set train control -- 5-fold cross validation
  ctrl <- trainControl(method = "cv"
                       , number = 5
                       , allowParallel = TRUE
                       , classProbs =  TRUE
  )
  
  ## subset predictors to most important
  Y_rate <- factor(Y[,r], levels = c("0", "1"), labels = c("no", "yes"))
  X_rate <- X[,var_importance]
  
  ## tuning grid
  mtry <- seq(from = 5, to = 30, by = 5)
  tunegrid <- expand.grid(.mtry=mtry)
  ## train & tune the random forest
  cl <- makeCluster(detectCores())
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
  plot(fit)
  fit_varImp <- varImp(fit, scale=FALSE)
  plot(fit_varImp)
  
  ## ROC curve
  library(pROC)
  probsTrain <- predict(fit, X_rate, type = "prob")
  rocCurve   <- roc(response = Y_rate,
                    predictor = probsTrain[, "yes"],
                    levels = rev(levels(Y_rate)))
  plot(rocCurve, print.thres = "best")
  
  ## check change detection
  train_probs <- predict(fit, X_rate, type = "prob")
  train_pred <- predict(fit, X_rate)
  train_probs <- cbind(Y, Y_rate, train_probs, train_pred)
  confusionMatrix(train_probs$train_pred, train_probs$Y_rate)
  
  ## prediction on test set
  Yt_rate <- factor(Yt[,r], levels = c("0", "1"), labels = c("no", "yes"))
  Xt_rate <- Xt[,var_importance]
  final_pred <- data.frame(
    "dt" = DTt$horizon_data.DTt,
    "pred" = predict(fit$finalModel, Xt_rate),
    # "act" = factor(Yt[,r], levels = c(0, 1), labels = c("nc","c"))
    "act" = Yt_rate,
    "rate" = Yt[,response],
    "change" = Yt$change
  )
  probs <- predict(fit$finalModel, Xt_rate, type = "prob")
  final_pred <- cbind(final_pred, probs, Xt_rate)
  confusionMatrix(final_pred$pred, final_pred$act)
  
  ## change metrics
  change <- final_pred[which(final_pred$change == "change"),]
  confusionMatrix(change$pred, change$act)
  
  probs <- predict(fit$finalModel, Xt_rate, type = "prob")
  probs <- cbind(DTt$horizon_data.DTt, probs, Yt)

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
