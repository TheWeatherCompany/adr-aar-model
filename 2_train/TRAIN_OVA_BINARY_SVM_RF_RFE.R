################################################################################
## Name: TRAIN_OVA_BINARY_SVM.R
## Description: Train binary model for each high frequency rate (OVA) -- SVM
## Date: Feb 27, 2018
## Author: jaf
##
## Notes: Feature engineering & processing specific to SVM
##        Categorical variables converted to numeric (ordinal)
##        Binary variables == 0 or 1
##        All features centered & scaled
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

################################################ SVM -- RFE with random forest
# r <- rates[2]
for(r in rates){
  print(r)
  ## prepare data for feature selection
  Y_rfe <- factor(Y[,r])
  X_rfe <- X

  ## set rfe controls
  rfe_control <- rfeControl(functions = rfFuncs
                            , method = "cv"
                            , number = 5
                            # , repeats = 3
                            , allowParallel = TRUE)
  ## recursive feature elimination w/ random forest function
  cl <- makeCluster(detectCores() / 2)
  registerDoParallel(cl)
  start_time <- Sys.time()
  rfe_results <- rfe(x = X_rfe
                     , y = Y_rfe
                     , scale = FALSE
                     , sizes = seq(from = 5, to = 30, by = 5)
                     , rfeControl = rfe_control
                     , seed = seed)
  stopCluster(cl)
  print(Sys.time() - start_time)

  print(rfe_results)
  rfe_results$results
  predictors(rfe_results)
  # plot(rfe_results, type=c("g", "o"))
  rfe_results$fit$importance
  
  ## select the most important features
  var_importance <- predictors(rfe_results)
  ## if the model selects all features -- just subset the top 30
  if(length(var_importance) == ncol(X)){
    selectedVars <- rfe_results$variables
    var_importance <- rfe_results$control$functions$selectVar(selectedVars, 30)
  }
  
  ## save variables to file
  var_importance_file <- file.path(getwd(), "2_train", model, "variable_importance", paste0(paste(airport, rate, model, horizon, r, sep = "_"), ".txt"))
  write.table(var_importance, file = var_importance_file)

  ## subset predictors to most important
  Y_rate <- factor(Y[,r], levels = c("0", "1"), labels = c("no", "yes"))
  X_rate <- X[,var_importance]
  
  ## set train control
  ctrl <- trainControl(method = "cv"
                       , number = 10
                       # , repeats = 5
                       , allowParallel = TRUE
                       # , sampling = "up"
                       , classProbs =  TRUE
                       # , summaryFunction = twoClassSummary
  )
  ## train & tune the support vector machine
  cl <- makeCluster(detectCores() / 2)
  registerDoParallel(cl)
  start_time <- Sys.time()
  fit_svm <- train(
    y = Y_rate
    , x = X_rate
    , method = "svmRadialSigma"
    # , metric = "ROC"
    , tuneLength = 10
    , trControl = ctrl
    , seed = seed
  )
  stopCluster(cl)
  print(Sys.time() - start_time)

  ## review model results
  print(fit_svm)
  fit_svm$results
  # plot(fit_svm)
  fit_varImp <- varImp(fit_svm, scale=FALSE)
  # plot(fit_varImp)

  ## save final model parameters
  filename <- file.path(dir,"2_train",model,"svm/svm_results.csv")
  write.table(fit_svm$results
              , file = filename
              , row.names=F
              , eol = "\r"
              , na="NA"
              , append=T
              , quote= FALSE
              , sep=","
              , col.names=F)

  svm_model_file <- file.path(dir,"2_train",model,"svm",paste0(airport,"_",rate,"_",model_num,"_",r,'_svm_model.rda'))
  saveRDS(fit_svm, file = svm_model_file)
}
