################################################################################
## Name: TRAIN_OVA_BINARY_RF_SVM.R
## Description: Train binary model for each high frequency rate (OVA) -- RF and SVM
## Date: Feb 27, 2018
## Author: jaf
##
## Notes: 
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

# source("custom_functions.R")
# source("CARET_TRAIN_FUNCTIONS.R")

################################################
## set model parameters

RunBatch = 1

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
X[,c("runway_lag1")] <- NULL

## create dummies for categorical vars
################### create dummies for categorical variable
cat_vars <- sapply(X,FUN = is.numeric)
cat_vars <- names(cat_vars[which(cat_vars == FALSE)])
cat_vars <- cat_vars[!cat_vars %in% c('dt','datetime')] ## need to remove this variable

if(!is.null(cat_vars)){
  dum_create <- dummyVars(time ~ ., data = X[,names(X) %in% c("time",cat_vars)])
  dum_features <- data.frame(predict(dum_create, newdata = X))
  names(dum_features) <- gsub("\\.","_",names(dum_features))
  X <- data.frame(X, dum_features)
  X <- X[-which(names(X) %in% c(cat_vars,"Y"))]
}

cat_vars <- sapply(Xt,FUN = is.numeric)
cat_vars <- names(cat_vars[which(cat_vars == FALSE)])
cat_vars <- cat_vars[!cat_vars %in% c('dt','datetime')] ## need to remove this variable

if(!is.null(cat_vars)){
  dum_create <- dummyVars(time ~ ., data = Xt[,names(Xt) %in% c("time",cat_vars)])
  dum_features <- data.frame(predict(dum_create, newdata = Xt))
  names(dum_features) <- gsub("\\.","_",names(dum_features))
  Xt <- data.frame(Xt, dum_features)
  Xt <- Xt[-which(names(Xt) %in% c(cat_vars,"Y"))]
}

################################################ xgb - binary:logistic for each rate
# r <- rates[1]
for(r in rates){
  ## prepare data for feature selection
  Y_rate <- factor(Y[,r])
  X_rfe <- X
  
  ## set rfe controls
  rfe_control <- rfeControl(functions = rfFuncs
                            , method = "repeatedcv"
                            , number = 5
                            , allowParallel = TRUE)
  ## recursive feature elimination w/ random forest function
  cl <- makeCluster(16)
  registerDoParallel(cl)
  start_time <- Sys.time()
  rfe_results <- rfe(x = X_rfe
                     , y = Y_rate
                     , sizes = seq(from = 5, to = 50, by = 5)
                     , rfeControl = rfe_control
                     , seed = seed)
  stopCluster(cl)
  print(Sys.time() - start_time)
  
  print(rfe_results)
  rfe_results$results
  predictors(rfe_results)
  plot(rfe_results, type=c("g", "o"))
  
  ## select the most important features
  var_importance <- predictors(rfe_results)
  
  ## save variables to file
  var_importance_file <- file.path(getwd(), "2_train", model, "variable_importance", paste0(paste(airport, rate, model, horizon, r, sep = "_"), ".txt"))
  write.table(var_importance, file = var_importance_file)
  
  ## read in variable importance (if already run)
  # var_importance <- read.table(var_importance_file)
  # var_importance <- var_importance$x
  
  ## subset predictors to most important
  # Y_rate <- factor(Y[1:1000,r])
  # Yt_rate <- factor(Yt[1:1000,r])
  # X_rate <- X[1:1000,var_importance]
  # Xt_rate <- Xt[1:1000,var_importance]
  Y_rate <- factor(Y[,r])
  Yt_rate <- factor(Yt[,r])
  X_rate <- X[,var_importance]
  Xt_rate <- Xt[,var_importance]
  
  ## model training
  ctrl <- trainControl(method = "repeatedcv"
                       , repeats = 5
                       , allowParallel = TRUE
                       # , sampling = "smote"
  )
  ## train & tune the random forest
  cl <- makeCluster(7)
  registerDoParallel(cl)
  start_time <- Sys.time()
  grid_rf <- expand.grid(mtry = seq(5, 50, 5))
  fit_rf <- train(
    y = Y_rate
    , x = X_rate
    , method = "rf"
    # , tuneGrid = grid_rf
    , tuneLength = 10
    , trControl=ctrl)
  stopCluster(cl)
  print(Sys.time() - start_time)
  print(fit_rf)
  fit_rf$results
  plot(fit_rf)
  
  ## train & tune the support vector machine
  cl <- makeCluster(16)
  registerDoParallel(cl)
  start_time <- Sys.time()
  grid_svm <- expand.grid("C" = 2^c(0:5), "sigma" = 2^c(-25, -20, -15,-10, -5, 0))
  fit_svm <- train(
    y = Y_rate
    , x = X_rate
    , method = "svmRadialSigma"
    # , tuneGrid = grid_svm
    , tuneLength = 10
    # , preProc = c("center","scale")
    , trControl=ctrl)
  stopCluster(cl)
  print(Sys.time() - start_time)
  print(fit_svm)
  fit_svm$results
  plot(fit_svm)
  
  results <- resamples(list(RF = fit_rf, SVM = fit_svm))
  results_summary <- summary(results)
  ## boxplots of results
  bwplot(results)
  ## dot plots of results
  dotplot(results)
  
  final_pred <- data.frame(
    "pred_rf" = predict(fit_rf$finalModel, Xt_rate),
    "pred_svm" = predict(fit_svm$finalModel, Xt_rate),
    "act" = Yt_rate
  )
  confusionMatrix(final_pred$pred_rf, final_pred$act)
  confusionMatrix(final_pred$pred_svm, final_pred$act)

  ## save final model parameters
  filename <- file.path(dir,"2_train",model,"rf/rf_results.csv")
  write.table(fit_rf$results
              , file = filename
              , row.names=F
              , eol = "\r"
              , na="NA"
              , append=T
              , quote= FALSE
              , sep=","
              , col.names=F)
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
  
  ## save final models
  rf_model_file <- file.path(dir,"2_train",model,"rf",paste0(airport,"_",rate,"_",model_num,"_",r,'_rf_model.rda'))
  saveRDS(fit_rf$finalModel, file = rf_model_file)

  svm_model_file <- file.path(dir,"2_train",model,"svm",paste0(airport,"_",rate,"_",model_num,"_",r,'_svm_model.rda'))
  saveRDS(fit_svm$finalModel, file = svm_model_file)
}

