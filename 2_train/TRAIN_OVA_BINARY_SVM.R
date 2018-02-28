################################################################################
## Name: TRAIN_OVA_BINARY_RF_SVM.R
## Description: Train binary model for each high frequency rate (OVA) -- SVM
## Date: Feb 27, 2018
## Author: jaf
##
## Notes: Feature engineering & processing specific to SVM
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
rem_vars <- c("wxcodes","wdir","wdir_sin","ceiling","wx_light","wx_heavy","lifr","ifr","mvfr","vfr","runway_lag1"
              ,"hr_local","weekday","month")
X[,c(rem_vars)] <- NULL
Xt[,c(rem_vars)] <- NULL

################### categorical variable
## for SVM -- factorize categorical variables into numeric
X$wdir_cat <- as.numeric(factor(X$wdir_cat, levels = c("N","NE","E","SE","S","SW","W","NW"), labels = c(1:8)))
X$flightrule <- as.numeric(factor(X$flightrule, levels = c("LIFR","IFR","MVFR","VFR"), labels = c(1:4)))
X$ceilingflightrule <- as.numeric(factor(X$ceilingflightrule, levels = c("LIFR","IFR","MVFR","VFR"), labels = c(1:4)))
X$visflightrule <- as.numeric(factor(X$visflightrule, levels = c("LIFR","IFR","MVFR","VFR"), labels = c(1:4)))
X$wx_obscur <- as.numeric(X$wx_obscur)
X$wx_precip <- as.numeric(X$wx_precip)
X$wx_convec <- as.numeric(X$wx_convec)
X$wx_other <- as.numeric(as.factor(X$wx_other))

Xt$wdir_cat <- as.numeric(factor(Xt$wdir_cat, levels = c("N","NE","E","SE","S","SW","W","NW"), labels = c(1:8)))
Xt$flightrule <- as.numeric(factor(Xt$flightrule, levels = c("LIFR","IFR","MVFR","VFR"), labels = c(1:4)))
Xt$ceilingflightrule <- as.numeric(factor(Xt$ceilingflightrule, levels = c("LIFR","IFR","MVFR","VFR"), labels = c(1:4)))
Xt$visflightrule <- as.numeric(factor(Xt$visflightrule, levels = c("LIFR","IFR","MVFR","VFR"), labels = c(1:4)))
Xt$wx_obscur <- as.numeric(Xt$wx_obscur)
Xt$wx_precip <- as.numeric(Xt$wx_precip)
Xt$wx_convec <- as.numeric(Xt$wx_convec)
Xt$wx_other <- as.numeric(as.factor(Xt$wx_other))

## pre-process -- center and scale
preProc <- preProcess(X, method = c("scale", "center"))
X <- predict(preProc, X)
Xt <- predict(preProc, Xt)

## remove features with near-zero variance
# nzv <- as.data.frame(nearZeroVar(X,saveMetrics = TRUE))
# nzv <- nzv[nzv$nzv == TRUE,]
# nzv <- row.names(nzv)
# print(nzv)
# X[,c(nzv)] <- NULL
# Xt[,c(nzv)] <- NULL

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
  
  ## subset predictors to most important
  Y_rate <- factor(Y[,r])
  Yt_rate <- factor(Yt[,r])
  X_rate <- X[,var_importance]
  Xt_rate <- Xt[,var_importance]

  ## model training
  ctrl <- trainControl(method = "cv"
                       , repeats = 5
                       , allowParallel = TRUE
                       # , sampling = "smote"
  )
  ## train & tune the support vector machine
  cl <- makeCluster(7)
  registerDoParallel(cl)
  start_time <- Sys.time()
  ## define parameter tuning grid (or user tuneLenght = N)
  grid_svm <- expand.grid("C" = 2^c(0:5), "sigma" = 2^c(-25, -20, -15,-10, -5, 0))
  fit_svm <- train(
    y = Y_rate
    , x = X_rate
    , method = "svmRadialSigma"
    # , tuneGrid = grid_svm
    , tuneLength = 10
    , trControl=ctrl)
  stopCluster(cl)
  print(Sys.time() - start_time)
  
  ## review model results
  print(fit_svm)
  fit_svm$results
  plot(fit_svm)
  fit_varImp <- varImp(fit_svm, scale=FALSE)
  plot(fit_varImp)
  
  final_pred <- cbind(Yt, Xt_rate)
  final_pred <- na.omit(final_pred)
  final_pred$pred <- predict(fit_svm$finalModel,  na.omit(Xt_rate))
  # final_pred <- data.frame(
  #   "pred_svm" = predict(fit_svm$finalModel, na.omit(Xt_rate)),
  #   "act" = Yt_rate
  # )
  print(confusionMatrix(final_pred$pred, final_pred[,r]))
  
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
  saveRDS(fit_svm$finalModel, file = svm_model_file)
}
