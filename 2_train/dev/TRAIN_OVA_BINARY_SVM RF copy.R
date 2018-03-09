################################################################################
## Name: TRAIN_OVA_BINARY_SVM.R
## Description: Train binary model for each high frequency rate (OVA) -- SVM
## Date: Mar 2, 2018
## Author: jaf
##
## Notes: Random forest feature selection
##        Feature engineering & processing specific to SVM
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
              # ,"hr_local","weekday","month"
              ,"time")
X[,c(rem_vars)] <- NULL
Xt[,c(rem_vars)] <- NULL

################### categorical variable
## for SVM -- factorize categorical variables into numeric
X$wdir_cat <- as.numeric(factor(X$wdir_cat, levels = c("N","NE","E","SE","S","SW","W","NW"), labels = c(1:8)))
X$flightrule <- as.numeric(factor(X$flightrule, levels = c("LIFR","IFR","MVFR","VFR"), labels = c(1:4)))
X$ceilingflightrule <- as.numeric(factor(X$ceilingflightrule, levels = c("LIFR","IFR","MVFR","VFR"), labels = c(1:4)))
X$visflightrule <- as.numeric(factor(X$visflightrule, levels = c("LIFR","IFR","MVFR","VFR"), labels = c(1:4)))
X$hr_local <- as.numeric(factor(X$hr_local, levels = c("00","01","02","03","04","05","06","07","08","09","10","11","12",
                                                       "13","14","15","16","17","18","19","20","21","22","23"),labels = c(0:23)))
X$month<- as.numeric(factor(X$month, levels = c("01","02","03","04","05","06","07","08","09","10","11","12"), labels = c(0:11)))
X$weekday <- as.numeric(factor(X$weekday, levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"), labels = c(0:6)))
# X$hr_local <- sin(pi * ((360 / 24) * X$hr_local) / 180)
# X$month <- sin(pi * ((360 / 12) * X$month) / 180)
# X$weekday <- sin(pi * ((360 / 7) * X$weekday) / 180)
X$wx_obscur <- as.numeric(as.character(X$wx_obscur))
X$wx_precip <- as.numeric(as.character(X$wx_precip))
X$wx_convec <- as.numeric(as.character(X$wx_convec))
X$wx_other <- as.numeric(X$wx_other)
# cat_vars <- names(X)[which(grepl("rwycnfg", names(X)) == T)]
# cat_vars <- c(cat_vars, "wx_obscur", "wx_precip", "wx_convec", "wx_other")
# for(var in cat_vars){X[,var] <- ifelse(X[var] == 0, -1, 1)}

Xt$wdir_cat <- as.numeric(factor(Xt$wdir_cat, levels = c("N","NE","E","SE","S","SW","W","NW"), labels = c(1:8)))
Xt$flightrule <- as.numeric(factor(Xt$flightrule, levels = c("LIFR","IFR","MVFR","VFR"), labels = c(1:4)))
Xt$ceilingflightrule <- as.numeric(factor(Xt$ceilingflightrule, levels = c("LIFR","IFR","MVFR","VFR"), labels = c(1:4)))
Xt$visflightrule <- as.numeric(factor(Xt$visflightrule, levels = c("LIFR","IFR","MVFR","VFR"), labels = c(1:4)))
Xt$hr_local <- as.numeric(factor(Xt$hr_local, levels = c("00","01","02","03","04","05","06","07","08","09","10","11","12",
                                                       "13","14","15","16","17","18","19","20","21","22","23"),labels = c(0:23)))
Xt$month<- as.numeric(factor(Xt$month, levels = c("01","02","03","04","05","06","07","08","09","10","11","12"), labels = c(0:11)))
Xt$weekday <- as.numeric(factor(Xt$weekday, levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"), labels = c(0:6)))
# Xt$hr_local <- sin(pi * ((360 / 24) * Xt$hr_local) / 180)
# Xt$month <- sin(pi * ((360 / 12) * Xt$month) / 180)
# Xt$weekday <- sin(pi * ((360 / 7) * Xt$weekday) / 180)
Xt$wx_obscur <- as.numeric(as.character(Xt$wx_obscur))
Xt$wx_precip <- as.numeric(as.character(Xt$wx_precip))
Xt$wx_convec <- as.numeric(as.character(Xt$wx_convec))
Xt$wx_other <- as.numeric(Xt$wx_other)
# cat_vars <- names(Xt)[which(grepl("rwycnfg", names(Xt)) == T)]
# cat_vars <- c(cat_vars, "wx_obscur", "wx_precip", "wx_convec", "wx_other")
# for(var in cat_vars){Xt[,var] <- ifelse(Xt[var] == 0, -1, 1)}

## pre-process -- center and scale
# preProc <- preProcess(X, method = c("scale", "center"))
preProc <- preProcess(X, method = "range", rangeBounds = c(-1, 1))
X <- predict(preProc, X)
Xt <- predict(preProc, Xt)

## remove features with near-zero variance
# nzv <- as.data.frame(nearZeroVar(X,saveMetrics = TRUE))
# nzv <- nzv[nzv$nzv == TRUE,]
# nzv <- row.names(nzv)
# print(nzv)
# X[,c(nzv)] <- NULL
# Xt[,c(nzv)] <- NULL
# X <- X[1:500,]
# Y <- Y[1:500,]
# Xt <- Xt[1:500,]
# Yt <- Yt[1:500,]

################################################ SVM -- RFE with random forest
r <- rates[1]
for(r in rates){
  print(r)
  ## prepare data for feature selection
  Y_rfe <- factor(Y[,r])
  X_rfe <- X

  ## set rfe controls
  rfe_control <- rfeControl(functions = rfFuncs
                            , method = "cv"
                            , number = 10
                            , repeats = 5
                            , allowParallel = TRUE)
  ## recursive feature elimination w/ random forest function
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  start_time <- Sys.time()
  rfe_results <- rfe(x = X_rfe
                     , y = Y_rfe
                     , sizes = seq(from = 5, to = 50, by = 5)
                     , rfeControl = rfe_control
                     , seed = seed)
  stopCluster(cl)
  print(Sys.time() - start_time)

  print(rfe_results)
  rfe_results$results
  predictors(rfe_results)
  plot(rfe_results, type=c("g", "o"))
  rfe_results$fit$importance

  ## select the most important features
  var_importance <- predictors(rfe_results)

  ## save variables to file
  var_importance_file <- file.path(getwd(), "2_train", model, "variable_importance", paste0(paste(airport, rate, model, horizon, r, sep = "_"), ".txt"))
  write.table(var_importance, file = var_importance_file)

  ## subset predictors to most important
  Y_rate <- factor(Y[,r], levels = c("0", "1"), labels = c("no", "yes"))
  Yt_rate <- factor(Yt[,r], levels = c("0", "1"), labels = c("no", "yes"))
  X_rate <- X[,var_importance]
  Xt_rate <- Xt[,var_importance]

  ## set train control
  ctrl <- trainControl(method = "repeatedcv"
                       , repeats = 5
                       , allowParallel = TRUE
                       # , sampling = "smote"
                       , classProbs =  TRUE
                       , summaryFunction = twoClassSummary
  )
  ## train & tune the support vector machine
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  start_time <- Sys.time()
  fit_svm <- train(
    y = Y_rate
    , x = X_rate
    , method = "svmRadialSigma"
    , metric = "ROC"
    , tuneLength = 10
    , trControl = ctrl
    , seed = seed
  )
  stopCluster(cl)
  print(Sys.time() - start_time)

  ## review model results
  print(fit_svm)
  fit_svm$results
  plot(fit_svm)
  fit_varImp <- varImp(fit_svm, scale=FALSE)
  plot(fit_varImp)

  pred <- predict(fit_svm, Xt_rate, type = "prob")

  ## print predictions on test set
  final_pred <- data.frame(
    "dt" = DTt$horizon_data.DTt,
    "pred_svm" = predict(fit_svm$finalModel, Xt_rate),
    "act" = Yt_rate
  )
  print(confusionMatrix(final_pred$pred_svm, final_pred$act))

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
