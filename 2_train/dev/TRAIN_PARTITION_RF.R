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
# suppressMessages(library(xgboost))
suppressMessages(library(reshape2))
suppressMessages(library(data.table))
suppressMessages(library(doParallel))
suppressMessages(library(tidyverse))
suppressMessages(library(gridExtra))
suppressMessages(library(gtools))

Sys.setenv(TZ="America/New_York")

# source("custom_functions.R")
# source("TRAIN_FUNCTIONS.R") 

################################################
## set model parameters

RunBatch = 0

if(RunBatch == 0){
  user <- 'jfinn'
  airport <- 'SFO'
  response <- 'ARR_RATE'
  model <- 'M1'
  horizon <- 'H12'
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

## get the models we need to create
rate_part <- suppressMessages(read_csv(file.path(dir, "1_data_prep/rate_partition.csv")))
rate_part <- rate_part %>% filter(locid == airport & rate == response)

## create combinations of rates -- pairwise models
# rates <- names(Y)[!names(Y) %in% c(response,"rate_lag","rate_dt","rate_dt_pct","rate_change")]
# pw_models <- c()
# for(r in rates){
#   rates <- rates[which(rates != r)]
#   temp <- merge(rates, r)
#   pw_models <- rbind(pw_models, temp)
#   rm(r, temp)
# }
# names(pw_models) <- c("rate_1", "rate_2")
# pw_models$rate_1 <- as.character(pw_models$rate_1)
# pw_models$rate_2 <- as.character(pw_models$rate_2)

################################################ xgb - binary:logistic for each rate
# r <- rates[1]
for(i in c(1:nrow(rate_part))){
  ## set rates
  partition <- rate_part[i,]
  left <- unlist(strsplit(partition$left, ", "))
  right <- unlist(strsplit(partition$right, ", "))
  partition <- partition$partition
  
  mnum <- partition
  
  ## subset data -- rates for this partition; create new variables
  Y_model <- Y[which(Y[,response] %in% c(left, right)),]
  train_idx <- rownames(Y_model)
  Y_model <- ifelse(Y_model[,response] %in% left, "left", "right")
  Y_model <- factor(Y_model)
  
  Yt_model <- Yt[which(Yt[,response] %in% c(left, right)),]
  test_idx <- rownames(Yt_model)
  Yt_model <- ifelse(Yt_model[,response] %in% left, "left", "right")
  Yt_model <- factor(Yt_model)
  
  X_model <- X[which(rownames(X) %in% train_idx),]
  Xt_model <- Xt[which(rownames(Xt) %in% test_idx),]
  
  ctrl <- trainControl(method = "repeatedcv"                  # 10-fold cross validation
                       , repeats = 5	                        # do 5 repititions of cv
                       , summaryFunction = twoClassSummary	  # Use AUC to pick the best model
                       , classProbs = TRUE
                       , allowParallel = TRUE
                       # , sampling = "smote"
                       # , summaryFunction = kss_metric
  )
  ## train & tune the svm
  cl <- makeCluster(7)
  registerDoParallel(cl)
  grid <- expand.grid(maxdepth = seq(3, 15, 3), mincriterion = 0.50)
  grid <- expand.grid(mtry = seq(5, 15, 5))
  start_time <- Sys.time()
  fit <- train(y = Y_model
                   , x = X_model
                   , method = "rf"                    # random forest
                   # , tuneLength = 9					        # 9 values of the cost function
                   # , preProc = c("center","scale")  # Center and scale data
                   , metric = "ROC"
                   , tuneGrid = grid
                   # , metric = c('KSS')
                   , trControl=ctrl)
  train <- fit
  rm(fit)
  stopCluster(cl)
  print(Sys.time() - start_time)
  
  print(train)
  train$results
  
  plot(train)
  
  featureImp <- varImp(train, scale=FALSE)
  plot(featureImp)
  
  final_pred <- data.frame(
    # "dt" = DTt$horizon_data.DTt,
    "pred" = predict(train$finalModel, Xt_model),
    # "act" = factor(Yt[,r], levels = c(0, 1), labels = c("nc","c"))
    "act" = Yt_model
  )
  confusionMatrix(final_pred$pred, final_pred$act)

  model_file <- file.path(dir,'results/models',paste0(airport,"_",rate,"_",model_num,"_",mnum,"rf_model.rda"))
  saveRDS(train, file = model_file)
}
