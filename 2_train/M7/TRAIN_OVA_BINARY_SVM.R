################################################################################
## Name: TRAIN_OVA_BINARY_SVM.R
## Description: Train binary model for each high frequency rate (OVA) -- RF
## Date: Mar 22, 2018
## Author: jaf
##
## Notes: No testing data; Manually create folds for cross-validation
##        Tuning grid - weights on rate observations (no mtry tuning)
##        Specificity threshold set; if initial model meets then do not tune
##        If initial model DOES NOT meet, then run through grid of weights
################################################################################
### load libraries
suppressMessages(library(caret))
suppressMessages(library(plyr))
suppressMessages(library(reshape2))
suppressMessages(library(data.table))
suppressMessages(library(doParallel))
suppressMessages(library(tidyverse))
suppressMessages(library(mlbench))
suppressMessages(library(pROC))
suppressMessages(library(plotROC))

Sys.setenv(TZ="America/New_York")

setwd("../../")
dir <- getwd()

################################################
## set model parameters

RunBatch = 0

if(RunBatch == 0){
  user <- 'jfinn'
  airport <- 'LGA'
  response <- 'ARR_RATE'
  model <- 'M7'
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
spec_threshold <- 0.5
weight_interval <- 5

################################################
## load in model datasets 
X <- file.path(file.path(dir,'1_data_prep',model),paste0(airport,"_",rate,'_',model_num,'_X.Rds'))
X <- readRDS(file = X)

Y <- file.path(file.path(dir,'1_data_prep',model),paste0(airport,"_",rate,'_',model_num,'_Y.Rds'))
Y <- readRDS(file = Y)

DT <- file.path(file.path(dir,'1_data_prep',model),paste0(airport,"_",rate,'_',model_num,'_DT.Rds'))
DT <- readRDS(file = DT)

## for testing purposes
# X <- X[1:5000,]
# Y <- Y[1:5000,]

rates <- names(Y)[!names(Y) %in% c(response,"rate_lag","rate_dt","rate_dt_pct","rate_change")]
print(rates)

## define train / test -- split dataset into even nfold groups where no partition repeated
index <- as.numeric(rownames(X))
nfold <- 5
groups <- cut_number(index, nfold)
## create list of indices
group_folds <- groupKFold(groups, k = nfold)

################################################ SVM -- RFE with random forest
r <- rates[1]
for(r in rates){
  print(r)
  r_num <- as.numeric(gsub("rate_", "", r))
  
  ## identify rate -- get ratio of rate observations to total samples
  rate_total <- sum(Y[,r])
  ratio <- rate_total / nrow(Y)
  
  ## set train control -- 5-fold cross validation
  ctrl <- trainControl(method = "cv"
                       , number = nfold
                       , index = group_folds
                       , allowParallel = TRUE
                       , classProbs =  TRUE
                       , savePredictions = "all"
                       , summaryFunction = twoClassSummary
  )
  
  ## subset predictors to most important -- FUTURE STEP
  Y_rate <- factor(Y[,r], levels = c("1", "0"), labels = c("yes", "no"))
  X_rate <- X
  
  ## set tune grid
  tunegrid <- expand.grid(
    "C" = 0.25, 
    "sigma" = 0.006877942,
    # "Weight" = c(1 / ratio)
    "Weight" = seq(0, 20, by = 5)
  )
  
  ## fit initial model with no weighting / sampling schema
  cl <- makeCluster(detectCores() - 1)
  registerDoParallel(cl)
  start_time <- Sys.time()
  fit_wt <- train(y = Y_rate
                  , x = X_rate
                  , method = "svmRadialWeights"
                  , trControl = ctrl
                  , tuneGrid = tunegrid
                  # , tuneLength = 4
                  # , weights = model_weights
                  , metric = "Sens"
                  , preProc = c("center", "scale")
  )
  stopCluster(cl)
  print(Sys.time() - start_time)
  
  fit_wt
  fit_wt$finalModel
  pred <- fit_wt$pred %>% filter(C == fit_wt$bestTune$C & sigma == fit_wt$bestTune$sigma & Weight == fit_wt$bestTune$Weight)
  # print(roc(pred$obs, pred$yes) %>% auc())
  print(confusionMatrix(pred$pred, pred$obs))
  plot(varImp(fit_wt))
  
  pred <- fit_wt$pred %>% filter(Weight != 1)
  print(confusionMatrix(pred$pred, pred$obs))
  
  
  ## plot ROC curves
  p1 <- pred %>%
    ggplot(aes(m = yes, d = factor(obs))) + 
    geom_roc(hjust = -0.4, vjust = 1.5) + coord_equal()
  plot_file <- file.path(getwd(), "2_train", model, "svm/roc_plots", paste0(paste(airport, rate, model, horizon, r, sep = "_"), ".png"))
  # dev.copy(png, plot_file, width = 800, height = 600)
  png(filename = plot_file)
  plot(p1)
  dev.off()
  
  ## save final model parameters
  results_out <- fit_wt$results
  results_out$airport <- airport
  results_out$response <- response
  results_out$model <- model
  results_out$horizon <- horizon
  results_out$rate <- r
  results_out$ratio <- ratio
  # results_out$ <- i
  # results_out$weight <- max(model_weights)
  filename <- file.path(dir,"2_train",model,"svm/rf_results.csv")
  write.table(results_out
              , file = filename
              , row.names=F
              , eol = "\r"
              , na="NA"
              , append=T
              , quote= FALSE
              , sep=","
              , col.names=F)
  
  ## save final model object
  svm_model_file <- file.path(dir,"2_train",model,"svm",paste0(airport,"_",rate,"_",model_num,"_",r,'_svm_model.rda'))
  saveRDS(fit_wt, file = svm_model_file)
  
}

