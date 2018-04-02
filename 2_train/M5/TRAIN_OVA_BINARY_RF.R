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

setwd("../")
dir <- getwd()

################################################
## define custom loss fuction
kss_metric <- function(data, lev = NULL, model = NULL){
  cm <- table(data$pred, data$obs)
  H <- as.integer(cm[2,2])
  FA <- as.integer(cm[2,1])
  M <- as.integer(cm[1,2])
  CN <- as.integer(cm[1,1])
  acc_val <- (H + CN) / (H + CN + FA + M)
  kss_val <- ((H * CN) - (FA * M))/((H + M)*(FA + CN))
  spc_val <- (CN / (CN + FA))
  sns_val <- (H / (M + H))
  c(KSS = kss_val,
    Accuracy = acc_val,
    Sensitivity = sns_val,
    Specificity = spc_val,
    Miss = M,
    Hit = H,
    CorrectNormal = CN,
    FalseAlarm = FA
  )
}

################################################
## set model parameters

RunBatch = 0

if(RunBatch == 0){
  user <- 'jfinn'
  airport <- 'LGA'
  response <- 'ARR_RATE'
  model <- 'M5'
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
spec_threshold <- 0.75
weight_interval <- 5

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

## remove operational features
X[,c("oag_arr","oag_dep")] <- NULL

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
  Y_rate <- factor(Y[,r], levels = c("0", "1"), labels = c("no", "yes"))
  X_rate <- X
  
  ## tune all models with weight
  model_weights <- ifelse(Y[,r] == 0, 1, 1 / ratio)
  
  ## initial tune with the same mtry, splitrule, and min.node.size value
  mtry <- 5
  splitrule <- 'gini'
  min.node.size <- 1
  tunegrid <- expand.grid(.mtry = mtry, .splitrule = splitrule, .min.node.size = min.node.size)
  
  ## fit initial model with no weighting / sampling schema
  cl <- makeCluster(detectCores() - 1)
  registerDoParallel(cl)
  start_time <- Sys.time()
  fit_init <- train(y = Y_rate
               , x = X_rate
               , method = "ranger"
               , num.trees = 100
               , trControl = ctrl
               , tuneGrid = tunegrid
               , weights = model_weights
               , seed = seed
               , metric = "ROC"
               , importance = "impurity"
  )
  stopCluster(cl)
  print(Sys.time() - start_time)
  ## save the seeds so we can replicate for weight training
  ctrl$seeds <- fit_init$control$seeds
  plot(varImp(fit_init))
  
  ## get specificity calculated on test set
  init_specificity <- confusionMatrix(fit_init$pred$pred, fit_init$pred$obs)$byClass['Specificity']
  print(init_specificity)

  ## if initial model meets specificity theshold, then save as final model
  model_list <- list("fit_init" = fit_init)
  if(init_specificity >= spec_threshold) fit_final <- fit_init

  ## if initial model does not meet specificity threshold, then weight rate observations
  if(init_specificity < spec_threshold){
    ## now, train models using different weights on the rate observations -- penalize misclassification
    cl <- makeCluster(detectCores() - 1)
    registerDoParallel(cl)
    start_time <- Sys.time()
    i <- 1
    ## start with weight == to ratio; iterate through until specificity threshold is met
    while(i < 500){
      print(paste("weight =", i))
      model_weights <- ifelse(Y[,r] == 0, 1, 1 / ratio * i)
      fit_wt <- train(y = Y_rate
                      , x = X_rate
                      , method = "ranger"
                      , num.trees = 100
                      , trControl = ctrl
                      , tuneGrid = tunegrid
                      , weights = model_weights
                      , metric = "ROC"
                      , importance = "impurity"
      )
      print(fit_wt)
      plot(varImp(fit_wt))
      ## get specificity calculated on test set
      specificity <- confusionMatrix(fit_wt$pred$pred, fit_wt$pred$obs)$byClass['Specificity']
      print(specificity)
      
      model_list[[paste0('fit_wt', i)]] <- fit_wt

      if(specificity >= spec_threshold){
        ## save final model once criteria is met
        fit_final <- fit_wt
        break
      }
      else{
        if(i == 1){i <- weight_interval}
        else{i <- i + weight_interval}
      }
    }
    stopCluster(cl)
    print(Sys.time() - start_time)

  }
  
  ## combine models to assess results
  summary(resamples(model_list))
  bwplot(resamples(model_list))
  dotplot(resamples(model_list))

  length(model_list)
  plot(varImp(model_list[[1]]))
  plot(varImp(model_list[[2]]))
  plot(varImp(model_list[[3]]))
  plot(varImp(model_list[[4]]))
  plot(varImp(model_list[[5]]))
  plot(varImp(model_list[[20]]))
  plot(varImp(model_list[[23]]))
  
  fit_final
  pred <- fit_final$pred
  # print(roc(pred$obs, pred$yes) %>% auc())
  print(confusionMatrix(pred$pred, pred$obs, positive='yes', mode="prec_recall"))
  
  ## plot ROC curves
  p1 <- pred %>%
    ggplot(aes(m = yes, d = factor(obs))) + 
    geom_roc(hjust = -0.4, vjust = 1.5) + coord_equal()
  plot_file <- file.path(getwd(), "2_train", model, "rf/roc_plots", paste0(paste(airport, rate, model, horizon, r, sep = "_"), ".png"))
  # dev.copy(png, plot_file, width = 800, height = 600)
  png(filename = plot_file)
  plot(p1)
  dev.off()
  
  ## save final model parameters
  results_out <- fit_final$results
  results_out$airport <- airport
  results_out$response <- response
  results_out$model <- model
  results_out$horizon <- horizon
  results_out$rate <- r
  results_out$ratio <- ratio
  # results_out$i <- i
  results_out$weight <- max(model_weights)
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
  
  ## save final model object
  rf_model_file <- file.path(dir,"2_train",model,"rf",paste0(airport,"_",rate,"_",model_num,"_",r,'_rf_model.rda'))
  saveRDS(fit_final, file = rf_model_file)
  
}
