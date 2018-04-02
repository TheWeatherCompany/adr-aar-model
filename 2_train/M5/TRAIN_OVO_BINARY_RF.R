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
spec_threshold <- 0.35
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

## create combinations of rates -- pairwise models
rates <- names(Y)[!names(Y) %in% c(response,"rate_lag","rate_dt","rate_dt_pct","rate_change")]
pw_models <- c()
for(r in rates){
  rates <- rates[which(rates != r)]
  temp <- merge(rates, r)
  pw_models <- rbind(pw_models, temp)
  rm(r, temp)
}
names(pw_models) <- c("rate_1", "rate_2")
pw_models$rate_1 <- as.character(pw_models$rate_1)
pw_models$rate_2 <- as.character(pw_models$rate_2)

################################################ SVM -- RFE with random forest
i <- 1
for(i in c(1:nrow(pw_models))){
  ## set rates
  rate_1 <- pw_models[i,"rate_1"]
  rate_2 <- pw_models[i,"rate_2"]
  mnum <- paste(rate_1, rate_2, sep = "_")
  print(mnum)
  
  ## subset data -- default set rate_1 == 1, rate_2 == 0
  Y_model <- Y[which(Y[,rate_1] == 1 | Y[,rate_2] == 1),]
  train_idx <- rownames(Y_model)
  Y_model <- ifelse(Y_model[,rate_1] == 1, 1, 0)
  Y_model <- factor(Y_model, levels = c("1", "0"), labels = c("rate_1", "rate_2"))
  X_model <- X[which(rownames(X) %in% train_idx),]
  
  ## define train / test -- split dataset into even nfold groups where no partition repeated
  index <- as.numeric(rownames(X_model))
  nfold <- 5
  groups <- cut_number(index, nfold)
  ## create list of indices
  group_folds <- groupKFold(groups, k = nfold)
  
  ## identify rate -- get ratio of rate observations to total samples
  rate_total <- length(Y_model[which(Y_model == 'rate_1')])
  ratio <- rate_total / length(Y_model)
  
  ## set train control -- 5-fold cross validation
  ctrl <- trainControl(method = "cv"
                       , number = nfold
                       , index = group_folds
                       , allowParallel = TRUE
                       , classProbs =  TRUE
                       , savePredictions = "all"
                       # , sampling = "smote"
                       , summaryFunction = twoClassSummary
  )
  
  ## tune all models with weight
  model_weights <- ifelse(Y_model == "rate_1", ratio, 1)
  
  ## initial tune with the same mtry, splitrule, and min.node.size value
  mtry <- 5
  splitrule <- 'gini'
  min.node.size <- 1
  tunegrid <- expand.grid(.mtry = mtry, .splitrule = splitrule, .min.node.size = min.node.size)
  
  ## fit initial model with no weighting / sampling schema
  cl <- makeCluster(detectCores() - 1)
  registerDoParallel(cl)
  start_time <- Sys.time()
  fit_init <- train(y = Y_model
               , x = X_model
               , method = "ranger"
               , trControl = ctrl
               , tuneGrid = tunegrid
               # , tuneLength = 5
               # , weights = model_weights
               , seed = seed
               , metric = "ROC"
  )
  stopCluster(cl)
  print(Sys.time() - start_time)

  fit_final <- fit_init
  fit_final
  pred <- fit_final$pred %>% filter(mtry == fit_final$bestTune$mtry & splitrule == fit_final$bestTune$splitrule)
  # print(roc(pred$obs, pred$yes) %>% auc())
  print(confusionMatrix(pred$pred, pred$obs))
  
  ## plot ROC curves
  p1 <- pred %>%
    ggplot(aes(m = rate_2, d = factor(obs))) + 
    geom_roc(hjust = -0.4, vjust = 1.5) + coord_equal()
  plot_file <- file.path(getwd(), "2_train", model, "ovo/roc_plots", paste0(paste(airport, rate, model, horizon, mnum, sep = "_"), ".png"))
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
  results_out$rates <- mnum
  results_out$ratio <- ratio
  # results_out$i <- i
  results_out$weight <- max(model_weights)
  filename <- file.path(dir,"2_train",model,"ovo/rf_results.csv")
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
  rf_model_file <- file.path(dir,"2_train",model,"ovo",paste0(airport,"_",rate,"_",model_num,"_",mnum,'_rf_model.rda'))
  saveRDS(fit_final, file = rf_model_file)
  
}
