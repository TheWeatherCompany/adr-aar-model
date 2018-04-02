################################################################################
## Name: TRAIN_OVA_BINARY_SVM.R
## Description: Train binary model for each high frequency rate (OVA) -- RF
## Date: Mar 12, 2018
## Author: jaf
##
## Notes: No testing data; Manually create folds for cross-validation
##        Tuning grid - weights on rate observations (no mtry tuning)
################################################################################
### load libraries
suppressMessages(library(caret))
suppressMessages(library(plyr))
suppressMessages(library(reshape2))
suppressMessages(library(data.table))
suppressMessages(library(doParallel))
suppressMessages(library(tidyverse))
suppressMessages(library(mlbench))
suppressMessages(library(randomForest))
suppressMessages(library(pROC))
suppressMessages(library(plotROC))

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
  
  ## initial tune with the same mtry, splitrule, and min.node.size value
  mtry <- 5
  splitrule <- 'gini'
  min.node.size <- 1
  tunegrid <- expand.grid(.mtry = mtry, .splitrule = splitrule, .min.node.size = min.node.size)
  
  ## set weights to train
  weights <- c(0.5, 1, 5, 10, 25, 50, 100, 250, 500)
  
  ## for each set of weights -- train & tune the random forest
  cl <- makeCluster(detectCores() - 1)
  registerDoParallel(cl)
  start_time <- Sys.time()
  
  ## fit model with no weighting / sampling schema
  fit <- train(y = Y_rate
               , x = X_rate
               , method = "ranger"
               , trControl = ctrl
               , tuneGrid = tunegrid
               , seed = seed
               , metric = "ROC"
  )
  ## save the seeds so we can replicate for weight training
  ctrl$seeds <- fit$control$seeds
  
  ## now, train models using different weights on the rate observations -- penalize misclassification
  model_list <- list("fit" = fit)
  for(w in weights){
    print(paste("weight =", w))
    model_weights <- ifelse(Y[,r] == 0, 1, 1 / ratio * w)
    fit_wt <- train(y = Y_rate
                    , x = X_rate
                    , method = "ranger"
                    , trControl = ctrl
                    , tuneGrid = tunegrid
                    , weights = model_weights
                    , metric = "ROC"
    )
    model_list[[paste0('fit_wt', w)]] <- fit_wt
    rm(model_weights, fit_wt)
  }
  stopCluster(cl)
  print(Sys.time() - start_time)
  
  ## combine models to assess results
  summary(resamples(model_list))
  bwplot(resamples(model_list))
  dotplot(resamples(model_list))
  
  summary <- summary(resamples(model_list))
  spec <- data.frame(summary$statistics$Spec)
  spec$weight <- rownames(spec)
  spec$metric <- "Specificity"
  sens <- data.frame(summary$statistics$Sens)
  sens$weight <- rownames(spec)
  sens$metric <- "Sensitivity"
  metrics <- rbind(sens, spec)
  metrics %>%
    select(weight, metric, Mean) %>%
    mutate(
      weight = gsub("fit","",weight),
      weight = gsub("_wt","",weight),
      weight = as.numeric(weight),
      weight = ifelse(is.na(weight), 0, weight)
    ) %>%
    spread(metric, Mean) %>%
      filter(weight <= 10) %>%
      ggplot(aes(x = weight)) +
      geom_point(aes(y = Sensitivity, group = 1, color = "Sensitivity")) +
      geom_line(aes(y = Sensitivity, group = 1, color = "Sensitivity")) +
      geom_point(aes(y = Specificity, group = 1, color = "Specificity")) +
      geom_line(aes(y = Specificity, group = 1, color = "Specificity")) +
      ylab("Value") +
      scale_color_manual(name="Metric",
                      values=c(Sensitivity="#008B00", Specificity="#CD4F39"))
  
  ## calculate area under the curve
  test_roc <- function(data) roc(data$obs, data$yes)
  model_results <- c()
  for (i in 1:length(model_list)){
    print(names(model_list)[i])
    pred <- model_list[[i]]$pred
    print(test_roc(pred) %>% auc())
    print(confusionMatrix(pred$pred, pred$obs))
    pred$model <- names(model_list)[i]
    if(!"weights" %in% colnames(pred)) pred$weights <- NA
    model_results <- rbind(model_results, pred)
    rm(pred)
  }
  
  ## plot ROC curves
  suppressWarnings(model_results %>%
    ggplot(aes(m = yes, d = factor(obs), colour = model)) + 
    geom_roc(hjust = -0.4, vjust = 1.5) + coord_equal())
  
}
