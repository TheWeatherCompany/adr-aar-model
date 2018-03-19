################################################################################
## Name: TRAIN_RATE_CLASS.R
## Description: Train decision tree model with rate probs and change probs as input -- predict final rate 
## Date: Mar 7, 2018
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
suppressMessages(library(randomForest))
suppressMessages(library(scales))

Sys.setenv(TZ="America/New_York")

setwd("../../")
dir <- getwd()

## define function to calculate rate probabilities
binary_rate_pred <- function(r, X){
  ## load important features & subset
  featureSelect_file <- file.path(model_dir, "variable_importance",paste0(paste(airport, rate, model_num, r, sep = "_"),".txt"))
  featureSelect <- read.table(featureSelect_file)
  featureSelect <- as.character(rownames(featureSelect))
  Xt_R <- X[which(names(X) %in% featureSelect)]
  
  ## load model
  fitModel_file <- file.path(model_dir,paste(airport, rate, model_num, r, model_type, 'model.rda',sep = "_"))
  fitModel <- readRDS(fitModel_file)
  
  ## generate predictions
  temp_pred <- predict.train(object = fitModel,newdata = Xt_R, type = "prob")
  temp_pred <- data.frame(temp_pred[,"yes"])
  names(temp_pred) <- paste0(r,"_prob")
  return(temp_pred)
}

################################################
## set model parameters

RunBatch = 0

if(RunBatch == 0){
  user <- 'jfinn'
  airport <- 'SFO'
  response <- 'ARR_RATE'
  model <- 'M3'
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

model_type <- 'rf'
model_dir <- file.path(dir, "2_train", model, model_type)

seed <- 2187

################################################
## load in model datasets 
X <- file.path(file.path(dir,'1_data_prep',model),paste0(airport,"_",rate,'_',model_num,'_X.Rds'))
X <- readRDS(file = X)

Y <- file.path(file.path(dir,'1_data_prep',model),paste0(airport,"_",rate,'_',model_num,'_Y.Rds'))
Y <- readRDS(file = Y)

DT <- file.path(file.path(dir,'1_data_prep',model),paste0(airport,"_",rate,'_',model_num,'_DT.Rds'))
DT <- readRDS(file = DT)

rates <- names(Y)[!names(Y) %in% c(response,"rate_lag","rate_dt","rate_dt_pct","rate_change")]

################################################ first step -- predict probabilities
## get predictions for each rate
for(r in rates){
  temp_pred <- binary_rate_pred(r, X)
  if(r == rates[1]){rate_probs <- temp_pred}
  if(r != rates[1]){rate_probs <- cbind(rate_probs, temp_pred)}
  rm(temp_pred, r)
}
rate_probs$rowid <- seq(1:(nrow(rate_probs)))

proc <- preProcess(x = rate_probs, method = c("center", "scale"))
rate_probs_proc <- predict(proc, rate_probs)

################################################ train a model to better classify the rate based on probabilities -- recursive partition
Y_probs <- factor(Y[,response])
X_probs <- rate_probs %>% select(-rowid)

index <- as.numeric(rownames(X_probs))
nfold <- 5
groups <- cut_number(index, 5)
## create list of indices
group_folds <- groupKFold(groups, k = nfold)

## define train control
fitControl <- trainControl(method = 'cv'
                           , allowParallel = TRUE
                           , number = nfold
                           , index = group_folds
                           # , summaryFunction = kss_metric
                           # , sampling = 'smote'
                           # , classProbs = TRUE
                           # , summaryFunction = twoClassSummary
                           , savePredictions = TRUE
)

## ctree
grid_ctree2 <- expand.grid(maxdepth = seq(3, 18, 3), mincriterion = 0.95)
cl <- makeCluster(detectCores())
registerDoParallel(cl)
set.seed(218)
fit_ctree2 <- train(x = X_probs
                    , y = Y_probs
                    # , weights = case_weights
                    , method = 'ctree2'
                    , trControl = fitControl
                    , tuneGrid = grid_ctree2
                    # , tuneLength = 10
                    #, returnResamp = TRUE
                    # , metric = 'ROC'
)
train_ctree2 <- fit_ctree2
rm(fit_ctree2) 
stopCluster(cl)

print(train_ctree2)
train_ctree2$results
train_ctree2$finalModel

plot(train_ctree2)
# plot(train_ctree2$finalModel)
train_ctree2$finalModel

featureImp <- varImp(train_ctree2, scale=FALSE)
plot(featureImp)

################################################ preliminary look at predictions
final_pred <- data.frame(
  "dt" = DT$horizon_data.DT,
  "pred" = as.numeric(as.character(predict(train_ctree2$finalModel, X_probs))),
  "act" = (as.character(Y_probs)),
  "rate_change" = Y$rate_change
)
final_pred <- cbind(final_pred, X_probs)
confusionMatrix(final_pred$pred, final_pred$act)

## look at metrics on the change data
final_pred$act_change <- ifelse(abs(final_pred$rate_change) == 1, 1, 0)
final_pred$act_dir <- ifelse(final_pred$rate_change == 0, "NONE", 
                             ifelse(final_pred$rate_change == 1, "POS", "NEG"))

final_pred$dt <- as.POSIXct(strptime(x = final_pred$dt, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
final_pred$dt_lag <- shift(x = final_pred$dt, n = 1, type = "lag")
final_pred$time_diff <- final_pred$dt - final_pred$dt_lag

## calculate pred change
final_pred$pred_lag <- shift(x = final_pred$pred,n = 1L,fill = NA,type = "lag")
final_pred$pred_diff_pct <- ifelse(final_pred$pred != 0,
                             (final_pred$pred - final_pred$pred_lag) / final_pred$pred * 100,
                             (final_pred$pred - final_pred$pred_lag) / 1 * 100)  
## identify significant predicted changes
final_pred$pred_change <- ifelse(abs(final_pred$pred_diff_pct) >= 5, 1, 0)
final_pred$pred_change <- ifelse(final_pred$time_diff != 1, NA, final_pred$pred_change)
## determine the direction of predicted change
final_pred$pred_dir <- ifelse(final_pred$pred_change == 0, "NONE", 
                              ifelse(final_pred$pred_diff_pct > 0, "POS", "NEG"))
confusionMatrix(final_pred$pred_change, final_pred$act_change)

## ignoring if we get the change right or not -- how do we do on the change hours in terms of getting the correct rate?
change <- final_pred %>% filter(act_change == 1)
confusionMatrix(change$pred, change$act)
