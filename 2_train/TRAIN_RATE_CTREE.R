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

Sys.setenv(TZ="America/New_York")

setwd("../")
dir <- getwd()

## define function to calculate rate probabilities
binary_rate_pred <- function(r, Xt){
  ## load important features & subset
  featureSelect_file <- file.path(model_dir, "variable_importance",paste0(paste(airport, rate, model_num, r, sep = "_"),".txt"))
  featureSelect <- read.table(featureSelect_file)
  featureSelect <- as.character(featureSelect$x)
  Xt_R <- Xt[which(names(Xt) %in% featureSelect)]
  
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
  airport <- 'LGA'
  response <- 'ARR_RATE'
  model <- 'M2'
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
# rates <- c(rates, c("pos","neg"))
# rates <- c(rates, c("change"))

## classification of changes
# change_data <- data.frame(rbind(Y, Yt)) %>%
#   group_by(rate_change) %>%
#   summarise(freq = length(rate_change)) %>%
#   mutate(
#     tot = sum(freq),
#     weight = (1 - freq / tot) * 10,
#     weight = weight ^ 2 / 10
#   )
# change_data$class <- seq(0, nrow(change_data) - 1, 1)
# num_class <- nrow(change_data)

################################################ first step -- predict probabilities

## get predictions for each rate
for(r in rates){
  temp_pred_train <- binary_rate_pred(r, X)
  temp_pred_test <- binary_rate_pred(r, Xt)
  if(r == rates[1]){
    rate_probs_train <- temp_pred_train
    rate_probs_test <- temp_pred_test
  }
  if(r != rates[1]){
    rate_probs_train <- cbind(rate_probs_train, temp_pred_train)
    rate_probs_test <- cbind(rate_probs_test, temp_pred_test)
  }
  rm(temp_pred_train, temp_pred_test, r)
}

## get rate change predictions
# change_pred_train <- multi_change_pred(num_class, X)
# change_pred_train$change_none <- NULL
# change_pred_test <- multi_change_pred(num_class, Xt)
# change_pred_test$change_none <- NULL

## create final training matrices
X_probs <- cbind(rate_probs_train)
# X_probs$response_lag <- Y$rate_lag
Xt_probs <- cbind(rate_probs_test)
# X_probs <- rate_probs_train 
# X_probs$rate_40_prob <- NULL
# X_probs$response_lag <- Y$rate_lag
# X_probs$change_prob <- ifelse(X_probs$change_prob > 0.7, 1, 0)
# X_probs$change_prob <- Y$rate_change
# Xt_probs <- rate_probs_test
# Xt_probs$response_lag <- Yt$rate_lag
# Xt_probs$rate_40_prob <- NULL
# Xt_probs$change_prob <- ifelse(Xt_probs$change_prob > 0.7, 1, 0)
# Xt_probs$change_prob <- Yt$rate_change

Y_probs <- factor(Y[,response]
                  # , levels = c(30, 36, 40), labels = c("rate_30","rate_36","rate_40")
                  )
Yt_probs <- factor(Yt[,response]
                   # , levels = c(30, 36, 40), labels = c("rate_30","rate_36","rate_40")
                   )

train <- cbind(DT, Y[,c(response, "rate_change")], X_probs)
test <- cbind(DTt, Yt[,c(response, "rate_change")], Xt_probs, Xt)

################################################ train classification model
## pre-processing (optional)
# summary(X_probs)
# preProc_X <- preProcess(X_probs, method = c("center", "scale"))
# X_probs <- predict(preProc_X, X_probs)
# Xt_probs <- predict(preProc_X, Xt_probs)
# summary(X_probs)

## define train control
fitControl <- trainControl(method = 'cv'
                           , allowParallel = TRUE
                           , number = 10
                           # , summaryFunction = kss_metric
                           , sampling = 'up'
                           # , classProbs = TRUE
                           # , summaryFunction = twoClassSummary
                           , savePredictions = TRUE
)

## ctree
# grid_ctree2 <- expand.grid(maxdepth = seq(5, 10, 1), mincriterion = 0.95)
cl <- makeCluster(detectCores())
registerDoParallel(cl)
set.seed(218)
fit_ctree2 <- train(x = X_probs
                    , y = Y_probs
                    # , weights = case_weights
                    , method = 'ctree2'
                    , trControl = fitControl
                    # , tuneGrid = grid_ctree2
                    , tuneLength = 10
                    #, returnResamp = TRUE
                    # , metric = 'ROC'
)
train_ctree2 <- fit_ctree2
rm(fit_ctree2) 
stopCluster(cl)

print(train_ctree2)
train_ctree2$results

plot(train_ctree2)
plot(train_ctree2$finalModel)
train_ctree2$finalModel

featureImp <- varImp(train_ctree2, scale=FALSE)
plot(featureImp)

# train$pred <- train_ctree2$finalModel$predicted
pred <- train_ctree2$pred

################################################ preliminary look at predictions
final_pred <- data.frame(
  "dt" = DTt$horizon_data.DTt,
  "pred" = as.numeric(as.character(predict(train_ctree2$finalModel, Xt_probs))),
  "act" = (as.character(Yt_probs)),
  "rate_change" = Yt$rate_change
)
table(final_pred$pred, final_pred$act)

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
final_pred$pred_change <- ifelse(abs(final_pred$pred_diff_pct) >= 10, 1, 0)
final_pred$pred_change <- ifelse(final_pred$time_diff != 1, NA, final_pred$pred_change)
## determine the direction of predicted change
final_pred$pred_dir <- ifelse(final_pred$pred_change == 0, "NONE", 
                              ifelse(final_pred$pred_diff_pct > 0, "POS", "NEG"))

confusionMatrix(final_pred$pred_change, final_pred$act_change)

final_pred <- cbind(Xt_probs, final_pred)

## subset missing
metrics_dat <- final_pred
metrics_miss <- metrics_dat[which(metrics_dat$act_change == 1 & metrics_dat$pred_change == 0),]
metrics_miss <- metrics_miss$dt

metrics_dat$pred_change_rev <- metrics_dat$pred_change
# for changes that we missed, check the previous and following hour to see if the change was captured later
for(m in metrics_miss){
  temp <- metrics_dat[which(metrics_dat$dt %in% c(m, m+3600, m-3600)),]
  if(nrow(temp) != 0){
    temp <- temp[order(temp$dt),]
    prev_hour <- paste(temp[1,"pred_dir"],temp[1,"pred_change"])
    curr_hr <- paste(temp[2,"act_dir"],temp[2,"act_change"])
    next_hour <- paste(temp[3,"pred_dir"],temp[3,"pred_change"])
    prev_change <- ifelse(curr_hr == prev_hour, 1, 0)
    next_change <- ifelse(curr_hr == next_hour, 1, 0)
    ## correct misses to hits if captured the hour before / after; reset incorrect change
    if(prev_change == 1){
      metrics_dat$pred_change_rev <- ifelse(metrics_dat$dt == m, 1, metrics_dat$pred_change_rev)
      metrics_dat$pred_change_rev <- ifelse(metrics_dat$dt == m-3600, 0, metrics_dat$pred_change_rev)
    }
    if(next_change == 1){
      metrics_dat$pred_change_rev <- ifelse(metrics_dat$dt == m, 1, metrics_dat$pred_change_rev)
      metrics_dat$pred_change_rev <- ifelse(metrics_dat$dt == m+3600, 0, metrics_dat$pred_change_rev)
    }
  }
}
confusionMatrix(metrics_dat$pred_change_rev, metrics_dat$act_change)
