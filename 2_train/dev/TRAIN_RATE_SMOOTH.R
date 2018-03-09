################################################################################
## Name: TRAIN_RATE_CLASS.R
## Description: Train decision tree model with rate probs and change probs as input -- predict final rate 
## Date: Feb 15, 2018
## Author: jaf
##
## Notes:
################################################################################
### load libraries
suppressMessages(library(caret))
suppressMessages(library(plyr))
suppressMessages(library(xgboost))
suppressMessages(library(reshape2))
suppressMessages(library(data.table))
suppressMessages(library(doParallel))
suppressMessages(library(tidyverse))
suppressMessages(library(gridExtra))

Sys.setenv(TZ="America/New_York")

# source("custom_functions.R")
# source("RATE_CLASS_FUNCTIONS.R")

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
  model <- 'M1'
  horizons <- paste0('H', c(1:12))
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
print(horizons)

horizons <- factor(x = horizons, 
                   levels = c("H1","H2","H3","H4","H5","H6","H7","H8","H9","H10","H11","H12"),
                   labels = c("H1","H2","H3","H4","H5","H6","H7","H8","H9","H10","H11","H12")
)

rate <- ifelse(response == "DEP_RATE","ADR","AAR")
response <- tolower(response)

seed <- 2187

################################################
## load in model datasets 
setwd("../")
dir <- getwd()
model_type <- 'rf'
model_dir <- file.path(dir, "2_train", model, model_type)

ts_probs_train <- c()
ts_probs_test <- c()
for(horizon in horizons){
  model_num <- paste0(model,"_",horizon)
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
  
  ## get unique rates
  rates <- names(Y)[!names(Y) %in% c(response,"rate_lag","rate_dt","rate_dt_pct","rate_change")]
  # rates <- c(rates, c("pos","neg"))
  
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
  rate_probs_train$dt <- as.character(DT$horizon_data.DT)
  rate_probs_train$horizon <- horizon
  rate_probs_train <- cbind(rate_probs_train, Y)
  rate_probs_test$dt <- as.character(DTt$horizon_data.DTt)
  rate_probs_test$horizon <- horizon
  rate_probs_test <- cbind(rate_probs_test, Yt)
  
  ts_probs_train <- rbind(ts_probs_train, rate_probs_train)
  ts_probs_test <- rbind(ts_probs_test, rate_probs_test)
  
  rm(X, Xt, Y, Yt, DT, DTt, rates, rate_probs_train, rate_probs_test, horizon)
}
ts_probs_train$horizon <- factor(x = ts_probs_train$horizon, 
                                 levels = c("H1","H2","H3","H4","H5","H6","H7","H8","H9","H10","H11","H12"),
                                 labels = c("H1","H2","H3","H4","H5","H6","H7","H8","H9","H10","H11","H12")
)
ts_probs_test$horizon <- factor(x = ts_probs_test$horizon, 
                                 levels = c("H1","H2","H3","H4","H5","H6","H7","H8","H9","H10","H11","H12"),
                                 labels = c("H1","H2","H3","H4","H5","H6","H7","H8","H9","H10","H11","H12")
)

# datetime <- as.POSIXct(strptime(ts_probs_train$dt, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
# ts_probs_train <- cbind(datetime, ts_probs_train)
# ts_probs_train$datetime <- as.POSIXct(ts_probs_train$datetime)

## for each timestamp we have, create timeseries of predicted probabilities
ts <- unique(ts_probs_train$dt)
ts <- ts[1:100]
timeseries_train <- c()
timeseries_test <- c()
for(t in ts){
  t <- as.POSIXct(strptime(t, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  curr_ts <- data.frame("datetime" = seq(from = t, to = (t + (3600 * 11)), by = "hour"), "horizon" = horizons)
  curr_ts$dt <- as.character(curr_ts$datetime)
  dat_train <- right_join(x = ts_probs_train, y = curr_ts, by = c("dt","horizon"))
  dat_train$dt_init <- t
  dat_train <- na.omit(dat_train)
  
  dat_test <- right_join(x = ts_probs_train, y = curr_ts, by = c("dt","horizon"))
  dat_test$dt_init <- t
  dat_test <- na.omit(dat_test)
  
  if(nrow(dat_train) == 12){timeseries_train <- rbind(timeseries_train, dat_train)}
  if(nrow(dat_test) == 12){timeseries_test <- rbind(timeseries_test, dat_test)}
}

names(timeseries_train)

## determine predicted rate based on max probability
pred_max <- timeseries_train %>%
  select(dt, horizon, rate_30_prob, rate_36_prob, rate_40_prob) %>%
  gather(rate, pred, -dt, -horizon) %>%
  group_by(dt, horizon) %>%
  mutate(max_pred = max(pred, na.rm = T))
pred_max <- pred_max[which(pred_max$max_pred == pred_max$pred),]
pred_max$pred <- as.numeric(gsub(paste(c("rate_","_prob"),collapse = "|"),"",pred_max$rate))
pred_max <- pred_max[,c("dt","horizon","pred")]

ts_train_pred <- merge(x = timeseries_train, y = pred_max, by = c("dt","horizon"))
ts_train_pred <- ts_train_pred %>% arrange(dt_init, horizon)

## determine accuracy on change data
ts_train_pred$dt_lag <- shift(x = ts_train_pred$datetime, n = 1, type = "lag")
ts_train_pred$time_diff <- ts_train_pred$datetime - ts_train_pred$dt_lag

## calculate pred change
ts_train_pred$pred_lag <- shift(x = ts_train_pred$pred,n = 1L,fill = NA,type = "lag")
ts_train_pred$pred_diff_pct <- ifelse(ts_train_pred$pred != 0,
                                      (ts_train_pred$pred - ts_train_pred$pred_lag) / ts_train_pred$pred * 100,
                                      (ts_train_pred$pred - ts_train_pred$pred_lag) / 1 * 100)  
## identify significant predicted changes
ts_train_pred$pred_change <- ifelse(abs(ts_train_pred$pred_diff_pct) >= 10, 1, 0)
ts_train_pred$pred_change <- ifelse(ts_train_pred$time_diff != 1, NA, ts_train_pred$pred_change)
## determine the direction of predicted change
ts_train_pred$pred_dir <- ifelse(ts_train_pred$pred_change == 0, "NONE", 
                                 ifelse(ts_train_pred$pred_diff_pct > 0, "POS", "NEG"))

# ts_train_pred$act_change <- ifelse(ts_train_pred$rate_change == 0, 0, 1)
ts_train_pred$act_change <- ifelse(abs(ts_train_pred$rate_dt_pct) >= 10, 1, 0)
ts_train_pred$act_dir <- ifelse(ts_train_pred$rate_change == 0, "NONE", 
                                ifelse(ts_train_pred$rate_change == 1, "POS", "NEG"))
confusionMatrix(ts_train_pred$pred_change, ts_train_pred$act_change)











################################################ first step -- predict probabilities
X_probs <- rate_probs_train
Xt_probs <- rate_probs_test

Y_probs <- factor(Y[,response])
Yt_probs <- factor(Yt[,response])

################################################ train classification model
## pre-processing (optional)
preProc_X <- preProcess(X)
X <- predict(preProc_X, X)

## define train control
fitControl <- trainControl(method = 'cv'
                           , allowParallel = TRUE
                           , number = 10
                           # , summaryFunction = kss_metric
                           # , sampling = 'up'
                           , savePredictions = TRUE
)

## ctree
grid_ctree2 <- expand.grid(maxdepth = seq(5, 10, 1), mincriterion = 0.95)
cl <- makeCluster(16)
registerDoParallel(cl)
set.seed(218)
fit_ctree2 <- train(x = X_probs
                    , y = Y_probs
                    # , weights = case_weights
                    , method = 'ctree2'
                    , trControl = fitControl
                    , tuneGrid = grid_ctree2
                    #, returnResamp = TRUE
                    # , metric = c('KSS')
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

################################################ preliminary look at predictions
final_pred <- data.frame(
  "dt" = DTt$horizon_data.DTt,
  "pred" = as.numeric(as.character(predict(train_ctree2$finalModel, Xt_probs))),
  "act" = as.numeric(as.character(Yt_probs)),
  "rate_change" = Yt$rate_change
)
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
    if(prev_change == 1 | next_change == 1){
      metrics_dat$pred_change_rev <- ifelse(metrics_dat$dt == m, 1, metrics_dat$pred_change_rev)
    }
  }
}
confusionMatrix(metrics_dat$pred_change_rev, metrics_dat$act_change)

