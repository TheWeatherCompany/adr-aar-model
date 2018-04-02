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

RunBatch = 0

if(RunBatch == 0){
  user <- 'jfinn'
  airport <- 'SFO'
  response <- 'ARR_RATE'
  model <- 'M8'
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

model_type <- 'fa_5pct_wt1_sfo_wx'
model_dir <- file.path(dir, "2_train", model, model_type)

seed <- 2187

## define function to calculate rate probabilities
binary_rate_pred <- function(r, X){
  print(r)
  
  ## load model
  fitModel_file <- file.path(model_dir,paste(airport, rate, model_num, r, "rf", 'model.rda',sep = "_"))
  fitModel <- readRDS(fitModel_file)
  
  pred <- fitModel$pred
  cm <- confusionMatrix(pred$pred, pred$obs)
  freq <- sum(Y[,r]) / nrow(Y) * 100
  false_alarms <- cm$table[2,1] / nrow(Y) * 100
  hits <- cm$table[2,2] / nrow(Y) * 100
  
  temp <- data.frame("rate" = r, "freq" = freq, "hit" = hits, "false_alarms" = false_alarms)
  
  ## print model results
  print(fitModel)
  print(confusionMatrix(pred$pred, pred$obs))
  
  ## plot top features
  var_imp <- data.frame("importance" = fitModel$finalModel$variable.importance)
  var_imp$variable <- row.names(var_imp)
  var_imp <- var_imp %>% arrange(desc(importance)) %>% head(10)
  # p1 <- plot(varImp(fitModel))
  print(var_imp)
  
  ## get predictions on holdout set
  holdout_pred <- fitModel$pred %>% data.frame()
  holdout_pred <- holdout_pred[,c("rowIndex","yes")]
  names(holdout_pred) <- gsub("yes", paste0(r,"_prob"), names(holdout_pred))
  
  data_frames <- list(
    "holdout_pred" = holdout_pred
    , "temp" = temp
    , "var_imp" = var_imp
  )
  return(data_frames)
}

## load in model datasets 
X <- file.path(file.path(dir,'1_data_prep',model),paste0(airport,"_",rate,'_',model_num,'_X.Rds'))
X <- readRDS(file = X)

Y <- file.path(file.path(dir,'1_data_prep',model),paste0(airport,"_",rate,'_',model_num,'_Y.Rds'))
Y <- readRDS(file = Y)

DT <- file.path(file.path(dir,'1_data_prep',model),paste0(airport,"_",rate,'_',model_num,'_DT.Rds'))
DT <- readRDS(file = DT)

rates <- names(Y)[!names(Y) %in% c(response,"rate_lag","rate_dt","rate_dt_pct","rate_change")]

## load in and print metrics table
metrics_table <- read_csv(file.path(model_dir, "rf_metrics.csv"), 
                          col_names = c("rate","frequency","weight","hit_rate","false_alarm_rate"))
metrics_table <- metrics_table %>% data.frame()
print(metrics_table)


## get predictions for each rate
for(r in rates){
  temp_pred <- binary_rate_pred(r, X)
  if(r == rates[1]){
    rate_probs <- temp_pred$holdout_pred
    rate_metrics <- temp_pred$temp
    rate_var_imp <- temp_pred$var_imp
  }
  if(r != rates[1]){
    rate_probs <- left_join(x = rate_probs, y = temp_pred$holdout_pred, by = "rowIndex")
    rate_metrics <- rbind(rate_metrics, temp_pred$temp)
    rate_var_imp <- rbind(rate_var_imp, temp_pred$var_imp)
  }
  rm(r)
}

## get unique important variables
var_select <- unique(rate_var_imp$variable)

probs_wx <- cbind(Y[,response], rate_probs, X)

Y_train <- factor(Y[,response])
X_probs <- rate_probs %>% select(-rowIndex)
X_train <- cbind(
  X_probs
  , X[,var_select]
)

## pre-process probabilities -- center and scale
# preProc <- preProcess(x = X_train, method = c("center","scale"))
# X_train <- predict(preProc, X_train)

## manually create folds
index <- as.numeric(rownames(X_train))
nfold <- 5
groups <- cut_number(index, nfold)
## create list of indices
group_folds <- groupKFold(groups, k = nfold)

## define train control
fitControl <- trainControl(method = 'cv'
                           , allowParallel = TRUE
                           , number = nfold
                           , index = group_folds
                           , returnResamp = "all"
                           # , selectionFunction = "oneSE"
                           , sampling = "smote"
                           , summaryFunction = multiClassSummary
                           , savePredictions = TRUE
)

## ctree
grid_ctree2 <- expand.grid(maxdepth = seq(3, 15, 3), mincriterion = 0.95)
cl <- makeCluster(detectCores())
registerDoParallel(cl)
set.seed(218)
fit_ctree2 <- train(x = X_train
                    , y = Y_train
                    , method = 'ctree2'
                    , trControl = fitControl
                    # , tuneLength = 3
                    , tuneGrid = grid_ctree2
                    , metric = 'Mean_F1'
)
train_ctree2 <- fit_ctree2
rm(fit_ctree2) 
stopCluster(cl)

train_ctree2
plot(train_ctree2)
train_ctree2$finalModel
train_ctree2$results
plot(train_ctree2$finalModel)
# train_ctree2$resample
plot(varImp(train_ctree2))

## dataset with final predictions
final_pred <- train_ctree2$pred %>% data.frame() %>% 
  filter(maxdepth == as.numeric(train_ctree2$bestTune$maxdepth) & mincriterion == train_ctree2$bestTune$mincriterion) %>%
  # filter(mtry == as.numeric(train_ctree2$bestTune$mtry) & splitrule == train_ctree2$bestTune$splitrule) %>%
  mutate(
    act = as.numeric(as.character(obs)),
    pred = as.numeric(as.character(pred))
  ) %>%
  select(pred, act)
final_pred$dt <- DT$horizon_data.DT
final_pred$rate_change <- Y$rate_change
final_pred <- cbind(final_pred, X_train)

confusionMatrix(final_pred$pred, final_pred$act)
