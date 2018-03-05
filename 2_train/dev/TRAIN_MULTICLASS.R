################################################################################
## Name: TRAIN_MULTICLASS.R
## Description: Train xgb multi-classification model to predict changes in ADR/AAR rates
## Date: Feb 6, 2018
## Author: jaf
##
## Notes: 1)  overfitting: minimize by reducing number of threes and features through rough feature selection
##        2)  evaluation metric: using mlogloss for now; could use merror or develop own
##        3)  grid search: what parameters should be searched? how do we select best model?
##            lowest test error or smallest difference between train / test? manual?
##        4)  weight: should a weight vector be added - weight for each class?
##        5)  probability threshold: probs for each class are provided, can tune esp. on change hours
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

################################################
## set model parameters

RunBatch = 0

if(RunBatch == 0){
  user <- 'jfinn'
  airport <- 'SFO'
  response <- 'ARR_RATE'
  model <- 'M11'
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

X <- file.path(dir,'datasets',paste0(airport,"_",rate,'_',model_num,'_X.Rds'))
X <- readRDS(file = X)
Xt <- file.path(dir,'datasets',paste0(airport,"_",rate,'_',model_num,'_Xt.Rds'))
Xt <- readRDS(file = Xt)
  
Y <- file.path(dir,'datasets',paste0(airport,"_",rate,'_',model_num,'_Y.Rds'))
Y <- readRDS(file = Y)
Yt <- file.path(dir,'datasets',paste0(airport,"_",rate,'_',model_num,'_Yt.Rds'))
Yt <- readRDS(file = Yt)
  
DT <- file.path(dir,'datasets',paste0(airport,"_",rate,'_',model_num,'_DT.Rds'))
DT <- readRDS(file = DT)
DTt <- file.path(dir,'datasets',paste0(airport,"_",rate,'_',model_num,'_DTt.Rds'))
DTt <- readRDS(file = DTt)

## rate classes
names(Y) <- gsub(response, "rate", names(Y))
names(Yt) <- gsub(response, "rate", names(Yt))
# rate_data <- data.frame(rbind(Y, Yt))
rate_data <- data.frame(rbind(Y, Yt)) %>%
  group_by(rate) %>%
  summarise(freq = length(rate)) %>%
  mutate(
    tot = sum(freq),
    weight = (1 - freq / tot) * 10,
    weight = weight ^ 2 / 10
  )
rate_data$class <- seq(0, nrow(rate_data) - 1, 1)

################################################ feature selection 
## create xgb matrices
X_init <- as.matrix(X)
Xt_init <- as.matrix(Xt)
Y_multi <- as.matrix(left_join(Y, rate_data, by = "rate") %>% select(class))
Yt_multi <- as.matrix(left_join(Yt, rate_data, by = "rate") %>% select(class))
W <- as.matrix(left_join(Y, rate_data, by = "rate") %>% select(weight))
Wt <- as.matrix(left_join(Yt, rate_data, by = "rate") %>% select(weight))

xgbMulti_train <- xgb.DMatrix(data = X_init, label = Y_multi, missing = NaN, weight = W)
xgbMulti_test <- xgb.DMatrix(data = Xt_init, label = Yt_multi, missing = NaN,  weight = Wt)
watchlistMulti <- list(train = xgbMulti_train, test = xgbMulti_test)

## feature selection -- run standard model and select most important only
ntrees <- 200
num_class <- nrow(rate_data)

paramFeature <- list(
  "objective" = "multi:softprob",
  "booster" = "gbtree",
  "eval.metric" = "merror",
  'nthread' = detectCores(),
  'num_class' = num_class
)
xgbFeature <- xgb.train(param = paramFeature
                   , data = xgbMulti_train
                   , seed = seed
                   
                   , nrounds = ntrees
                   , watchlist = watchlistMulti
                   
                   # , scale_pos_weight = 1
                   # , stratified = T
                   , early_stopping_rounds = 10
                   , maximize = FALSE
                   , verbose = TRUE
                   , print_every_n = 10
                   
                   , save_period = 1          ## saves the metrics at every nround
                   , prediction = T           ## saves best train prediction
)
## select the most important features -- top 20
var_importance <- xgb.importance(feature_names = colnames(X_init), model = xgbFeature)
xgb.plot.importance(importance_matrix = var_importance)
var_importance <- var_importance[1:20,]

## save to file
var_importance_file <- file.path(getwd(), "train/variable_importance", paste0(paste(airport, rate, model, horizon, sep = "_"), ".txt"))
write.table(var_importance, file = var_importance_file)

## get important features
featureSelect <- unique(var_importance$Feature)

################################################ xgb - multi:softprob
## create xgb matrices -- select features
X <- as.matrix(X[,which(names(X) %in% featureSelect)])
Xt <- as.matrix(Xt[,which(names(Xt) %in% featureSelect)])

xgbMulti_train <- xgb.DMatrix(data = X, label = Y_multi, missing = NaN)
xgbMulti_test <- xgb.DMatrix(data = Xt, label = Yt_multi, missing = NaN)
watchlistMulti <- list(train = xgbMulti_train, test = xgbMulti_test)

## train xgboost model -- 5-fold cross validation
ntrees <- 10
num_class <- nrow(rate_data)

training_results <- c()
start_time <- Sys.time()
for(i in c(1:20)){
  paramMulti <- list(
    "objective" = "multi:softprob",
    "booster" = "gbtree",
    "eval.metric" = "merror",
    'nthread' = detectCores(),
    'num_class' = num_class,
  
    ## randomized grid search
    max_depth = sample(1:13, 1),
    min_child = sample(1:6, 1),
    eta = runif(1, 0.1, 0.3),
    gamma = runif(1, 0.0, 0.5),
    subsample = runif(1, 0.6, 1.0),
    colsample_bytree = runif(1, 0.5, 1.0)
  )
  xgbMulti <- xgb.cv(param = paramMulti
                      , data = xgbMulti_train
                      , nfold = 5
                      , seed = seed
  
                      , nrounds = ntrees
                      , watchlist = watchlistMulti
  
                      , early_stopping_rounds = 10
                      , maximize = FALSE
                      , verbose = TRUE
                      , print_every_n = 1
  
                      , save_period = 1          ## saves the metrics at every nround
                      , prediction = T           ## saves best train prediction
  )
  ## get best iteration
  cv_scores <- as.data.frame(xgbMulti$evaluation_log)
  best_cv_score <- cv_scores[which(cv_scores$iter == xgbMulti$best_iteration),]
  
  paramMulti <- as.data.frame(paramMulti)
  best_cv_out <- cbind(paramMulti, best_cv_score)
  best_cv_out$iter <- i
  training_results <- rbind(training_results, best_cv_out)
  
  ## remove xgboost model
  unlink(paste0(dir,"/xgboost.model"), recursive = FALSE, force = FALSE)
}
end_time = Sys.time() - start_time
print(end_time)

################################################ select final model
training_results$diff <- (training_results$test_merror_mean - training_results$train_merror_mean)
training_results$airport <- airport
training_results$response <- response
training_results$model <- model
training_results$horizon <- horizon

filename <- file.path(dir,'results/xgbMulti_results.csv')
write.table(training_results
            , file = filename
            , row.names=F
            , eol = "\r"
            , na="NA"
            , append=T
            , quote= FALSE
            , sep=","
            , col.names=F)


## select final model: results with 1) smallest test / train difference and 2) test error > train error 
# output <- training_results %>% filter(diff >= 0) %>% filter(diff == min(diff))
output <- training_results %>% filter(test_merror_mean == 0.0915326)

## train the final model
xgbFinal_param <- list("objective" = "multi:softprob"
                       , "booster" = "gbtree"
                       , "eval.metric" = "merror"
                       , 'nthread' = detectCores()
                       , 'num_class' = num_class

                      ## parameters imported from spreadsheet
                      , 'max_depth' = output$max_depth
                      , 'min_child' = output$min_child
                      , 'gamma' = output$gamma
                      , 'subsample' = output$subsample
                      , 'colsample_bytree' = output$colsample_bytree
                      , 'eta' = output$eta
)
xgbFinal_model <- xgb.train(param = xgbFinal_param
                           , data = xgbMulti_train
                           # , feval = evalerror
                           , seed = seed

                           , nrounds = ntrees
                           , watchlist = watchlistMulti

                           , early_stopping_rounds = 10
                           , maximize = FALSE
                           , verbose = TRUE
                           , print_every_n = 10

                           , save_period = 1          ## saves the metrics at every nround
                           , prediction = T           ## saves best train prediction
)
## model summary
# xgbFinal_model
# var_importance <- xgb.importance(feature_names = colnames(X), model = xgbFinal_model)
# xgb.plot.importance(importance_matrix = var_importance)

## export final model
xgb_final <- file.path(dir,'results/models',paste0(airport,"_",rate,"_",model_num,"_xgboost.model"))
xgb.save(model = xgbFinal_model, fname = xgb_final)

## remove xgboost model
unlink(paste0(dir,"/xgboost.model"), recursive = FALSE, force = FALSE)
