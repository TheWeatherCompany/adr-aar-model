################################################
### load libraries
suppressMessages(library(caret))
suppressMessages(library(plyr))
suppressMessages(library(xgboost))
suppressMessages(library(reshape2))
suppressMessages(library(data.table))
# suppressMessages(library(verification))
suppressMessages(library(parallel))
suppressMessages(library(doParallel))
suppressMessages(library(doMC))
suppressMessages(library(tidyverse))

Sys.setenv(TZ="America/New_York")

################################################
## set model parameters

RunBatch = 1

if(RunBatch == 0){
  user <- 'jfinn'
  airport <- 'ATL'
  response <- 'ARR_RATE'
  model <- 'M9'
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

X <- as.matrix(X)
Xt <- as.matrix(Xt)
Y <- as.matrix(Y)
Yt <- as.matrix(Yt)

################################################ 
## create xgb matrices

xgb_train <- xgb.DMatrix(data = X, label = Y, missing = NaN)
xgb_test <- xgb.DMatrix(data = Xt, label = Yt, missing = NaN)
watchlist <- list(train = xgb_train, test = xgb_test)

###############################################
### xgboost tree - random grid search
ntrees <- 200

training_results <- c()
start_time <- Sys.time()
for(i in c(1:20)){
  param <- list(
    objective = "reg:linear",
    eval_metric = c("rmse"),
    booster = "gbtree",
    nthread = 16,
    
    ## randomized grid search
    max_depth = sample(1:13, 1),
    min_child = sample(1:6, 1),
    eta = runif(1, 0.1, 0.3),
    gamma = runif(1, 0.0, 0.5), 
    subsample = runif(1, 0.6, 1.0),
    colsample_bytree = runif(1, 0.5, 1.0)
  )
  xgboostModelCV <- xgb.cv(params = param
                           , data =  xgb_train
                           , nrounds = ntrees
                           , nfold = 5
                           , scale_pos_weight = 1
                           , stratified = T
                           , seed = seed
                           , print_every_n = 10
                           
                           , showsd = F
                           , verbose = T
                           
                           , watchlist = watchlist
                           , early_stopping_rounds = 10
                           , maximize = FALSE
  )
  cv_scores <- as.data.frame(xgboostModelCV$evaluation_log)  
  best_cv_score <- cv_scores[which(cv_scores$iter == xgboostModelCV$best_iteration),]
  
  param <- as.data.frame(param)
  best_cv_out <- cbind(param, best_cv_score)
  training_results <- rbind(training_results, best_cv_out)
}
end_time = Sys.time() - start_time
print(end_time)

## get the final model results
output <- training_results[which(training_results$test_rmse_mean == min(training_results$test_rmse_mean)),]
## then, select iteration with lowest train RMSE
output <- output[which(output$train_rmse_mean == min(output$train_rmse_mean)),]
## if still duplicates, select lowest number of nrounds
output <- output[which(output$iter == min(output$iter)),]
output$airport <- airport
output$response <- response
output$model <- model
output$horizon <- horizon

# ## train the final model
# xgbFinal_param <- list("objective" = "reg:linear"
#                       , "booster" = "gbtree"
#                       , "eval.metric" = "rmse"
#                       # , 'nthread' = detectCores()
#                       
#                       ## parameters imported from spreadsheet
#                       , 'max_depth' = output$max_depth
#                       , 'min_child' = output$min_child
#                       , 'gamma' = output$gamma
#                       , 'subsample' = output$subsample
#                       , 'colsample_bytree' = output$colsample_bytree
#                       , 'eta' = output$eta
# )
# xgbFinal_model <- xgb.train(param = xgbFinal_param
#                            , data = xgb_train
#                            # , feval = evalerror
#                            , seed = seed
#                            
#                            , nrounds = ntrees
#                            , watchlist = watchlist
#                            
#                            , scale_pos_weight = 1
#                            , stratified = T
#                            , early_stopping_rounds = 10
#                            , maximize = FALSE
#                            , verbose = TRUE
#                            , print_every_n = 10
#                            
#                            , save_period = 1          ## saves the metrics at every nround
#                            , prediction = T           ## saves best train prediction
# )
# 
# ## export final model
# model_file <- file.path(dir,'results/models',paste0(airport,"_",rate,"_",model_num,"_xgboost.model"))
# xgb.save(model = xgbFinal_model, fname = model_file)

## export model parameters
filename <- file.path(dir,'results/xgboost_rmse_params.csv')
write.table(output
            , file = filename
            , row.names=F
            , eol = "\r"
            , na="NA"
            , append=T
            , quote= FALSE
            , sep=","
            , col.names=F)
