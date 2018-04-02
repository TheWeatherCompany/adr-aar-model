################################################################################
## Name: TRAIN_OVA_BINARY_SVM.R
## Description: Train binary model for each high frequency rate (OVA) -- RF
## Date: Mar 22, 2018
## Author: jaf
##
## Notes: No testing data; Manually create folds for cross-validation
##        Tuning grid - weights on rate observations (no mtry tuning)
##        Specificity threshold set; if initial model meets then do not tune
##        https://mlr-org.github.io/mlr-tutorial/devel/html/cost_sensitive_classif/index.html
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
spec_threshold <- 0
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
  
  ## subset predictors to most important -- FUTURE STEP
  training_data <- X
  training_data[,r] <- factor(Y[,r], levels = c("0", "1"), labels = c("no", "yes"))
  
  library(mlr)
  library("parallelMap")
  credit.task = makeClassifTask(data = training_data, target = r)
  credit.task = removeConstantFeatures(credit.task)
  credit.task
  
  costs = matrix(c(0, 50, 100, 0), 2)
  colnames(costs) = rownames(costs) = getTaskClassLevels(credit.task)
  costs
  
  lrn = makeLearner("classif.multinom", predict.type = "prob", trace = FALSE)
  mod = train(lrn, credit.task)
  pred = predict(mod, task = credit.task)
  pred
  table(pred$data$response)
  
  th = costs[2,1]/(costs[2,1] + costs[1,2])
  th
  
  pred.th = setThreshold(pred, th)
  pred.th
  table(pred.th$data$response)
  
  credit.costs = makeCostMeasure(id = "credit.costs", name = "Credit costs", costs = costs,
                                 best = 0, worst = 100)
  credit.costs
  
  ## Performance with default thresholds 0.5
  performance(pred, measures = list(credit.costs, mmce))
  
  ## Performance with theoretical thresholds
  performance(pred.th, measures = list(credit.costs, mmce))
  
  ## Cross-validated performance with theoretical thresholds
  rin = makeResampleInstance("CV", iters = 5, task = credit.task)
  lrn = makeLearner("classif.multinom", predict.type = "prob", predict.threshold = th, trace = FALSE)
  r = resample(lrn, credit.task, resampling = rin, measures = list(credit.costs, mmce), show.info = FALSE)
  r
  
  ## Cross-validated performance with default thresholds
  performance(setThreshold(r$pred, 0.5), measures = list(credit.costs, mmce))
  
  d = generateThreshVsPerfData(r, measures = list(credit.costs, mmce))
  plotThreshVsPerf(d, mark.th = th)
  
  ## empirical thresholding
  lrn = makeLearner("classif.ksvm", predict.type = "prob")
  ## 3-fold cross-validation
  r = resample(lrn, credit.task, resampling = rin, measures = list(credit.costs, mmce), show.info = FALSE)
  r
  
  ## Tune the threshold based on the predicted probabilities on the 3 test data sets
  tune.res = tuneThreshold(pred = r$pred, measure = credit.costs)
  tune.res
  performance(setThreshold(r$pred, tune.res$th), measures = list(credit.costs, mmce))
  
  ## Weight for positive class corresponding to theoretical treshold
  w = (1 - tune.res$th)/tune.res$th
  w
  
  ## Weighted learner
  lrn = makeLearner("classif.multinom", trace = FALSE)
  lrn = makeWeightedClassesWrapper(lrn, wcw.weight = w)
  lrn
  
  r = resample(lrn, credit.task, rin, measures = list(credit.costs, mmce), show.info = FALSE)
  r
  r$measures.test
  
  ## tune the weight
  parallelStartSocket(detectCores())
  lrn = makeLearner("classif.ksvm", predict.type = "prob", fix.factors.prediction = FALSE)
  lrn = makeWeightedClassesWrapper(lrn)
  rin = makeResampleInstance("CV", iters = 5, task = credit.task)
  ps = makeParamSet(makeDiscreteParam("wcw.weight", seq(1, 5, 0.5)))
  ctrl = makeTuneControlGrid()
  tune.res = tuneParams(lrn, credit.task, resampling = rin, par.set = ps,
                        measures = list(credit.costs, mmce), control = ctrl, show.info = FALSE)
  tune.res
  parallelStop()
  
  as.data.frame(tune.res$opt.path)[1:3]
  

  
}
