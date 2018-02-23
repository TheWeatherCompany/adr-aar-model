################################################################################
## Name: TRAIN_FUNCTIONS.R
## Description: Functions to help model training
## Date: Feb 7, 2018
## Author: jaf
##
## Notes: 
##################################################################################
### load libraries
suppressMessages(library(caret))
suppressMessages(library(plyr))
suppressMessages(library(xgboost))
suppressMessages(library(reshape2))
suppressMessages(library(data.table))
suppressMessages(library(doParallel))
suppressMessages(library(tidyverse))
suppressMessages(library(gridExtra))

## PCA libraries
suppressMessages(library(FactoMineR))
suppressMessages(library(factoextra))
suppressMessages(library(corrplot))

# ntrees <- 200
# objective <- "binary:logistic"
# eval_metric <- "auc"
# maximize <- TRUE
# niter <- 1
# nfold <- 5

pca_featureSelect <- function(X_pca, Xt_pca, thresh){

  # X_PCA <- X
  
  ## run PCA
  # pca_train <- PCA(X_PCA, graph = FALSE, scale.unit = TRUE)
  # 
  # eig_val <- get_eigenvalue(pca_train)
  # eig_val
  # fviz_eig(pca_train, addlabels = TRUE, ylim = c(0, 50))
  # 
  # ## correlation circle
  # var <- get_pca_var(pca_train)
  # fviz_pca_var(pca_train, col.var = "black")
  # 
  # ## quality of representation
  # corrplot(var$cos2, is.corr=FALSE)
  # # Total cos2 of variables on Dim.1 and Dim.2
  # fviz_cos2(pca_train, choice = "var", axes = 1:2)
  # 
  # fviz_pca_var(pca_train, col.var = "cos2",
  #              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
  #              repel = TRUE # Avoid text overlapping
  # )
  # 
  # ## contributions of variables to PC
  # corrplot(var$contrib, is.corr=FALSE)   
  # # Contributions of variables to PC1
  # fviz_contrib(pca_train, choice = "var", axes = 1, top = 10)
  # # Contributions of variables to PC2
  # fviz_contrib(pca_train, choice = "var", axes = 2, top = 10)
  # 
  # fviz_pca_biplot(pca_train, 
  #                 col.ind = factor(Y$arr_rate), palette = "jco", 
  #                 addEllipses = TRUE, label = "var",
  #                 col.var = "black", repel = TRUE,
  #                 legend.title = "Species") 
  # 
  # ## now -- predict the data on PCA
  # X_C <- predict(pca_train, X_PCA)
  # head(X_C$dist, n = 3)
  
  
  
  
  ## prcomp
  prin_comp <- prcomp(X_pca, scale. = T)
  names(prin_comp)
  # biplot(prin_comp, scale = 0)
  
  std_dev <- prin_comp$sdev
  pr_var <- std_dev ^ 2
  pr_var[1:10]
  prop_varex <- pr_var/sum(pr_var)
  prop_varex[1:20]
  # plot(prop_varex, xlab = "Principal Component",
  #      ylab = "Proportion of Variance Explained",
  #      type = "b")
  ## cumulative plot
  cumsum <- cumsum(prop_varex)[which(cumsum(prop_varex) <= thresh)]
  # plot(cumsum(prop_varex), xlab = "Principal Component",
  #      ylab = "Cumulative Proportion of Variance Explained",
  #      type = "b")
  
  train_out <- data.frame(prin_comp$x)
  train_out <- train_out[,1:length(cumsum)]
  
  test_out <- predict(prin_comp, newdata = Xt_pca)
  test_out <- as.data.frame(test_out)
  test_out <- test_out[,1:length(cumsum)]
  
  data_frames <- list(
    "train_out" = train_out
    , "test_out" = test_out
  )
  return(data_frames)
  
}


xgb_featureSelect <- function(X, Xt, Y, Yt, ntrees, objective, eval_metric){
  ## create xgb matrices
  X <- as.matrix(X)
  Xt <- as.matrix(Xt)
  Y <- as.matrix(Y)
  Yt <- as.matrix(Yt)
  
  xgb_train <- xgb.DMatrix(data = X, label = Y, missing = NaN)
  xgb_test <- xgb.DMatrix(data = Xt, label = Yt, missing = NaN)
  watchlist <- list(train = xgb_train, test = xgb_test)
  
  ## feature selection -- run standard model and select most important only
  seed <- 2817
  maximize <- ifelse(eval_metric %in% c("auc"), TRUE, FALSE)
  
  paramFeature <- list(
    "objective" = objective,
    "booster" = "gbtree",
    "eval.metric" = eval_metric,
    'nthread' = detectCores()
  )
  xgbFeature <- xgb.train(param = paramFeature
                          , data = xgb_train
                          , seed = seed
                          
                          , nrounds = ntrees
                          , watchlist = watchlist
                        
                          , early_stopping_rounds = 10
                          , maximize = maximize
                          , verbose = TRUE
                          , print_every_n = 10
                          
                          , save_period = 1          ## saves the metrics at every nround
                          , prediction = T           ## saves best train prediction
  )
  ## select the most important features -- top 20
  var_importance <- xgb.importance(feature_names = colnames(X), model = xgbFeature)
  xgb.plot.importance(importance_matrix = var_importance)
  var_importance <- var_importance[1:20,]
  
  ## save variables to file
  var_importance_file <- file.path(getwd(), "train/variable_importance", paste0(paste(airport, rate, model, horizon, r, sep = "_"), ".txt"))
  write.table(var_importance, file = var_importance_file)
  
  featureSelect <- unique(var_importance$Feature)
  return(featureSelect)
}

xgb_train <- function(X, Xt, Y, Yt, niter, nfold, ntrees, objective, eval_metric){
  
  xgb_train <- xgb.DMatrix(data = X, label = Y, missing = NaN)
  xgb_test <- xgb.DMatrix(data = Xt, label = Yt, missing = NaN)
  watchlist <- list(train = xgb_train, test = xgb_test)
  
  ## control balance of positive and negative weights
  sumpos <- length(Y[Y == 1])
  sumneg <- length(Y[Y == 0])
  scale_pos_weight <- sumneg / sumpos
  
  seed <- 2817
  maximize <- ifelse(eval_metric %in% c("auc"), TRUE, FALSE)
  
  training_results <- c()
  start_time <- Sys.time()
  for(i in c(1:niter)){
    param <- list(
      "objective" = objective,
      "booster" = "gbtree",
      "eval.metric" = eval_metric,
      'nthread' = detectCores(),
      
      ## randomized grid search
      max_depth = sample(1:13, 1),
      min_child = sample(1:6, 1),
      eta = runif(1, 0.1, 0.3),
      gamma = runif(1, 0.0, 0.5),
      subsample = runif(1, 0.6, 1.0),
      colsample_bytree = runif(1, 0.5, 1.0)
    )
    xgb <- xgb.cv(param = param
                  , data = xgb_train
                  , nfold = nfold
                  , seed = seed
                  , scale_pos_weight = scale_pos_weight
                  
                  , nrounds = ntrees
                  , watchlist = watchlist
                  
                  , early_stopping_rounds = 10
                  , maximize = maximize
                  , verbose = TRUE
                  , print_every_n = 1
                  
                  , save_period = 1          ## saves the metrics at every nround
                  , prediction = T           ## saves best train prediction
    )
    ## get best iteration
    cv_scores <- as.data.frame(xgb$evaluation_log)
    best_cv_score <- cv_scores[which(cv_scores$iter == xgb$best_iteration),]
    
    param <- as.data.frame(param)
    best_cv_out <- cbind(param, best_cv_score)
    best_cv_out$iter <- i
    training_results <- rbind(training_results, best_cv_out)
    
    ## remove xgboost model
    unlink(paste0(dir,"/xgboost.model"), recursive = FALSE, force = FALSE)
  }
  end_time = Sys.time() - start_time
  print(end_time)
  
  return(training_results)
}

xgb_final <- function(X, Xt, Y, Yt, output, ntrees, objective, eval_metric){
  
  xgb_train <- xgb.DMatrix(data = X, label = Y, missing = NaN)
  xgb_test <- xgb.DMatrix(data = Xt, label = Yt, missing = NaN)
  watchlist <- list(train = xgb_train, test = xgb_test)
  
  sumpos <- length(Y[Y == 1])
  sumneg <- length(Y[Y == 0])
  scale_pos_weight <- sumneg / sumpos
  
  seed <- 2817
  maximize <- ifelse(eval_metric %in% c("auc"), TRUE, FALSE)
  
  ## train the final model
  xgbFinal_param <- list("objective" = objective
                         , "booster" = "gbtree"
                         , "eval.metric" = eval_metric
                         , 'nthread' = detectCores()
                         
                         ## parameters imported from spreadsheet
                         , 'max_depth' = output$max_depth
                         , 'min_child' = output$min_child
                         , 'gamma' = output$gamma
                         , 'subsample' = output$subsample
                         , 'colsample_bytree' = output$colsample_bytree
                         , 'eta' = output$eta
  )
  xgbFinal_model <- xgb.train(param = xgbFinal_param
                              , data = xgb_train
                              , seed = seed
                              , scale_pos_weight = scale_pos_weight
                              
                              , nrounds = ntrees
                              , watchlist = watchlist
                              
                              , early_stopping_rounds = 10
                              , maximize = maximize
                              , verbose = TRUE
                              , print_every_n = 10
                              
                              , save_period = 1          ## saves the metrics at every nround
                              , prediction = T           ## saves best train prediction
  )
  ## model summary
  var_importance <- xgb.importance(feature_names = colnames(X_train), model = xgbFinal_model)
  xgb.plot.importance(importance_matrix = var_importance)
  
  xgb_file <- file.path(dir,'results/models',paste0(airport,"_",rate,"_",model_num,"_",r,"_xgboost.model"))
  xgb.save(model = xgbFinal_model, fname = xgb_file)
}

## custom metrics
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