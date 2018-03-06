################################################################################
## Name: TEST_FUNCTIONS.R
## Description: Functions to generate predictions on test set for multiclass & binary models
## Date: Feb 7, 2018
## Author: jaf
##
## Notes: 
################################################################################
change_metrics <- function(data, lev = NULL, model = NULL){
  cm <- table(data$pred_change_rev, data$act_change)
  H <- as.integer(cm[2,2])
  FA <- as.integer(cm[2,1])
  M <- as.integer(cm[1,2])
  CN <- as.integer(cm[1,1])
  acc_val <- (H + CN) / (H + CN + FA + M)
  kss_val <- ((H * CN) - (FA * M))/((H + M)*(FA + CN))
  spc_val <- (CN / (CN + FA))
  sns_val <- (H / (M + H))
  data.frame(KSS = kss_val,
             Accuracy = acc_val,
             Sensitivity = sns_val,
             Specificity = spc_val,
             Miss = M,
             Hit = H,
             CorrectNormal = CN,
             FalseAlarm = FA
  )
}

rate_metrics <- function(data){
  ## determine accuracy: do we accurately categorize the rates?
  cm <- as.matrix(table(Actual = data[,response], Predicted = data$pred)) # create matrix
  # H <- ifelse(data[,response] == data$pred, 1, 0)
  # H <- sum(H)
  # M <- ifelse(data[,response] == data$pred, 0, 1)
  # M <- sum(M)
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  accuracy = sum(diag) / n 
  precision = diag / colsums 
  recall = diag / rowsums 
  f1 = 2 * precision * recall / (precision + recall) 
  
  ## sum of hits and misses by rate
  data$Rate <- data[,response]
  rate_freq <- data %>%
    mutate(
      H = ifelse(Rate == pred, 1, 0),
      M = ifelse(Rate == pred, 0, 1)
    ) %>%
    group_by(Rate) %>%
    summarise(
      Hit = sum(H),
      Miss = sum(M)
    ) %>%
    mutate(
      F1 = f1,
      Accuracy = accuracy,
      Precision = precision,
      Recall = recall,
      Hit_Overall = sum(Hit),
      Miss_Overall = sum(Miss)
    )
  return(rate_freq)
}

binary_rate_pred <- function(r, Xt){
  model_num <- paste0(model,"_",h)
  ## load important features & subset
  featureSelect_file <- file.path(dir_new,"2_train", model, "variable_importance",paste0(paste(airport, rate, model_num, r, sep = "_"),".txt"))
  featureSelect <- read.table(featureSelect_file)
  featureSelect <- as.character(featureSelect$x)
  Xt_R <- Xt[which(names(Xt) %in% featureSelect)]
  # Xt_R <- as.matrix(Xt_R)
  
  ## load model
  svmModel_file <- file.path(model_dir,paste(airport, rate, model_num, r, 'svm_model.rda',sep = "_"))
  svmModel <- readRDS(svmModel_file)
  
  ## generate predictions
  temp_pred <- predict.train(object = svmModel,newdata = Xt_R, type = "prob")
  temp_pred <- data.frame(temp_pred[,"yes"])
  names(temp_pred) <- paste0(r,"_prob")
  return(temp_pred)
}

binary_horizon_pred <- function(dataset_dir, model_dir, airport, rate, model, h){
  # print(h)
  model_num <- paste0(model,"_",h)
  
  ## load testing data
  DT  <- readRDS(file.path(dataset_dir,paste(airport, rate, model_num, 'DT.Rds',sep = "_")))
  DTt  <- readRDS(file.path(dataset_dir,paste(airport, rate, model_num, 'DTt.Rds',sep = "_")))
  names(DT) <- "dt"
  names(DTt) <- "dt"
  Y  <- readRDS(file.path(dataset_dir,paste(airport, rate, model_num, 'Y.Rds',sep = "_")))
  Yt  <- readRDS(file.path(dataset_dir,paste(airport, rate, model_num, 'Yt.Rds',sep = "_")))
  # names(Yt) <- "actual"
  X  <- readRDS(file.path(dataset_dir,paste(airport, rate, model_num, 'X.Rds',sep = "_")))
  Xt  <- readRDS(file.path(dataset_dir,paste(airport, rate, model_num, 'Xt.Rds',sep = "_")))
  
  ## remove some vars
  rem_vars <- c("wxcodes","wdir","wdir_sin","ceiling","wx_light","wx_heavy","wx_other","lifr","ifr","mvfr","vfr",
                paste0("runway_lag",gsub("H","",h)),"time")
  X[,c(rem_vars)] <- NULL
  Xt[,c(rem_vars)] <- NULL
  
  ## for SVM -- factorize categorical variables into numeric
  X$wdir_cat <- as.numeric(factor(X$wdir_cat, levels = c("N","NE","E","SE","S","SW","W","NW"), labels = c(1:8)))
  X$flightrule <- as.numeric(factor(X$flightrule, levels = c("LIFR","IFR","MVFR","VFR"), labels = c(1:4)))
  X$ceilingflightrule <- as.numeric(factor(X$ceilingflightrule, levels = c("LIFR","IFR","MVFR","VFR"), labels = c(1:4)))
  X$visflightrule <- as.numeric(factor(X$visflightrule, levels = c("LIFR","IFR","MVFR","VFR"), labels = c(1:4)))
  X$wx_obscur <- as.numeric(as.character(X$wx_obscur))
  X$wx_precip <- as.numeric(as.character(X$wx_precip))
  X$wx_convec <- as.numeric(as.character(X$wx_convec))
  # X$wx_other <- as.numeric(X$wx_other)
  X$hr_local <- as.numeric(factor(X$hr_local, levels = c("00","01","02","03","04","05","06","07","08","09","10","11","12",
                                                         "13","14","15","16","17","18","19","20","21","22","23"),labels = c(0:23)))
  X$month<- as.numeric(factor(X$month, levels = c("01","02","03","04","05","06","07","08","09","10","11","12"), labels = c(0:11)))
  X$weekday <- as.numeric(factor(X$weekday, levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"), labels = c(0:6)))
  # X$hr_local <- sin(pi * ((360 / 24) * X$hr_local) / 180)
  # X$month <- sin(pi * ((360 / 12) * X$month) / 180)
  # X$weekday <- sin(pi * ((360 / 7) * X$weekday) / 180)
  # cat_vars <- names(X)[which(grepl("rwycnfg", names(X)) == T)]
  # cat_vars <- c(cat_vars, "wx_obscur", "wx_precip", "wx_convec", "wx_other")
  # for(var in cat_vars){X[,var] <- ifelse(X[var] == 0, -1, 1)}
  
  Xt$wdir_cat <- as.numeric(factor(Xt$wdir_cat, levels = c("N","NE","E","SE","S","SW","W","NW"), labels = c(1:8)))
  Xt$flightrule <- as.numeric(factor(Xt$flightrule, levels = c("LIFR","IFR","MVFR","VFR"), labels = c(1:4)))
  Xt$ceilingflightrule <- as.numeric(factor(Xt$ceilingflightrule, levels = c("LIFR","IFR","MVFR","VFR"), labels = c(1:4)))
  Xt$visflightrule <- as.numeric(factor(Xt$visflightrule, levels = c("LIFR","IFR","MVFR","VFR"), labels = c(1:4)))
  Xt$wx_obscur <- as.numeric(as.character(Xt$wx_obscur))
  Xt$wx_precip <- as.numeric(as.character(Xt$wx_precip))
  Xt$wx_convec <- as.numeric(as.character(Xt$wx_convec))
  # Xt$wx_other <- as.numeric(Xt$wx_other)
  Xt$hr_local <- as.numeric(factor(Xt$hr_local, levels = c("00","01","02","03","04","05","06","07","08","09","10","11","12",
                                                           "13","14","15","16","17","18","19","20","21","22","23"),labels = c(0:23)))
  Xt$month<- as.numeric(factor(Xt$month, levels = c("01","02","03","04","05","06","07","08","09","10","11","12"), labels = c(0:11)))
  Xt$weekday <- as.numeric(factor(Xt$weekday, levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"), labels = c(0:6)))
  # Xt$hr_local <- sin(pi * ((360 / 24) * Xt$hr_local) / 180)
  # Xt$month <- sin(pi * ((360 / 12) * Xt$month) / 180)
  # Xt$weekday <- sin(pi * ((360 / 7) * Xt$weekday) / 180)
  # cat_vars <- names(Xt)[which(grepl("rwycnfg", names(Xt)) == T)]
  # cat_vars <- c(cat_vars, "wx_obscur", "wx_precip", "wx_convec", "wx_other")
  # for(var in cat_vars){Xt[,var] <- ifelse(Xt[var] == 0, -1, 1)}
  
  ## pre-process -- center and scale
  preProc <- preProcess(X, method = c("scale", "center"))
  X <- predict(preProc, X)
  Xt <- predict(preProc, Xt)
  
  rates <- names(Yt)[!names(Yt) %in% c(response,"rate_lag","rate_dt","rate_dt_pct","rate_change")]
  for(r in rates){
    temp_pred <- binary_rate_pred(r, Xt)
    if(r == rates[1]){pred <- temp_pred}
    if(r != rates[1]){pred <- cbind(pred, temp_pred)}
    rm(temp_pred)
  }
  pred$rowid <- seq(1:(nrow(pred)))
  # Yt <- Yt[,!names(Yt) %in% rates]
  
  ## determine predicted rate based on max probability
  pred_max <- pred %>%
    gather(rate, pred, -rowid) %>%
    group_by(rowid) %>%
    mutate(max_pred = max(pred, na.rm = T))
  pred_max <- pred_max[which(pred_max$max_pred == pred_max$pred),]
  pred_max$pred <- as.numeric(gsub(paste(c("rate_","_prob"),collapse = "|"),"",pred_max$rate))
  pred_max <- pred_max[,c("rowid","pred")]
  
  pred$dt <- DTt$dt
  pred$dt <- as.POSIXct(strptime(x = pred$dt, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  pred <- left_join(x = pred, y = pred_max, by = "rowid")
  pred <- cbind(pred, Yt)
  
  ## determine accuracy on change data
  pred$dt_lag <- shift(x = pred$dt, n = 1, type = "lag")
  pred$time_diff <- pred$dt - pred$dt_lag
  
  ## calculate pred change
  pred$pred_lag <- shift(x = pred$pred,n = 1L,fill = NA,type = "lag")
  pred$pred_diff_pct <- ifelse(pred$pred != 0,
                               (pred$pred - pred$pred_lag) / pred$pred * 100,
                               (pred$pred - pred$pred_lag) / 1 * 100)  
  ## identify significant predicted changes
  pred$pred_change <- ifelse(abs(pred$pred_diff_pct) >= 10, 1, 0)
  pred$pred_change <- ifelse(pred$time_diff != 1, NA, pred$pred_change)
  ## determine the direction of predicted change
  pred$pred_dir <- ifelse(pred$pred_change == 0, "NONE", ifelse(pred$pred_diff_pct > 0, "POS", "NEG"))
  
  # pred$act_change <- ifelse(pred$rate_change == 0, 0, 1)
  pred$act_change <- ifelse(abs(pred$rate_dt_pct) >= 10, 1, 0)
  pred$act_dir <- ifelse(pred$rate_change == 0, "NONE", ifelse(pred$rate_change == 1, "POS", "NEG"))

  # confusionMatrix(pred$pred_change, pred$act_change)
  table(pred$pred, pred$arr_rate)
  
  ## subset missing
  metrics_dat <- pred
  metrics_miss <- metrics_dat[which(metrics_dat$act_change == 1 & metrics_dat$pred_change == 0),]
  metrics_miss <- metrics_miss$dt
  
  metrics_dat$pred_change_rev <- metrics_dat$pred_change
  # for changes that we missed, check the previous and following hour to see if the change was captured later
  # for(m in metrics_miss){
  #   temp <- metrics_dat[which(metrics_dat$dt %in% c(m, m+3600, m-3600)),]
  #   if(nrow(temp) != 0){
  #     temp <- temp[order(temp$dt),]
  #     prev_hour <- paste(temp[1,"pred_dir"],temp[1,"pred_change"])
  #     curr_hr <- paste(temp[2,"act_dir"],temp[2,"act_change"])
  #     next_hour <- paste(temp[3,"pred_dir"],temp[3,"pred_change"])
  #     prev_change <- ifelse(curr_hr == prev_hour, 1, 0)
  #     next_change <- ifelse(curr_hr == next_hour, 1, 0)
  #     if(prev_change == 1 | next_change == 1){
  #       metrics_dat$pred_change_rev <- ifelse(metrics_dat$dt == m, 1, metrics_dat$pred_change_rev)
  #     }
  #   }
  # }
  confusionMatrix(metrics_dat$pred_change_rev, metrics_dat$act_change)
  
  metrics_dat <- metrics_dat %>% data.frame()
  metrics_dat <-  within(metrics_dat,{
    diff_pct_cat <- NA
    diff_pct_cat[metrics_dat[,'pred_change']  == 0] <- "No Change"
    diff_pct_cat[metrics_dat[,'pred_change']  ==1 & metrics_dat[,'pred_diff_pct']  < 0.05] <- "<5%"
    diff_pct_cat[metrics_dat[,'pred_change']  ==1 & metrics_dat[,'pred_diff_pct']  >= 0.05 & metrics_dat[,'pred_diff_pct']  < 0.1] <- "5-10%"
    diff_pct_cat[metrics_dat[,'pred_change']  ==1 & metrics_dat[,'pred_diff_pct']  >= 0.1 & metrics_dat[,'pred_diff_pct']  < 0.2] <- "10-20%"
    diff_pct_cat[metrics_dat[,'pred_change']  ==1 & metrics_dat[,'pred_diff_pct']  >= 0.2] <- ">20%"
  })
  metrics_dat$diff_pct_cat <- factor(x = metrics_dat$diff_pct_cat, 
                                     labels = c("No Change","<5%","5-10%","10-20%",">20%"), 
                                     levels = c("No Change","<5%","5-10%","10-20%",">20%"))
  return(metrics_dat)
  
}

binary_horizon_pred_Xt <- function(dataset_dir, model_dir, airport, rate, model, h){
  # print(h)
  model_num <- paste0(model,"_",h)
  
  ## load testing data
  DTt  <- readRDS(file.path(dataset_dir,paste(airport, rate, model_num, 'DTt.Rds',sep = "_")))
  names(DTt) <- "dt"
  Yt  <- readRDS(file.path(dataset_dir,paste(airport, rate, model_num, 'Yt.Rds',sep = "_")))
  # names(Yt) <- "actual"
  Xt  <- readRDS(file.path(dataset_dir,paste(airport, rate, model_num, 'Xt.Rds',sep = "_")))
  
  rates <- names(Yt)[!names(Yt) %in% c(response,"rate_lag","rate_dt","rate_dt_pct","rate_change")]
  for(r in rates){
    temp_pred <- binary_rate_pred(r, Xt)
    if(r == rates[1]){pred <- temp_pred}
    if(r != rates[1]){pred <- cbind(pred, temp_pred)}
    rm(temp_pred)
  }
  pred$rowid <- seq(1:(nrow(pred)))
  # Yt <- Yt[,!names(Yt) %in% rates]
  
  ## determine predicted rate based on max probability
  pred_max <- pred %>%
    gather(rate, pred, -rowid) %>%
    group_by(rowid) %>%
    mutate(max_pred = max(pred, na.rm = T))
  pred_max <- pred_max[which(pred_max$max_pred == pred_max$pred),]
  pred_max$pred <- as.numeric(gsub(paste(c("rate_","_prob"),collapse = "|"),"",pred_max$rate))
  pred_max <- pred_max[,c("rowid","pred")]
  
  pred$dt <- DTt$dt
  pred$dt <- as.POSIXct(strptime(x = pred$dt, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  pred <- left_join(x = pred, y = pred_max, by = "rowid")
  pred <- cbind(pred, Yt)
  pred <- cbind(pred, Xt)
  
  ## determine accuracy on change data
  pred$dt_lag <- shift(x = pred$dt, n = 1, type = "lag")
  pred$time_diff <- pred$dt - pred$dt_lag
  
  ## calculate pred change
  pred$pred_lag <- shift(x = pred$pred,n = 1L,fill = NA,type = "lag")
  pred$pred_diff_pct <- ifelse(pred$pred != 0,
                               (pred$pred - pred$pred_lag) / pred$pred * 100,
                               (pred$pred - pred$pred_lag) / 1 * 100)  
  ## identify significant predicted changes
  pred$pred_change <- ifelse(abs(pred$pred_diff_pct) >= 5, 1, 0)
  pred$pred_change <- ifelse(pred$time_diff != 1, NA, pred$pred_change)
  ## determine the direction of predicted change
  pred$pred_dir <- ifelse(pred$pred_change == 0, "NONE", ifelse(pred$pred_diff_pct > 0, "POS", "NEG"))
  
  pred$act_change <- ifelse(abs(pred$rate_dt_pct) >= 10, 1, 0)
  pred$act_dir <- ifelse(pred$rate_change == 0, "NONE", ifelse(pred$rate_change == 1, "POS", "NEG"))
  
  confusionMatrix(pred$pred_change, pred$act_change)
  
  ## subset missing
  metrics_dat <- pred
  metrics_miss <- metrics_dat[which(metrics_dat$act_change == 1 & metrics_dat$pred_change == 0),]
  metrics_miss <- metrics_miss$dt
  
  metrics_dat$pred_change_rev <- metrics_dat$pred_change
  # for changes that we missed, check the previous and following hour to see if the change was captured later
  # for(m in metrics_miss){
  #   temp <- metrics_dat[which(metrics_dat$dt %in% c(m, m+3600, m-3600)),]
  #   if(nrow(temp) != 0){
  #     temp <- temp[order(temp$dt),]
  #     prev_hour <- paste(temp[1,"pred_dir"],temp[1,"pred_change"])
  #     curr_hr <- paste(temp[2,"act_dir"],temp[2,"act_change"])
  #     next_hour <- paste(temp[3,"pred_dir"],temp[3,"pred_change"])
  #     prev_change <- ifelse(curr_hr == prev_hour, 1, 0)
  #     next_change <- ifelse(curr_hr == next_hour, 1, 0)
  #     if(prev_change == 1 | next_change == 1){
  #       metrics_dat$pred_change_rev <- ifelse(metrics_dat$dt == m, 1, metrics_dat$pred_change_rev)
  #     }
  #   }
  # }
  confusionMatrix(metrics_dat$pred_change_rev, metrics_dat$act_change)
  
  metrics_dat <- metrics_dat %>% data.frame()
  metrics_dat <-  within(metrics_dat,{
    diff_pct_cat <- NA
    diff_pct_cat[metrics_dat[,'pred_change']  == 0] <- "No Change"
    diff_pct_cat[metrics_dat[,'pred_change']  ==1 & metrics_dat[,'pred_diff_pct']  < 0.05] <- "<5%"
    diff_pct_cat[metrics_dat[,'pred_change']  ==1 & metrics_dat[,'pred_diff_pct']  >= 0.05 & metrics_dat[,'pred_diff_pct']  < 0.1] <- "5-10%"
    diff_pct_cat[metrics_dat[,'pred_change']  ==1 & metrics_dat[,'pred_diff_pct']  >= 0.1 & metrics_dat[,'pred_diff_pct']  < 0.2] <- "10-20%"
    diff_pct_cat[metrics_dat[,'pred_change']  ==1 & metrics_dat[,'pred_diff_pct']  >= 0.2] <- ">20%"
  })
  metrics_dat$diff_pct_cat <- factor(x = metrics_dat$diff_pct_cat, 
                                     labels = c("No Change","<5%","5-10%","10-20%",">20%"), 
                                     levels = c("No Change","<5%","5-10%","10-20%",">20%"))
  return(metrics_dat)
  
}
