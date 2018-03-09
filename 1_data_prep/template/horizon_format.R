################################################################################
## Name: horizon_format.R
## Description: Functions to prepare datasets for training for each horizon
## Date: Jan 25, 2018
## Author: jaf
################################################################################

horizon_data_prep <- function(notam_data, aspm_data, metar_data, airport, response, model, horizon){
  ## format data for horizon
  notam_data_hz <- format_notam(notam_data, horizon)
  metar_data_hz <- format_metar(metar_data, horizon)
  aspm_data_hz <- format_aspm(aspm_data, horizon, response)
  
  ## create change indicator
  aspm_data_hz <- aspm_data_hz %>% data.frame()
  aspm_data_hz$rate_lag <- shift(x = aspm_data_hz[,response], n = 1L, type = "lag")
  aspm_data_hz$rate_dt <- aspm_data_hz[,response] - aspm_data_hz$rate_lag
  aspm_data_hz$rate_dt_pct <- ifelse(aspm_data_hz[,response] != 0,
                                     (aspm_data_hz[,response] - aspm_data_hz$rate_lag) / aspm_data_hz[,response] * 100,
                                     (aspm_data_hz[,response] - aspm_data_hz$rate_lag) / 1 * 100)
  aspm_data_hz$rate_change <- ifelse(aspm_data_hz$rate_dt_pct >= 5, 1, 
                                     ifelse(aspm_data_hz$rate_dt_pct <= -5, -1, 0))
  
  ## only keep data for selected rates
  rate_freq <- suppressMessages(read_csv("1_data_prep/rate_frequency.csv"))
  rate_freq <- rate_freq %>%
    filter(locid == airport & rate == response & keep == 1)
  rates <- unique(rate_freq$value)
  
  ## only keep partition rates
  # rate_part <- suppressMessages(read_csv("1_data_prep/rate_partition.csv"))
  # rate_part <- rate_part %>% filter(locid == airport & rate == response)
  # rates <- na.omit(as.numeric(unique(c(rate_part$left, rate_part$right))))
  
  ## merge datasets by time
  horizon_data <- left_join(x = metar_data_hz, y = aspm_data_hz, by = "dt")
  horizon_data <- left_join(x = horizon_data, y = notam_data_hz, by = "dt")
  # horizon_data <- horizon_data[which(horizon_data[,response] %in% c(rates)),]
  
  ## sort by time -- make sure we fill in any missing gaps
  datetime <- as.POSIXct(strptime(horizon_data$dt, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  horizon_data <- cbind(datetime, horizon_data)
  horizon_data$datetime <- as.POSIXct(horizon_data$datetime)
  datetime <- horizon_data$datetime
  horizon_data <- horizon_data %>% arrange(datetime)
  
  ## create continuous time variable
  horizon_data$time <- as.numeric(format(horizon_data$datetime,"%Y"))*8760 + 
    as.numeric(strftime(horizon_data$datetime, format = "%j"))*24 +
    as.numeric(format(horizon_data$datetime,"%H"))
  
  horizon_data$hr_local <- factor(format(horizon_data$datetime,"%H"))
  horizon_data$weekday <- factor(format(horizon_data$datetime,"%a"))
  horizon_data$month <- factor(format(horizon_data$datetime,"%m"))
  
  horizon_data <- horizon_data[order(as.vector(horizon_data$datetime)),]
  rownames(horizon_data) <- 1:nrow(horizon_data)
  
  ################### create factor variables
  factor_vars <- c("wx_obscur","wx_precip","wx_convec","wdir_cat")
  for(var in factor_vars){horizon_data[var] <- factor(horizon_data[,var])}
  
  ################### create dummies for categorical variable
  # cat_vars <- sapply(horizon_data,FUN = is.numeric)
  # cat_vars <- names(cat_vars[which(cat_vars == FALSE)])
  # cat_vars <- cat_vars[!cat_vars %in% c('dt','datetime')] ## need to remove this variable
  # 
  # if(!is.null(cat_vars)){
  #   dum_create <- dummyVars(dt ~ ., data = horizon_data[,names(horizon_data) %in% c("dt",cat_vars)])
  #   dum_features <- data.frame(predict(dum_create, newdata = horizon_data))
  #   names(dum_features) <- gsub("\\.","_",names(dum_features))
  #   horizon_data <- data.frame(horizon_data, dum_features)
  #   horizon_data <- horizon_data[-which(names(horizon_data) %in% c(cat_vars,"Y"))]
  # }
  
  ################### set start / end dates
  bounds <- file.path('1_data_prep','rate_bounds.csv')
  bounds <- read.csv(file = bounds,header = T,
                     colClasses = c("character","character","numeric","numeric","numeric","numeric"))
  names(bounds) <- tolower(names(bounds))
  bounds <- bounds[which(bounds$airport == airport & bounds$rate == toupper(response)),]
  start_date <- strptime(bounds$start_date, "%m/%d/%y", tz = "UTC")
  
  horizon_data <- horizon_data %>% filter(datetime >= start_date)
  
  ################### train / test split -- select every 5th week for training dataset
  horizon_data <- horizon_data %>% na.omit() %>% arrange(dt)
  horizon_data$week_id <- paste0(format(horizon_data$datetime,"%Y"),"-",format(horizon_data$datetime,"%W"))
  
  week <- as.data.frame(unique(horizon_data$week_id))
  # week <- horizon_data %>%
  #   group_by(week_id) %>%
  #   summarise(freq = length(week_id))
  names(week) <- "week_id"
  week$row_name <- seq(from = 1,to = nrow(week),by = 1)
  week$n <- week$row_name %% 5
  ## checking the number of observations in train / test set
  # week$train <- ifelse(week$n == 0, 0, 1)
  # week %>%
  #   group_by(train) %>%
  #   summarise(obs = sum(freq))
  
  intrain <- as.character(week[which(week$n != 0),"week_id"])
  
  ################### initial datasets
  dtrain <- horizon_data[which(horizon_data$week_id %in% intrain & horizon_data[,response] %in% c(rates)),]
  dtest <- horizon_data[which(horizon_data$week_id %!in% intrain & horizon_data[,response] %in% c(rates)),]
  ## keep full testing set with all rates
  dval <- horizon_data[which(horizon_data$week_id %!in% intrain),]
  
  ## response -- create binaries for each response type; keep rate change information
  Y <- data.frame(dtrain[,c(response, "rate_lag", "rate_dt", "rate_dt_pct", "rate_change")])
  # names(Y) <- response
  for(r in rates){
    rate_var <- paste0("rate_", r)
    Y[,rate_var] <- ifelse(Y[,response] == r, 1, 0)
  }
  Yt <- data.frame(dtest[,c(response, "rate_lag", "rate_dt", "rate_dt_pct", "rate_change")])
  # names(Yt) <- response
  for(r in rates){
    rate_var <- paste0("rate_", r)
    Yt[,rate_var] <- ifelse(Yt[,response] == r, 1, 0)
  }
  Yv <- data.frame(dval[,c(response, "rate_lag", "rate_dt", "rate_dt_pct", "rate_change")])
  # names(Yv) <- response
  for(r in rates){
    rate_var <- paste0("rate_", r)
    Yv[,rate_var] <- ifelse(Yv[,response] == r, 1, 0)
  }
  
  ## date / times
  DT <- as.character(dtrain[,"dt"])
  DTt <- as.character(dtest[,"dt"])
  DTv <- as.character(dval[,"dt"])
  
  ## predictors
  rem_vars <- c("week_id","datetime","dt", response, "rate_lag", "rate_dt", "rate_dt_pct", "rate_change")
  dtrain[,rem_vars] <- NULL
  dtest[,rem_vars] <- NULL
  dval[,rem_vars] <- NULL
  X <- (dtrain)
  Xt <- (dtest)
  Xv <- (dval)
  
  ################### initial datasets
  data_frames <- list(
    "X" = X
    , "Xt" = Xt
    , "Xv" = Xv
    , "Y" = Y
    , "Yt" = Yt
    , "Yv" = Yv
    , "DT" = DT
    , "DTt" = DTt
    , "DTv" = DTv
  )
  return(data_frames)
}
