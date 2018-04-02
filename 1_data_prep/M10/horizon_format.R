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
  rate_freq <- aspm_data %>% group_by(arr_rate) %>% summarise(n = length(arr_rate)) %>%
    mutate(total = sum(n), pct = n / total * 100) %>%
    filter(pct > 5)
  rates <- unique(rate_freq$arr_rate)
  # rate_freq <- suppressMessages(read_csv(file.path(dir,"1_data_prep/rate_frequency.csv")))
  # rate_freq <- rate_freq %>%
  #   filter(locid == airport & rate == response & keep == 1)
  # rates <- unique(rate_freq$value)
  
  ## merge datasets by time
  horizon_data <- left_join(x = metar_data_hz, y = aspm_data_hz, by = "dt")
  
  ## sort by time -- make sure we fill in any missing gaps
  datetime <- as.POSIXct(strptime(horizon_data$dt, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  horizon_data <- cbind(datetime, horizon_data)
  horizon_data$datetime <- as.POSIXct(horizon_data$datetime)
  datetime <- horizon_data$datetime
  horizon_data <- horizon_data %>% arrange(datetime)
  
  horizon_data$hr_local <- factor(format(horizon_data$datetime,"%H"))
  horizon_data$weekday <- factor(format(horizon_data$datetime,"%a"))
  horizon_data$month <- factor(format(horizon_data$datetime,"%m"))
  
  horizon_data <- horizon_data[order(as.vector(horizon_data$datetime)),]
  rownames(horizon_data) <- 1:nrow(horizon_data)
  
  ################### set start / end dates
  bounds <- file.path('1_data_prep','rate_bounds.csv')
  bounds <- read.csv(file = bounds,header = T,
                     colClasses = c("character","character","numeric","numeric","numeric","numeric"))
  names(bounds) <- tolower(names(bounds))
  bounds <- bounds[which(bounds$airport == airport & bounds$rate == toupper(response)),]
  start_date <- strptime(bounds$start_date, "%m/%d/%y", tz = "UTC")
  
  horizon_data <- horizon_data %>% filter(datetime >= start_date)
  
  ################### data formatting
  ## dummy factor variables
  cat_vars <- sapply(horizon_data,FUN = is.numeric)
  cat_vars <- names(cat_vars[which(cat_vars == FALSE)])
  cat_vars <- cat_vars[!cat_vars %in% c('dt','datetime')] ## need to remove this variable

  if(!is.null(cat_vars)){
    dum_create <- dummyVars(dt ~ ., data = horizon_data[,names(horizon_data) %in% c("dt",cat_vars)])
    dum_features <- data.frame(predict(dum_create, newdata = horizon_data))
    names(dum_features) <- gsub("\\.","_",names(dum_features))
    horizon_data <- data.frame(horizon_data, dum_features)
    horizon_data <- horizon_data[-which(names(horizon_data) %in% c(cat_vars,"Y"))]
  }
  
  ################### final dataset
  horizon_data <- horizon_data %>% na.omit() %>% arrange(dt)
  dtrain <- horizon_data[which(horizon_data[,response] %in% c(rates)),]
  
  ## response -- create binaries for each response type; keep rate change information
  Y <- data.frame(dtrain[,c(response, "rate_lag", "rate_dt", "rate_dt_pct", "rate_change")])
  for(r in rates){
    rate_var <- paste0("rate_", r)
    Y[,rate_var] <- ifelse(Y[,response] == r, 1, 0)
  }
  
  ## date / times
  DT <- as.character(dtrain[,"dt"])
  
  ## predictors
  rem_vars <- c("week_id","datetime","dt", response, "rate_lag", "rate_dt", "rate_dt_pct", "rate_change")
  dtrain[,rem_vars] <- NULL
  X <- (dtrain)

  ################### initial datasets
  data_frames <- list(
    "X" = X
    , "Y" = Y
    , "DT" = DT
  )
  return(data_frames)
}
