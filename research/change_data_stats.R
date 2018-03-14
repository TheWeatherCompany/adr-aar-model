################################################################################
## Name: change_data_stats.R
## Description: Analyze the variance in features between change / no change hours
## Date: Mar 13, 2018
## Author: jaf
################################################################################
'%!in%' <- function(x,y)!('%in%'(x,y))
Sys.setenv(TZ="America/New_York")
seed <- 11182017
options(scipen = 999)

suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(splitstackshape))
suppressPackageStartupMessages(library(tidyverse))

RunBatch = 0

user <- 'jfinn'
airport <- 'LGA'
response <- 'arr_rate'

setwd("../")
dir <- getwd()

source('1_data_prep/M1/dataset_import.R')

################### import dataset
notam_data <- import_notam(user, airport)
metar_data <- import_metar(user, airport)
aspm_data <- import_aspm(user, airport)

## create change indicator
aspm_data <- aspm_data %>% data.frame()
aspm_data$rate_lag <- shift(x = aspm_data[,response], n = 1L, type = "lag")
aspm_data$rate_dt <- aspm_data[,response] - aspm_data$rate_lag
aspm_data$rate_dt_pct <- ifelse(aspm_data[,response] != 0,
                                   (aspm_data[,response] - aspm_data$rate_lag) / aspm_data[,response] * 100,
                                   (aspm_data[,response] - aspm_data$rate_lag) / 1 * 100)

## create indicator for runway change
aspm_data$runway_lag <- shift(x = aspm_data$runway, n = 1L, type = "lag")
aspm_data$runway_change <- ifelse(aspm_data$runway != aspm_data$runway_lag, 1, 0)

## create indicator for MC change
metar_data$flightRule_lag <- shift(x = metar_data$flightRule, n = 1L, type = "lag")
metar_data$flightRule_change <- ifelse(metar_data$flightRule != metar_data$flightRule_lag, 1, 0)

## create indicator for wind direction change
metar_data$wdir_cat_lag <- shift(x = metar_data$wdir_cat, n = 1L, type = "lag")
metar_data$wdir_cat_change <- ifelse(metar_data$wdir_cat != metar_data$wdir_cat_lag, 1, 0)

## keep positive / negative separate
# aspm_data$rate_change <- ifelse(aspm_data$rate_dt_pct >= 5, 1, 
#                                    ifelse(aspm_data$rate_dt_pct <= -5, -1, 0))
## any change
change_thresh <- 0
aspm_data$rate_change <- ifelse(abs(aspm_data$rate_dt_pct) > change_thresh, 1, 0)

## merge datasets
airport_data <- left_join(x = metar_data, y = aspm_data, by = "dt")
airport_data <- left_join(x = airport_data, y = notam_data, by = "dt")

## sort by time -- make sure we fill in any missing gaps
datetime <- as.POSIXct(strptime(airport_data$dt, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
airport_data <- cbind(datetime, airport_data)
airport_data$datetime <- as.POSIXct(airport_data$datetime)
datetime <- airport_data$datetime
airport_data <- airport_data %>% arrange(datetime)

## time difference 
airport_data$dt_lag <- shift(x = airport_data$datetime, n = 1, type = "lag")
airport_data$time_diff <- airport_data$datetime - airport_data$dt_lag

## only keep data for selected rates
rate_freq <- suppressMessages(read_csv(file.path(dir,"1_data_prep/rate_frequency.csv")))
rate_freq <- rate_freq %>%
  filter(locid == airport & rate == response & keep == 1)
rates <- unique(rate_freq$value)

## what is the relationship between rate change and runway change?
test <- na.omit(airport_data)
# test <- test %>% filter(arr_rate %in% rates)
xtab <- table(test$rate_change, test$runway_change)
cor.test(test$rate_change, test$runway_change)
confusionMatrix(xtab)

## what hours of day do the rate changes occur on?
airport_data$hour <- format(airport_data$datetime, "%H")
airport_data %>%
  group_by(hour) %>%
  summarise(n = sum(rate_change, na.rm = T)) %>%
  ggplot(aes(x = hour, y = n)) +
    geom_bar(stat = "identity") + theme_bw() + 
    xlab('Hour of Day (UTC)') + ylab('Total Rate Changes')

## what about runway configuration changes?
airport_data %>%
  group_by(hour) %>%
  summarise(n = sum(runway_change, na.rm = T)) %>%
  ggplot(aes(x = hour, y = n)) +
  geom_bar(stat = "identity") + theme_bw() + 
  xlab('Hour of Day (UTC)') + ylab('Total Runway Config. Changes')

## there are more runway changes than rate changes
# ctrl <- trainControl(savePredictions = TRUE)
# test$rate_change <- factor(test$rate_change)
# m1 <- train(rate_change ~ flightRule_change + wdir_cat_change + wdir_cat + wdir_cat_lag + flightRule, data = test, method = "rpart", trControl = ctrl)
# m1
# print(m1$finalModel)
# plot(m1$finalModel)
# test$pred <- predict(m1, test)
# confusionMatrix(test$pred, test$rate_change)




################### summary statistic calculations
calc_vars <- c("dep_ct","arr_ct","oag_dep","oag_arr","etms_dep","etms_arr","dep_demand","arr_demand","dep_rate","arr_rate"
               , "rwycnfg_LGA_22","rwycnfg_LGA_4","rwycnfg_LGA_31","rwycnfg_LGA_13"
               , "clds_pct","wdir_sin","wspd","ceiling","ceiling_ln","vis"
               , "hrs_precip","hrs_obscur","hrs_convec","hrs_precip_daily","hrs_obscur_daily","hrs_convec_daily"
)

## first -- calculate the lag
delta_vars <- c()
for(var in calc_vars){
  lag_var1 <- paste0(var, "_lag")
  lag_var2 <- paste0(var, "_lag1")
  delta_var1 <- paste0(var, "_dt")
  delta_var2 <- paste0(var, "_dt_lag1")
  airport_data[,lag_var1] <- shift(x = airport_data[,var],n = 1L,fill = NA,type = "lag")
  airport_data[,lag_var2] <- shift(x = airport_data[,var],n = 2L,fill = NA,type = "lag")
  airport_data[,delta_var1] <- ifelse(airport_data$time_diff != 1, NA, airport_data[,var] - airport_data[,lag_var1])
  airport_data[,delta_var2] <- ifelse(airport_data$time_diff != 1, NA, airport_data[,lag_var1] - airport_data[,lag_var2])
  
  delta_vars <- c(delta_vars, delta_var1, delta_var2)
  # airport_data$lag_dum1 <- airport_data$lag_dum2 <- NULL
  rm(lag_var1, lag_var2, delta_var1, delta_var2)
}

## difference in changes
change_summary <- airport_data %>%
  filter(arr_rate %in% rates)
change_summary <- change_summary %>%
  filter(!is.na(rate_change)) %>%
  select(dt, rate_change, delta_vars) %>%
  gather(feature, value, -dt, -rate_change) %>%
  group_by(feature, rate_change) %>%
  summarise(
    min = min(value, na.rm = T),
    median = quantile(value, 0.5, na.rm = T),
    max = max(value, na.rm = T),
    mean = mean(value, na.rm = T),
    std = sd(value, na.rm = T)
  )


## look at some actual values
check <- airport_data %>%
  # filter(arr_rate %in% rates) %>%
  # select(dt, datetime, response, flightRule, runway, wdir_cat, ceiling, 
         # rate_change, flightRule_change, wdir_cat_change, runway_change
         # , ceiling_lag, ceiling_lag1, ceiling_dt, ceiling_dt_lag1
  # ) %>%
  # na.omit() %>%
  select(dt, arr_rate, flightRule, wdir_cat, wdir, runway, runway_change, rate_change, vis, ceiling, ceiling_lag, ceiling_lag1, ceiling_dt, ceiling_dt_lag1)
  # select(dt, arr_rate, rate_change, arr_demand, arr_demand_lag, arr_demand_lag1, arr_demand_dt, arr_demand_dt_lag1)
check$runway <- factor(check$runway)
check$wdir_cat <- factor(check$wdir_cat)
check$arr_rate <- factor(check$arr_rate)

ctrl <- trainControl(savePredictions = TRUE)
m1 <- train(runway ~ wdir_cat, data = na.omit(check), method = "rpart", trControl = ctrl)
m1
print(m1$finalModel)
m1

check$pred <- predict(object = m1, newdata = check)

 ## plot 1 -- rate change ahead of weather change
airport_data %>%
  filter(as.POSIXct(strptime('2014-01-01 14:00:00', format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) <= datetime & 
           datetime <= as.POSIXct(strptime('2014-01-02 06:00:00', format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))) %>%
  select(datetime, response, ceiling, wdir) %>%
  gather(variable, value, -datetime) %>%
  ggplot(aes(x = datetime)) +
  geom_point(aes(y = value)) +
  facet_wrap(~ variable, scales = "free", nrow = 3)

## plot 2 -- rate distribution by runway configuration
runway_freq <- airport_data %>%
  group_by(runway) %>%
  summarize(n = length(runway)) %>%
  arrange(desc(n)) %>%
  filter(n > 100  & !is.na(runway))
runways <- runway_freq$runway
runway_rates <- airport_data %>% 
  # filter(runway %in% runways & arr_rate %in% rates) %>%
  group_by(runway, flightRule) %>%
  summarise(
    n = length(arr_rate),
    min = min(arr_rate, na.rm = T),
    median = quantile(arr_rate, 0.5, na.rm = T),
    max = max(arr_rate, na.rm = T),
    mean = mean(arr_rate, na.rm = T),
    std = sd(arr_rate, na.rm = T)
  )
  # summarise(rate_freq = length(arr_rate)) %>%
  # ggplot(aes(x = runway, y = rate_freq, fill = factor(arr_rate))) +
  # geom_bar(stat = "identity", position = "dodge")



dep_ct <- change_summary %>%
  filter(feature %in% c("dep_ct_dt", "dep_ct_dt_lag1"))
