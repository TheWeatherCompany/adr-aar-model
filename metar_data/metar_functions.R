################################################################################
## Name: metar_functions.R
## Description: Functions to support METAR data processing
## Date: Jan 19, 2018
## Author: jaf
##
## Notes: improve outlier removal of response variable
################################################################################
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(RCurl))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(splitstackshape))
suppressPackageStartupMessages(library(RcppRoll))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(randomForest))

source('/Users/jfinn/Google Drive/ADR AAR Model Build/2017 Model Re-Train/metar_data/metar_subfunctions.R')

metar_download_mesonet <- function(start_date, end_date, input_network, input_state, input_faaid){
  service <- "https://mesonet.agron.iastate.edu/cgi-bin/request/asos.py?"
  service <- paste0(service, "data=all&tz=Etc/UTC&format=onlycomma&latlon=yes&")
  service <- paste0(service, "year1=", year(start_date), "&month1=", month(start_date), "&day1=", mday(start_date), "&")
  service <- paste0(service, "year2=", year(end_date), "&month2=", month(end_date), "&day2=", mday(end_date), "&")
  
  network <- paste0(input_state, "_ASOS")
  
  ## get metadata -- need sitename
  uri <- str_c("https://mesonet.agron.iastate.edu/geojson/network/", network, ".geojson", sep="")
  data <- url(uri)
  jdict <- fromJSON(data)
  network_sites <- data.frame(jdict$features$properties)
  network_sites <- network_sites[which(network_sites$sid == input_faaid),]
  sitename <- network_sites$sname
  
  ## download data from site
  metar_url <- str_c(service, "station=", input_faaid)
  print(str_c("Network:", network, "Downloading:", sitename, input_faaid, sep=" "))
  data <- url(metar_url)
  datestring1 <- format(start_date, "%Y%m%d")
  datestring2 <- format(end_date, "%Y%m%d")
  output_raw <- str_c(input_faaid, "_", datestring1, "_to_", datestring2, ".txt")
  output_raw <- file.path("raw", output_raw)
  download.file(metar_url, output_raw, "auto")
}

metar_process <- function(dat){
  
  dat$clds1_pct <- clean_clouds(replace(dat$skyc1,dat$skyc1=='M',NA))
  dat$clds2_pct <- clean_clouds(replace(dat$skyc2,dat$skyc2=='M',NA))
  dat$clds3_pct <- clean_clouds(replace(dat$skyc3,dat$skyc3=='M',NA))
  dat$clds4_pct <- clean_clouds(replace(dat$skyc4,dat$skyc4=='M',NA))
  dat$clds1_lvl <- replace_M(dat$skyl1) ## altitude - feet
  dat$clds2_lvl <- replace_M(dat$skyl2) ## altitude - feet
  dat$clds3_lvl <- replace_M(dat$skyl3) ## altitude - feet
  dat$clds4_lvl <- replace_M(dat$skyl4) ## altitude - feet
  dat$mslp <- replace_M(dat$mslp) ## pressure in millibar
  dat$qpf <- replace_M(dat$p01i) ## one hour precipitation, inches
  dat$rh <- round(replace_M(dat$relh),1) ## relative humidity %
  dat$dewpt <- round(replace_M(dat$dwpf),1) ## dewpoint, F
  dat$temp <- round(replace_M(dat$tmpf),1) ## temperature, F
  dat$gust <- round(replace_M(dat$gust),1) ## wind gust, knots
  dat$wdir <- replace_M(dat$drct) ## wind direction, degrees from N
  dat$wdir_sin <- wdir_to_num(replace_M(dat$drct))
  dat$wspd <- round(replace_M(dat$sknt),1) ## wind speed in knows
  dat$vis <- round(replace_M(dat$vsby),1) ## visibility in miles
  dat$clds_pct <- dat$clds1_pct
  
  ## create date / time variables
  dat$datetime <- strptime(dat$valid,format = "%Y-%m-%d %H:%M",tz = "UTC")
  dat$date <- as.Date(dat$datetime)
  dat$hour <- format(dat$datetime,"%H")
  dat$min <- as.numeric(format(dat$datetime,"%M"))
  
  ## determine ceiling - lowest level where condition is either BKN or OVC; otherwise default to 99900
  dat$ceiling <- ifelse(dat$clds1_pct >= 87, dat$clds1_lvl,
                        ifelse(dat$clds2_pct >= 87, dat$clds2_lvl,
                               ifelse(dat$clds3_pct >= 87, dat$clds3_lvl,dat$clds4_lvl)))
  
  ## remove outlier
  dat$ceiling <- ifelse(dat$ceiling >= 50000, NA, dat$ceiling)
  
  ## adjust ceiling - by station; replace NA values with max/freq ceiling value reported
  dat$ceiling <- ifelse(is.na(dat$ceiling),
                        ifelse(dat$station == "SFO", 20000,
                               ifelse(dat$station == "DFW", 30000, 25000)),
                        dat$ceiling)
  
  ## create date / time variables
  dat$datetime <- as.POSIXct(strptime(paste(dat$date,dat$hour),format = "%Y-%m-%d %H",tz = "UTC"))
  ## remove missing observations
  dat <- dat[which(!is.na(dat$temp) & !is.na(dat$mslp)),]
  dat <- dat[!duplicated(dat$datetime),]
  ## don't need to take hourly average, etc -- just remove non-full synoptic observations
  dat <- dat %>%
    select(datetime, date, hour, station, wxcodes,
           clds_pct, wdir, wdir_sin, wspd, vis, ceiling,
           qpf, rh, dewpt, temp
    ) %>%
    ## remove days with < 75% data
    group_by(date) %>%
    mutate(
      daily_hrs = length(date)
    ) %>%
    ungroup() %>%
    filter(daily_hrs >= 18) %>%
    mutate(daily_hrs = NULL)
  
  
  ## define wind categories
  dat <- within(dat,{
    wdir_cat <- NA
    wdir_cat[dat[,'wdir'] >= 0 & dat[,'wdir'] <= 22.5 ] <- "N"
    wdir_cat[dat[,'wdir'] > 22.5 & dat[,'wdir'] <= 67.5 ] <- "NE"
    wdir_cat[dat[,'wdir'] > 67.5 & dat[,'wdir'] <= 112.5 ] <- "E"
    wdir_cat[dat[,'wdir'] > 112.5 & dat[,'wdir'] <= 157.5 ] <- "SE"
    wdir_cat[dat[,'wdir'] > 157.5 & dat[,'wdir'] <= 202.5 ] <- "S"
    wdir_cat[dat[,'wdir'] > 202.5 & dat[,'wdir'] <= 247.5 ] <- "SW"
    wdir_cat[dat[,'wdir'] > 247.5 & dat[,'wdir'] <= 292.5 ] <- "W"
    wdir_cat[dat[,'wdir'] > 292.5 & dat[,'wdir'] <= 337.5 ] <- "NW"
    wdir_cat[dat[,'wdir'] > 337.5 ] <- "N"
  })
  
  ## define VFR / IFR conditions (overall, ceiling, and visibility)
  dat <-  within(dat,{
    ceilingFlightRule <- NA
    ceilingFlightRule[dat[,'ceiling']  < 500] <- 0
    ceilingFlightRule[dat[,'ceiling']  >= 500 & dat[,'ceiling']  < 1000] <- 1
    ceilingFlightRule[dat[,'ceiling']  >= 1000 & dat[,'ceiling']  < 3000] <- 2
    ceilingFlightRule[dat[,'ceiling']  >= 3000] <- 3
  })
  dat <-  within(dat,{
    visFlightRule <- NA
    visFlightRule[dat[,'vis']  < 1] <- 0
    visFlightRule[dat[,'vis']  >= 1 & dat[,'vis']  < 3] <- 1
    visFlightRule[dat[,'vis']  >= 3 & dat[,'vis']  < 5] <- 2
    visFlightRule[dat[,'vis']  >= 5] <- 3
  })
  
  dat <- dat %>%
    transform(
      ## define overall flight rule based on worst ceiling/vis conditions
      flightRule = pmin(visFlightRule, ceilingFlightRule)
    ) %>% 
    mutate(
      ## conver to appropriate units
      ceiling_ln = log(ceiling + 1),
      wspd = round(wspd * 1.15, 2),  # convert from knts to mph
      
      ## factorized rule variables
      visFlightRule = factor(visFlightRule, levels = c(0,1,2,3), labels = c("LIFR","IFR","MVFR","VFR")),
      ceilingFlightRule = factor(ceilingFlightRule, levels = c(0,1,2,3), labels = c("LIFR","IFR","MVFR","VFR")),
      flightRule = factor(flightRule, levels = c(0,1,2,3), labels = c("LIFR","IFR","MVFR","VFR"))
      
    ) 
  
  ##################################### weather conditions
  
  ## parse the wxcodes variable
  dat$wx_obscur <- 0
  dat$wx_obscur[grepl('BR|FG|FU|VA|DU|SA|HZ|PY', dat$wxcodes)] <- 1
  dat$wx_precip <- 0
  dat$wx_precip[grepl('DZ|RA|SN|SG|IC|PL|GR|GS|UP', dat$wxcodes)] <-  1
  dat$wx_convec <- 0
  dat$wx_convec[grepl('TS', dat$wxcodes)] <-  1
  dat$wx_other <- 0
  dat$wx_other[grepl('PO|SQ|FC|SS|DS', dat$wxcodes)] <-  1
  dat$wx_light <- 0
  dat$wx_light[grepl('\\-', dat$wxcodes)] <- 1
  dat$wx_heavy <- 0
  dat$wx_heavy[grepl('\\+', dat$wxcodes)] <- 1
  
  ##################################### calculate the cumulative hours of weather phenomenon
  setDT(dat)[, hrs_precip := seq_len(.N), by = list(wx_precip, cumsum(wx_precip == 0L))]
  setDT(dat)[, hrs_obscur := seq_len(.N), by = list(wx_obscur, cumsum(wx_obscur == 0L))]
  setDT(dat)[, hrs_convec := seq_len(.N), by = list(wx_convec, cumsum(wx_convec == 0L))]
  
  dat <- dat %>% 
    mutate(
      hrs_precip = ifelse(wx_precip == 0, 0, hrs_precip),
      hrs_obscur = ifelse(wx_obscur == 0, 0, hrs_obscur),
      hrs_convec = ifelse(wx_convec == 0, 0, hrs_convec)
    ) %>% 
    group_by(station, date) %>%
    mutate(
      ## calculate the total daily hours of weather phenomenon
      hrs_precip_daily = sum(wx_precip),
      hrs_obscur_daily = sum(wx_obscur),
      hrs_convec_daily = sum(wx_convec)
    ) %>%
    ungroup()
  
  ## IFV / VFR -- calculate cumulative hours during day; total daily hours of each condition
  flight_rule_dum <- dummyVars(" ~ flightRule", data = dat, fullRank = F)
  flight_rule <- data.frame(predict(flight_rule_dum, newdata = dat))
  names(flight_rule) <- gsub("flightRule\\.","",names(flight_rule))
  
  ## calculate the cumulative hours of each flight rule
  setDT(flight_rule)[, hrs_LIFR := seq_len(.N), by = list(LIFR, cumsum(LIFR == 0L))]
  setDT(flight_rule)[, hrs_IFR := seq_len(.N), by = list(IFR, cumsum(IFR == 0L))]
  setDT(flight_rule)[, hrs_MVFR := seq_len(.N), by = list(MVFR, cumsum(MVFR == 0L))]
  setDT(flight_rule)[, hrs_VFR := seq_len(.N), by = list(VFR, cumsum(VFR == 0L))]
  
  dat <- cbind(dat, flight_rule)
  dat <- dat %>% 
    mutate(
      hrs_LIFR = ifelse(LIFR == 0, 0, hrs_LIFR),
      hrs_IFR = ifelse(IFR == 0, 0, hrs_IFR),
      hrs_MVFR = ifelse(MVFR == 0, 0, hrs_MVFR),
      hrs_VFR = ifelse(VFR == 0, 0, hrs_VFR)
    ) %>% 
    group_by(station, date) %>%
    mutate(
      ## calculate the total daily hours of weather phenomenon
      hrs_LIFR_daily = sum(LIFR),
      hrs_IFR_daily = sum(IFR),
      hrs_MVFR_daily = sum(MVFR),
      hrs_VFR_daily = sum(VFR)
    ) %>%
    ungroup()
  
  
  ##################################### lags & changes
  ## make sure we have all time periods -- fill in missing gaps
  dat$date <- NULL
  dat$hour <- NULL
  dat <- dat %>% arrange(datetime)
  datetime <- seq(from = min(dat$datetime), to = max(dat$datetime), by = "hour")
  datetime <- data.frame(datetime)
  dat <- left_join(x = datetime, y = dat, by = "datetime")
  missing <- dat[which(is.na(dat$station)),]
  dat <- dat[order(dat$datetime),]
  
  ## for 1-24 hour periods, calculate the max, min, average for weather features
  # names(dat) <- tolower(names(dat))
  # wx_calc_vars <- c("wspd","wdir_sin","vis","ceiling","ceiling_ln","clds_pct",
  #                   # "temp","dewpt","rh",
  #                   "wx_obscur","wx_precip","wx_convec","wx_other","wx_light","wx_heavy",
  #                   "hrs_precip","hrs_obscur","hrs_convec","hrs_precip_daily","hrs_obscur_daily",
  #                   "lifr","ifr","mvfr","vfr","hrs_lifr","hrs_ifr","hrs_mvfr","hrs_vfr",
  #                   "hrs_lifr_daily","hrs_ifr_daily","hrs_mvfr_daily","hrs_vfr_daily"
  # )
  # for(var in wx_calc_vars){
  #   # print(var)
  #   ## calculate rolling min, max, avg values
  #   temp_dat <- wx_calcs(dat, var)
  #   # temp_dat$datetime <- as.character(temp_dat$datetime)
  #   ## create lag variables
  #   lag_vars <- names(temp_dat)[!names(temp_dat) %in% c("datetime")]
  #   for(l_var in lag_vars){
  #     temp_dat_lag <- wx_lags(temp_dat, l_var)
  #     temp_dat_lag[,c("datetime", l_var)] <- NULL
  #     temp_dat <- cbind(temp_dat, temp_dat_lag)
  #     rm(temp_dat_lag)
  #   }
  #   temp_dat[,c(var, "datetime")] <- NULL
  #   dat <- cbind(dat, temp_dat)  
  #   rm(temp_dat, var)
  #   
  #   # ## just lags
  #   # for(hr in c(seq(from = 1, to = 12, by = 1))){
  #   #   ## calculate the lag
  #   #   lag_val <- shift(x = dat[,var], n = hr, type ="lag")
  #   #   lag_var <- paste0(var,"_lag",abs(hr))
  #   #   
  #   #   ## calculate the n hour change
  #   #   delta_val <- dat[,var] - lag_val
  #   #   # delta_var <- paste0(var,"_ch",abs(hr))
  #   #   
  #   #   dat[,lag_var] <- lag_val
  #   #   # var_val[,delta_var] <- delta_val
  #   # }
  # }
  
  ## convert to local time
  ## create variable for each time zone
  # dat$dt_est <- format(x = dat$datetime,tz = "Etc/GMT+5")
  # dat$dt_pst <- format(x = dat$datetime,tz = "Etc/GMT+8")
  # dat$dt_cst <- format(x = dat$datetime,tz = "Etc/GMT+6")
  # ## set correct datetime based on airport tz
  # datetime <- ifelse(dat$station %in% c("ATL","BOS","EWR","JFK","LGA","PHL"),dat$dt_est,
  #              ifelse(dat$station %in% c("LAX","SFO"),dat$dt_pst,
  #                     ifelse(dat$station %in% c ("DFW","ORD"),dat$dt_cst,NA)))
  # 
  # dat$datetime <- NULL
  # dat <- cbind(datetime, dat)
  ## remove extra variables
  dat$dt <- as.character(strptime(dat$datetime,format = "%Y-%m-%d %H:%M",tz = "UTC"))
  dat$datetime <- NULL
  # dat$dt_est <- NULL
  # dat$dt_pst <- NULL
  # dat$dt_cst <- NULL
  
  return(dat)
}
