################################################################################
## Name: wx_comp_sfo.R
## Description: Compare METAR data to weather data obtained from ASPM
## Date: Mar 20 2018
## Author: jaf
##
## Notes: 
################################################################################
'%!in%' <- function(x,y)!('%in%'(x,y))
Sys.setenv(TZ="America/New_York")
seed <- 11182017

suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(RcppRoll))

RunBatch = 0

setwd('../')

if(RunBatch == 0){
  user <- 'jfinn'
  airport <- 'SFO'
  response <- 'ARR_RATE'
  model <- 'M8'
  horizons <- paste0("H1")
}

if(RunBatch == 1){
  args <- commandArgs(trailingOnly = TRUE)
  user <- args[1]
  airport <- args[2]
  response <- args[3]
  model <- args[4]
  horizons <- args[5:length(args)]
}

###################
source(file.path(model, 'dataset_import.R'))
source(file.path(model, 'horizon_format.R'))

setwd("../")
dir <- getwd()

print(user)
print(airport)
print(response)
print(model)

################### import dataset
notam_data <- import_notam(user, airport)
metar_data <- import_metar(user, airport)
aspm_data <- import_aspm(user, airport)

## subset metar data
metar_wx <- c("flightRule","ceiling","vis","temp","wdir","wspd")
metar_data <- metar_data[,c("dt", metar_wx)]
names(metar_data) <- c("dt","flightRule","ceiling_metar","visible_metar","temp_metar","wnd_angl_metar","wnd_sped_metar")
max_ceiling <- quantile(metar_data$ceiling_metar, 0.99,na.rm = T)
metar_data$ceiling_metar <- ifelse(metar_data$ceiling_metar > max_ceiling, max_ceiling, metar_data$ceiling_metar)

## subset aspm data
names(aspm_data)
aspm_wx <- c("mc","ceiling","visible","temp","wnd_angl","wnd_sped")
aspm_data <- aspm_data[,c("dt","arr_rate", aspm_wx)]
## rename weather features
for(var in c("mc","ceiling","visible","temp","wnd_angl","wnd_sped")){
  names(aspm_data) <- gsub(var, paste0(var, "_aspm"), names(aspm_data))
}
## format weather features a bit
aspm_data$temp_aspm <- as.numeric(aspm_data$temp_aspm)
aspm_data$wnd_angl_aspm <- as.numeric(aspm_data$wnd_angl_aspm)
aspm_data$ceiling_aspm <- as.numeric(aspm_data$ceiling_aspm) * 100
## correct ceiling
aspm_data$ceiling_aspm <- ifelse(aspm_data$ceiling_aspm > max_ceiling, max_ceiling, aspm_data$ceiling_aspm)

 ## merge aspm data with metar data
data_merge <- merge(x = aspm_data, y = metar_data, by = "dt")
data_merge$datetime <- as.POSIXct(strptime(x = data_merge$dt, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
data_merge$hr_local <- format(data_merge$datetime, "%H")
data_merge$month <- as.numeric(format(data_merge$datetime, "%m"))
data_merge$year <- as.numeric(format(data_merge$datetime, "%Y"))
data_merge$mc_metar <- ifelse(data_merge$flightRule %in% c("VFR"), "VFR", "IFR")

## just focus on operational hours
overnight_hours <- c("06","07","08","09","10","11","12","13","14")
data_merge <- data_merge %>%
  filter(!hr_local %in% overnight_hours)

## we want to compare aspm weather data to metar weather data
table(data_merge$mc_metar, data_merge$mc_aspm)
table(data_merge$flightRule, data_merge$mc_aspm)

mc <- data_merge %>%
  select(datetime, arr_rate, flightRule, mc_metar, mc_aspm, visible_metar, visible_aspm, ceiling_metar, ceiling_aspm)

mc %>%
  ggplot(aes(x = visible_metar, y = visible_aspm)) + geom_point() + geom_smooth(method = "lm")

## look at case where MC is misclassified
data_merge %>%
  filter(datetime >= "2014-01-07 07:00:00" & datetime <= "2014-01-08 08:00:00") %>%
  mutate(
    flightRule = as.numeric((flightRule)),
    mc_metar = ifelse(mc_metar == "VFR", 1, 0),
    mc_aspm = ifelse(mc_aspm == "V", 1, 0)
  ) %>%
  select(datetime, arr_rate, visible_metar, visible_aspm, ceiling_metar, ceiling_aspm, mc_metar, mc_aspm) %>%
  gather(variable, value, -datetime) %>%
  mutate(
    source = ifelse(grepl("metar", variable) == TRUE, "METAR", "ASPM"),
    variable = gsub("_aspm","",variable),
    variable = gsub("_metar","",variable)
  ) %>%
  ggplot(aes(x = datetime, y = value, colour = source)) + 
  geom_point() + geom_line() +
  facet_wrap( ~ variable, scales = "free", ncol = 1)

## some summary stats by MC
mc_aspm_summary <- data_merge %>%
  na.omit() %>%
  mutate(
    # mc_metar = ifelse(mc_metar == "VFR", 1, 0),
    mc_aspm = ifelse(mc_aspm == "V", 1, 0)
  ) %>%
  select(datetime, arr_rate, visible_metar, visible_aspm, ceiling_metar, ceiling_aspm, mc_aspm) %>%
  gather(variable, value, -datetime, -mc_aspm) %>%
  mutate(
    source = ifelse(grepl("metar", variable) == TRUE, "METAR", "ASPM"),
    variable = gsub("_aspm","",variable),
    variable = gsub("_metar","",variable)
  ) %>%
  group_by(mc_aspm, source, variable) %>%
  summarise(
    min = round(min(value), 2),
    p25 = round(quantile(value, 0.25), 2),
    median = round(quantile(value, 0.50), 2),
    mean = round(mean(value), 2),
    p75 = round(quantile(value, 0.75), 2),
    max = round(max(value), 2)
  ) %>%
  arrange(mc_aspm, variable, source) %>%
  ungroup() %>%
  mutate(
    MC = mc_aspm,
    MC_source = "ASPM"
  ) %>%
  select(-mc_aspm)

mc_metar_summary <- data_merge %>%
  na.omit() %>%
  mutate(
    mc_metar = ifelse(mc_metar == "VFR", 1, 0)
    # mc_aspm = ifelse(mc_aspm == "V", 1, 0)
  ) %>%
  select(datetime, arr_rate, visible_metar, visible_aspm, ceiling_metar, ceiling_aspm, mc_metar) %>%
  gather(variable, value, -datetime, -mc_metar) %>%
  mutate(
    source = ifelse(grepl("metar", variable) == TRUE, "METAR", "ASPM"),
    variable = gsub("_aspm","",variable),
    variable = gsub("_metar","",variable)
  ) %>%
  group_by(mc_metar, source, variable) %>%
  summarise(
    min = round(min(value), 2),
    p25 = round(quantile(value, 0.25), 2),
    median = round(quantile(value, 0.50), 2),
    mean = round(mean(value), 2),
    p75 = round(quantile(value, 0.75), 2),
    max = round(max(value), 2)
  ) %>%
  arrange(mc_metar, variable, source) %>%
  ungroup() %>%
  mutate(
    MC = mc_metar,
    MC_source = "METAR"
  ) %>%
  select(-mc_metar)

wx_summary <- rbind(mc_aspm_summary, mc_metar_summary)

cor.test(mc$visible_aspm, mc$visible_metar)

## can we correct the METAR mc using SFO's criteria?
mc$mc_metar_corr <- ifelse(mc$ceiling_metar < 5000 | mc$visible_metar < 8, "IFR", "VFR")
table(mc$mc_metar_corr, mc$mc_aspm)

## ASPM IFR conditions persist for one hour -- can we match this to our METAR?
mc$mc_metar_lag <- lag(x = mc$mc_metar_corr, n = 1, type = "lag")
mc$mc_metar_corr_lag <- ifelse(mc$mc_metar_corr == "VFR" & mc$mc_aspm == "I" & mc$mc_metar_lag == "IFR",
                               "IFR", mc$mc_metar_corr)
table(mc$mc_metar_corr_lag, mc$mc_aspm)
mc$mc_aspm_new <- ifelse(mc$mc_aspm == "I", "IFR", "VFR")
mc$miss <- ifelse(mc$mc_aspm_new == "IFR" & mc$mc_metar_corr_lag == "VFR", -1,
                  ifelse(mc$mc_aspm_new == "VFR" & mc$mc_metar_corr_lag == "IFR", 1, 0))
table(mc$miss)

mc_sub <- mc %>%
  select(datetime, miss, mc_aspm, mc_metar, mc_metar_lag, mc_metar_corr_lag, visible_metar, visible_aspm, ceiling_metar, ceiling_aspm)
mc_sub$ceiling_diff <- mc_sub$ceiling_metar - mc_sub$ceiling_aspm
mc_sub$visible_diff <- mc_sub$visible_metar - mc_sub$visible_aspm
  
  
mc_sub %>%
  na.omit() %>%
  group_by(miss) %>%
  summarise(
    n = length(miss),
    min = round(min(ceiling_diff), 2),
    p25 = round(quantile(ceiling_diff, 0.25), 2),
    median = round(quantile(ceiling_diff, 0.50), 2),
    mean = round(mean(ceiling_diff), 2),
    p75 = round(quantile(ceiling_diff, 0.75), 2),
    max = round(max(ceiling_diff), 2)
  )

mc_sub %>%
  na.omit() %>%
  group_by(miss) %>%
  summarise(
    n = length(miss),
    min = round(min(visible_diff), 2),
    p25 = round(quantile(visible_diff, 0.25), 2),
    median = round(quantile(visible_diff, 0.50), 2),
    mean = round(mean(visible_diff), 2),
    p75 = round(quantile(visible_diff, 0.75), 2),
    max = round(max(visible_diff), 2)
  )
