################################################################################
## Name: dataset_prep.R
## Description: Merge datasets and prepare data for modeling
## Date: Jan 25, 2018
## Author: jaf
##
## Notes: Test feature selection: 1) all important variables; 2) some up to threshold; 3) remove all HCV, NZV features
## Test 1) Select features with var_imp >= 0.0005
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

names(aspm_data)

rates_keep <- c(30, 36, 38, 40)

## look at runway configurations
# table(aspm_data$runway)
runways <- aspm_data %>% group_by(runway) %>% 
  filter(arr_rate %in% rates_keep) %>%
  summarise(freq = length(runway)) %>%
  ungroup() %>%
  group_by(runway) %>%
  mutate(runway_freq = sum(freq), pct = runway_freq / nrow(aspm_data) * 100) %>%
  filter(pct >= 1)

## these three rates and runways account for 70% of the total data
runways_keep <- c(runways$runway)


data_sub <- aspm_data %>% 
  filter(runway %in% runways_keep & arr_rate %in% rates_keep )

data_merge <- merge(x = data_sub, y = metar_data, by = "dt")
data_merge$datetime <- as.POSIXct(strptime(x = data_merge$dt, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
data_merge$hr_local <- format(data_merge$datetime, "%H")

## what rates are called?
mc_summary <- data_merge %>%
  group_by(runway, arr_rate, flightRule) %>%
  summarise(freq = length(runway)) %>% 
  ungroup() %>%
  group_by(runway) %>%
  # group_by(runway, arr_rate) %>%
  mutate(total = sum(freq), pct = freq / total) %>%
  na.omit()

mc_summary %>%
  # filter(runway == runways_keep[1]) %>%
  ggplot(aes(x = factor(arr_rate), y = flightRule)) + 
  geom_tile(aes(fill = pct), colour = "white") + 
  scale_fill_gradient(low = "white", high = "steelblue") + 
  facet_wrap(~ runway, scales = "free")

## wind direction
wdir_summary <- data_merge %>%
  group_by(runway, arr_rate, wdir_cat) %>%
  summarise(freq = length(runway)) %>% 
  ungroup() %>%
  group_by(runway) %>%
  # group_by(runway, arr_rate) %>%
  mutate(total = sum(freq), pct = freq / total) %>%
  na.omit()

wdir_summary %>%
  # filter(runway == runways_keep[1]) %>%
  ggplot(aes(x = factor(arr_rate), y = wdir_cat)) + 
  geom_tile(aes(fill = pct), colour = "white") + 
  scale_fill_gradient(low = "white", high = "steelblue") + 
  facet_wrap(~ runway, scales = "free")


## hour of day?
hr_summary <- data_merge %>%
  group_by(runway, arr_rate, hr_local) %>%
  summarise(freq = length(runway)) %>% 
  ungroup() %>%
  # group_by(runway, arr_rate) %>%
  group_by(runway, hr_local) %>%
  mutate(total = sum(freq), pct = freq / total) %>%
  na.omit()

hr_summary %>%
  # filter(runway == runways_keep[1]) %>%
  ggplot(aes(x = factor(arr_rate), y = hr_local)) + 
  geom_tile(aes(fill = pct), colour = "white") + 
  scale_fill_gradient(low = "white", high = "steelblue") + 
  facet_wrap(~ runway, scales = "free")



## summarise weather features
wx_keep <- c("wdir_sin","wspd","clds_pct","vis","ceiling","qpf","rh","dewpt","temp")

wx_summary <- data_merge %>%
  na.omit() %>%
  select(runway, arr_rate, wx_keep) %>%
  gather(variable, value, -runway, -arr_rate) %>%
  group_by(runway, arr_rate, variable) %>%
  summarise(
    n = length(value),
    min = round(min(value), 2),
    p25 = round(quantile(value, 0.25), 2),
    median = round(quantile(value, 0.50), 2),
    mean = round(mean(value), 2),
    p75 = round(quantile(value, 0.75), 2),
    max = round(max(value), 2)
  ) %>%
  arrange(variable, arr_rate, runway)

data_merge %>%
  ggplot(aes(x = rh, colour = factor(arr_rate))) + 
  geom_density() +
  facet_wrap(~ runway, ncol = 1, scales = "free")

