################################################################################
## Name: TRAIN_OVA_BINARY_SVM.R
## Description: Train binary model for each high frequency rate (OVA) -- SVM
## Date: Feb 27, 2018
## Author: jaf
##
## Notes: Feature engineering & processing specific to SVM
##        Categorical variables converted to numeric (ordinal)
##        Binary variables == 0 or 1
##        All features centered & scaled
################################################################################
### load libraries
suppressMessages(library(caret))
suppressMessages(library(plyr))
suppressMessages(library(reshape2))
suppressMessages(library(data.table))
suppressMessages(library(doParallel))
suppressMessages(library(tidyverse))
suppressMessages(library(mlbench))

Sys.setenv(TZ="America/New_York")

setwd("../")
dir <- getwd()

################################################
## set model parameters

RunBatch = 0

if(RunBatch == 0){
  user <- 'jfinn'
  airport <- 'LGA'
  response <- 'ARR_RATE'
  model <- 'M1'
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

# ################################################
# ## load in model datasets 
# 
# X <- file.path(file.path(dir,'1_data_prep',model),paste0(airport,"_",rate,'_',model_num,'_X.Rds'))
# X <- readRDS(file = X)
# Xt <- file.path(file.path(dir,'1_data_prep',model),paste0(airport,"_",rate,'_',model_num,'_Xt.Rds'))
# Xt <- readRDS(file = Xt)
# 
# Y <- file.path(file.path(dir,'1_data_prep',model),paste0(airport,"_",rate,'_',model_num,'_Y.Rds'))
# Y <- readRDS(file = Y)
# Yt <- file.path(file.path(dir,'1_data_prep',model),paste0(airport,"_",rate,'_',model_num,'_Yt.Rds'))
# Yt <- readRDS(file = Yt)
# 
# DT <- file.path(file.path(dir,'1_data_prep',model),paste0(airport,"_",rate,'_',model_num,'_DT.Rds'))
# DT <- readRDS(file = DT)
# DTt <- file.path(file.path(dir,'1_data_prep',model),paste0(airport,"_",rate,'_',model_num,'_DTt.Rds'))
# DTt <- readRDS(file = DTt)
# 
# rates <- names(Y)[!names(Y) %in% c(response,"rate_lag","rate_dt","rate_dt_pct","rate_change")]
# print(rates)

################################################

dataset_dir <- 'aspm_data/processed'
aspm_files <- list.files(file.path(getwd(), dataset_dir), full.names = T)
aspm_files <- aspm_files[grepl(airport, aspm_files) == T]
aspm_data <- c()
for(file in aspm_files){
  load(file)
  aspm_data <- rbind(aspm_data, dat)
  rm(dat, file)
}

rate_ts <- aspm_data[1:1000,response]
names(rate_ts) <- "rate"
rate_ts <- rate_ts[which(rate_ts$rate >= 30 & rate_ts$rate <= 40),]
rate_ts <- ts(rate_ts$rate)
rate_ts <- rate_ts - min(rate_ts)
str(rate_ts)

library(nltsa)
library(ggplot2)
library(depmixS4)

par(mar=c(3,3,1,1), mgp=c(1.6,.6,0))
layout(matrix(c(1,2, 1,3), nc=2))
plot(rate_ts, type='h')
acf(rate_ts)
pacf(rate_ts)

ggplot()+
  geom_bar(mapping = aes(x=(rate_ts), y = (..count..)/sum(..count..))) +
  # geom_line(mapping = aes(x= 0:40,y=dpois(0:40,lambda = mean(EQcount))),colour = 'red') +
  ylab('Proportion of Earthquakes')
mean(rate_ts)
var(rate_ts)

# Create and fit the Hidden Markov Model
hmm <- depmix(rate_ts ~ 1, family = poisson(), nstates = 2, data=data.frame(rate_ts=rate_ts))
hmmfit <- fit(hmm, verbose = FALSE)

# Output both the true regimes and the
# posterior probabilities of the regimes
post_probs <- posterior(hmmfit)
layout(1:2)
plot(post_probs$state, type='s', main='True Regimes', xlab='', ylab='Regime')
matplot(post_probs[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
# legend(x='topright', c('Bull','Bear'), fill=1:2, bty='n')

