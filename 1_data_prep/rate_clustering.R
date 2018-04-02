################################################################################
## Name: rate_frequency.R
## Description: Calculate frequency of ADR/AAR values for each airport 
##              -- export to spreadsheet and use to select rates for binary classification model(s)
## Date: Feb 6, 2018
## Author: jaf
################################################################################
Sys.setenv(TZ="America/New_York")

airports <- c("ATL","LGA","PHL","SEA","SFO")

suppressPackageStartupMessages(library(tidyverse))

processed_data_dir <- file.path('/Users/jfinn/Google Drive/ADR AAR Model Build/2017 Model Re-Train/raw_datasets')

################### raw dataset saved in shared Google drive folder - no changes made from csv exports
file <- file.path(processed_data_dir, 'ASPM_raw.Rdata')
load(file)

# clean up location ID variable
hrly_dat$LOCID <- as.character(hrly_dat$LOCID)
hrly_dat$LOCID <- gsub(" ","",hrly_dat$LOCID)

################### only select airports of concern (for now)
airport <- 'LGA'
dat <- hrly_dat[which(hrly_dat$LOCID %in% airport),]
print(unique(dat$LOCID))

arr_rate <- 

## determine frequency of rates as they occur at each airport
rate_freq <- dat %>%
  select(LOCID, ARR_RATE, DEP_RATE) %>%
  gather(RATE, VALUE, -LOCID) %>%
  group_by(LOCID, RATE, VALUE) %>%
  summarise(FREQ = length(VALUE)) %>%
  mutate(
    TOTAL = sum(FREQ),
    PCT = FREQ / TOTAL * 100,
    KEEP = ifelse(PCT >= 10, 1, 0)
  ) %>%
  ungroup()

## export frequency plots
for(a in airports){
  p1 <- rate_freq %>%
    filter(LOCID == a 
           # & PCT > 1
           ) %>%
    ggplot(aes(x = factor(VALUE), y = PCT)) +
      geom_bar(position = "dodge", stat = "identity") +
      facet_grid(~ RATE, scales = "free") +
      theme_bw()
  plot_file <- file.path(getwd(), "rate_plots", paste0(a, ".png"))
  dev.copy(png, plot_file, width = 800, height = 600)
  plot(p1)
  dev.off()
  rm(p1)
}

## export csv -- to read in later
names(rate_freq) <- tolower(names(rate_freq))
rate_freq$rate <- tolower(rate_freq$rate)
write_csv(x = rate_freq, path = file.path(getwd(), "rate_frequency.csv"))





suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(AnomalyDetection))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(xgboost))
suppressPackageStartupMessages(library(factoextra))
suppressPackageStartupMessages(library(FactoMineR))
suppressPackageStartupMessages(library(corrplot))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(flexclust))
suppressPackageStartupMessages(library(foreach))
suppressPackageStartupMessages(library(doParallel))
suppressPackageStartupMessages(library(cluster))


Sys.setenv(TZ="America/New_York")
user <- Sys.getenv("LOGNAME")
response <- "arr_rate"
seed <- 12988

source("dataset_import.R")
source("custom_functions.R")

input_dir <- "/Users/jfinn/Google Drive/ADR AAR Model Build/2017 Model Re-Train/aspm_data/processed"
output_dir <- "/Users/jfinn/Google Drive/ADR AAR Model Build/2017 Model Re-Train/anomaly_detection/input"

data_files <- list.files(input_dir, pattern = ".Rdata", full.names = T)
target <- c()
for(file in data_files){
  load(file)
  ad_dat <- dat[,c("dt","locid","arr_rate","dep_rate")]
  ad_dat$dt <- as.POSIXct(strptime(ad_dat$dt, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  # names(ad_dat) <- c("Date","Location","arr_rate","dep_rate")
  target <- rbind(target, ad_dat)
  rm(ad_dat, dat)
}

# output_file <- file.path(output_dir, "target.csv")
# write_csv(x = target, path = output_file)

airports <- unique(target$locid)
airport <- "ATL"

data <- target[which(target$locid == airport & target$dt >= "2014-11-01"),c("dt","arr_rate")]
res <- AnomalyDetectionTs(data
                          , piecewise_median_period_weeks = 3
                          , alpha = 0.5
                          , max_anoms = 0.15
                          , direction = 'neg'
                          , plot = TRUE)
res$plot
anomalies <- res$anoms
anomalies$dt <- as.POSIXct(anomalies$timestamp)
anomalies$timestamp <- NULL

data <- left_join(data, anomalies, by = "dt")
data$anoms <- ifelse(is.na(data$anoms), 0, 1)

# notam_data <- import_notam(user, airport)
# metar_data <- import_metar(user, airport)
# aspm_data <- import_aspm(user, airport)

# metar_sub <- metar_data[,c("dt","clds_pct","wdir_cat","ceilingflightrule","visflightrule","flightrule","wspd",
#                            "vis","ceiling_ln","wx_obscur","wx_precip","wx_convec","hrs_precip","hrs_obscur","hrs_convec",
#                            "hrs_precip_daily","hrs_obscur_daily","hrs_convec_daily","lifr","ifr","mvfr","vfr")]
# aspm_sub <- aspm_data[,c("dt","oag_dep","oag_arr","etms_dep","etms_arr","dep_demand","arr_demand",
#                          "dep_rate","arr_rate","arr_dm_et_ratio","dep_dm_et_ratio","rwycnfg_ATL_26R","rwycnfg_ATL_8L",
#                          "rwycnfg_ATL_26L","rwycnfg_ATL_8R","rwycnfg_ATL_9R","rwycnfg_ATL_27L","rwycnfg_ATL_10",
#                          "rwycnfg_ATL_28","rwycnfg_ATL_27R","rwycnfg_ATL_9L")]
# aspm_sub[,response] <- NULL
# feature_data <- left_join(x = metar_sub, y = aspm_sub, by = "dt")
# feature_data$dt <- as.POSIXct(strptime(feature_data$dt, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
# 
# data_explore <- left_join(x = data, y = feature_data, by = "dt")
# data_explore <- na.omit(data_explore)
# 
# ## create date / time variables
# data_explore$time <- as.numeric(format(data_explore$dt,"%Y"))*8760 + 
#   as.numeric(strftime(data_explore$dt, format = "%j"))*24 +
#   as.numeric(format(data_explore$dt,"%H"))
# 
# data_explore$hr_local <- factor(format(data_explore$dt,"%H"))
# data_explore$weekday <- factor(format(data_explore$dt,"%a"))
# data_explore$month <- factor(format(data_explore$dt,"%m"))
# 
# data_explore <- data_explore[order(as.vector(data_explore$dt)),]
# rownames(data_explore) <- 1:nrow(data_explore)

################### cluster response
response <- target[which(target$locid == airport),c("arr_rate","dep_rate")]
response <- unique(response)

k2 <- kmeans(response, centers = 3, nstart = 10)
str(k2)
k2
fviz_cluster(k2, data = response)

set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(response, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


fviz_nbclust(response, kmeans, method = "wss")
fviz_nbclust(response, kmeans, method = "silhouette")

set.seed(123)
gap_stat <- clusGap(response, FUN = kmeans, nstart = 25,K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

response <- response %>%
  mutate(Cluster = k2$cluster)
response %>%
  group_by(Cluster) %>%
  summarise_all("mean")



library("NbClust")
nb <- NbClust(response, distance = "euclidean", min.nc = 3, max.nc = 10, method = "kmeans")
fviz_nbclust(nb)

