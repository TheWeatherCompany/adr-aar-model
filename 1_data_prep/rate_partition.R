################################################################################
## Name: rate_frequency.R
## Description: Calculate frequency of ADR/AAR values for each airport 
##              -- export to spreadsheet and use to select rates for binary classification model(s)
## Date: Feb 6, 2018
## Author: jaf
################################################################################

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(broom))
suppressPackageStartupMessages(library(stringr))

Sys.setenv(TZ="America/New_York")

airports <- c("ATL","LGA","PHL","SEA","SFO")

processed_data_dir <- file.path('/Users/jfinn/Google Drive/ADR AAR Model Build/2017 Model Re-Train/raw_datasets')

################### raw dataset saved in shared Google drive folder - no changes made from csv exports
file <- file.path(processed_data_dir, 'ASPM_raw.Rdata')
load(file)

# clean up location ID variable
hrly_dat$LOCID <- as.character(hrly_dat$LOCID)
hrly_dat$LOCID <- gsub(" ","",hrly_dat$LOCID)

################### data partition
a <- "SFO"
response <- "arr_rate"

## get the local time zone
airport_tz <- suppressWarnings(read_csv(file = "/Users/jfinn/Google Drive/ADR AAR Model Build/2017 Model Re-Train/aspm_data/airport_data.csv"))
airport_tz <- airport_tz[which(airport_tz$iata == a),]
# airport_tz_cat <- as.character(airport_tz$tz_timezone)
airport_tz_adj <- as.numeric(airport_tz$timezone)

## subset most frequent rates
dat <- hrly_dat[which(hrly_dat$LOCID == a),] %>% data.frame()
rate_freq <- dat %>%
  select(LOCID, ARR_RATE) %>%
  gather(RATE, VALUE, -LOCID) %>%
  group_by(LOCID, RATE, VALUE) %>%
  summarise(FREQ = length(VALUE)) %>%
  mutate(
    TOTAL = sum(FREQ),
    PCT = FREQ / TOTAL * 100,
    KEEP = ifelse(PCT >= 1, 1, 0)
  ) %>%
  ungroup() %>%
  arrange(desc(PCT)) %>%
  group_by(RATE) %>%
  head(6)
  # filter(PCT >= 5)
rate_freq <- unique(rate_freq[which(rate_freq$RATE == toupper(response)),])
rate_freq <- rate_freq$VALUE

## convert datetime, given in local time, to UTC (avoid DST issues)
names(dat) <- tolower(names(dat))
dat <- dat[which(dat[,response] %in% c(rate_freq)),]
dat$datetime <- paste0(dat$yyyymm,str_pad(dat$daynum, 2, pad = "0")," ",
                               str_pad(dat$hr_local, 2, pad = "0"))
dat$datetime <- as.POSIXct(strptime(x = dat$datetime,format = "%Y%m%d %H", tz = "UTC"))
dat$datetime_num <- as.numeric(dat$datetime)
## add in adjustment to convert from local time to UTC
dat$datetime_num <- dat$datetime_num + (3600 * airport_tz_adj)
dat$dt <- as.POSIXct(dat$datetime_num,origin = "1970-01-01", tz = "UTC")
dat <- dat[order(dat$dt),]

rate_cluster <- kmeans(dat[,response], 2)
rate_cluster
tidy(rate_cluster)
dat$cluster <- rate_cluster$cluster
ggplot(dat, aes(x = dt, y = arr_rate)) + geom_point(aes(color=factor(cluster)))

clust1 <- dat[which(dat$cluster == 1),]
clust2 <- dat[which(dat$cluster == 2),]

## second iteration -- partition cluster 2
clust_iter2 <- kmeans(clust2[,response], 2)
clust_iter2
tidy(clust_iter2)
clust2$cluster <- clust_iter2$cluster
ggplot(clust2, aes(x = dt, y = arr_rate)) + geom_point(aes(color=factor(cluster)))

## third iteration -- partition cluster 1
clust_iter3 <- kmeans(clust1[,response], 2)
clust_iter3
tidy(clust_iter3)
clust1$cluster <- clust_iter3$cluster
ggplot(clust1, aes(x = dt, y = arr_rate)) + geom_point(aes(color=factor(cluster)))

clust1_a <- clust1[which(clust1$cluster == 1),]
clust1_b <- clust1[which(clust1$cluster == 2),]

## fourth iteration -- partition cluster 1a
clust_iter4 <- kmeans(clust1_a[,response], 2)
clust_iter4
tidy(clust_iter4)
clust1_a$cluster <- clust_iter4$cluster
ggplot(clust1_a, aes(x = dt, y = arr_rate)) + geom_point(aes(color=factor(cluster)))






## only select airports of concern (for now)
# dat <- hrly_dat[which(hrly_dat$LOCID %in% airports),]
# print(unique(dat$LOCID))
# 
## determine frequency of rates as they occur at each airport
# rate_freq <- dat %>%
#   select(LOCID, ARR_RATE, DEP_RATE) %>%
#   gather(RATE, VALUE, -LOCID) %>%
#   group_by(LOCID, RATE, VALUE) %>%
#   summarise(FREQ = length(VALUE)) %>%
#   mutate(
#     TOTAL = sum(FREQ),
#     PCT = FREQ / TOTAL * 100,
#     KEEP = ifelse(PCT >= 1, 1, 0)
#   ) %>%
#   ungroup()
# 
# ## export frequency plots
# for(a in airports){
#   p1 <- rate_freq %>%
#     filter(LOCID == a & PCT > 1) %>%
#     ggplot(aes(x = factor(VALUE), y = PCT)) +
#       geom_bar(position = "dodge", stat = "identity") +
#       facet_grid(~ RATE, scales = "free") +
#       theme_bw()
#   plot_file <- file.path(getwd(), "rate_plots", paste0(a, ".png"))
#   dev.copy(png, plot_file)
#   plot(p1)
#   dev.off()
#   rm(p1)
# }
# 
# ## export csv -- to read in later
# names(rate_freq) <- tolower(names(rate_freq))
# rate_freq$rate <- tolower(rate_freq$rate)
# write_csv(x = rate_freq, path = file.path(getwd(), "rate_frequency.csv"))
