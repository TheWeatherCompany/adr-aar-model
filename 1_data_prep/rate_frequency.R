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
dat <- hrly_dat[which(hrly_dat$LOCID %in% airports),]
print(unique(dat$LOCID))

## determine frequency of rates as they occur at each airport
rate_freq <- dat %>%
  select(LOCID, ARR_RATE, DEP_RATE) %>%
  gather(RATE, VALUE, -LOCID) %>%
  group_by(LOCID, RATE, VALUE) %>%
  summarise(FREQ = length(VALUE)) %>%
  mutate(
    TOTAL = sum(FREQ),
    PCT = FREQ / TOTAL * 100,
    KEEP = ifelse(PCT >= 1, 1, 0)
  ) %>%
  ungroup()

## export frequency plots
for(a in airports){
  p1 <- rate_freq %>%
    filter(LOCID == a & PCT > 1) %>%
    ggplot(aes(x = factor(VALUE), y = PCT)) +
      geom_bar(position = "dodge", stat = "identity") +
      facet_grid(~ RATE, scales = "free") +
      theme_bw()
  plot_file <- file.path(getwd(), "rate_plots", paste0(a, ".png"))
  dev.copy(png, plot_file)
  plot(p1)
  dev.off()
  rm(p1)
}

## export csv -- to read in later
names(rate_freq) <- tolower(names(rate_freq))
rate_freq$rate <- tolower(rate_freq$rate)
write_csv(x = rate_freq, path = file.path(getwd(), "rate_frequency.csv"))
