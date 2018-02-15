################################################################################
## Name: metar_processing.R
## Description: Download & process raw metar data
## Date: Jan 19, 2018
## Author: jaf
################################################################################

Sys.setenv(TZ="America/New_York")

source('metar_functions.R')

output_dir <- "/Users/jfinn/Google Drive/ADR AAR Model Build/2017 Model Re-Train/metar_data" #download location
start_date <- ISOdate(2014,1,1) #start date in year, month, day format
end_date <- ISOdate(2017,7,1) #end date in year, month, day format
input_network <- c("ASOS")
input_state <- c("NY")
input_faaid <- c("SFO")

## download METAR data
# metar_download_mesonet(start_date, end_date, input_network, input_state, input_faaid, output_dir)

## define raw METAR file name
datestring1 <- format(start_date, "%Y%m%d")
datestring2 <- format(end_date, "%Y%m%d")
raw_datafile <- str_c(input_faaid, "_", datestring1, "_to_", datestring2, ".txt")
raw_datafile <- file.path(output_dir, "raw", raw_datafile)

## re-read in METAR data (if already downloaded)
data <- read.csv(file = raw_datafile, stringsAsFactors = F)
data <- unique(data)

## process the raw METAR data
data_proc <- metar_process(data)

## export raw data to .Rdata file
proc_datafile <- str_c(input_faaid, "_", datestring1, "_to_", datestring2, ".Rdata")
proc_datafile <- file.path(output_dir, "processed", proc_datafile)
save(data_proc, file = proc_datafile)
