library(knitr)
library(markdown)

###################

args <- commandArgs(trailingOnly = TRUE)
user <- args[1]
airport <- args[2]
response <- args[3]
model <- args[4]
test <- args[5]
horizons <- args[6:length(args)]

# user <- 'jfinn'
# airport <- 'ATL'
# response <- 'DEP_RATE'
# model <- 'M11'
# horizons <- c('H1','H3','H6','H12')
# test <- "TEST_BINARY"

###################

rate <- ifelse(response == "DEP_RATE","ADR","AAR")
dir <- getwd()

################### test -- binary (rates only)
filename <- paste0(airport,"_",rate,"_",model,"_",test)
file.rmd <- paste0(test,".Rmd")
filename <- paste0(airport,"_",rate,"_",model,"_",test)
file.md <- paste0(dir,"/",filename,".md")
file.html <- paste0(dir,"/",filename,".html")

## create markdown file
knit(file.rmd, file.md)
## convert to html
markdownToHTML(file.md, file.html, title = paste0(airport,"_",rate,"_",model))

## remove files in "figure" folder
fig.dir <- paste0(dir,"/figure/*")
unlink(fig.dir, recursive = FALSE, force = FALSE)

## remove markdown file
unlink(file.md, recursive = FALSE, force = FALSE)

## remove xgboost model
xgb <- paste0(dir,"/xgboost.model")
unlink(xgb, recursive = FALSE, force = FALSE)

