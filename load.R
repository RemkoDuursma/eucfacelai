

library(dplyr)    # summarize, group_by
library(HIEv)
library(doBy)     # summaryBy
library(gplots)   # plotCI
library(mgcv)     # gam
library(stringr)  # str_trim
library(Hmisc)    # approxExtrap

library(rmarkdown) # render


if(packageVersion('HIEv') < '0.7')
  stop("Please update the HIEv package!")

# set path for HIEv
if(!file.exists("cache"))dir.create("cache")
setToPath("cache")

# Load all functions
source("R/data_processing.R")
source("R/data_define.R")
source("R/functions-figures.R")
source("R/LAI-functions.R")

