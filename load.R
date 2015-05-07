
# library(plyr)
library(dplyr)
library(HIEv)
library(canopyphoto)
library(plotrix)
library(RColorBrewer)
library(doBy)
library(plantecophys)
library(plotBy)
library(scales)
library(Hmisc)
library(gplots)
library(mgcv)
library(stringr)

library(rmarkdown)


if(packageVersion('HIEv') < '0.6.4')
  stop("Please update the HIEv package!")

# set path for HIEv
if(!file.exists("cache"))dir.create("cache")
setToPath("cache")

# Load all functions
source("R/data_processing.R")
source("R/data_define.R")
source("R/functions-figures.R")
source("R/LAI-functions.R")

