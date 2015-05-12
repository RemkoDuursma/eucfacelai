
source("R/utils.R")

Library(dplyr)    # summarize, group_by
Library(HIEv)
Library(doBy)     # summaryBy
Library(gplots)   # plotCI
Library(mgcv)     # gam
Library(stringr)  # str_trim
Library(Hmisc)    # approxExtrap

Library(rmarkdown) # render

Library(broom)    # tidy; glance
Library(lme4)     # lmer
Library(lmerTest) # anova.merMod
Library(car)      # Anova
Library(reporttools)  # formatPval


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

