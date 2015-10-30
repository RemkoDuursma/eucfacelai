
source("R/utils.R")

Library(dplyr)    # summarize, group_by
Library(doBy)     # summaryBy
Library(gplots)   # plotCI
Library(mgcv)     # gam
Library(stringr)  # str_trim
Library(Hmisc)    # approxExtrap
Library(lubridate)
Library(rmarkdown) # render

Library(broom)    # tidy; glance
Library(lme4)     # lmer
Library(lmerTest) # anova.merMod
Library(car)      # Anova
Library(reporttools)  # formatPval

# set path for HIEv
if(!dir.exists("cache"))dir.create("cache")

if(!dir.exists("output"))dir.create("output")
if(!dir.exists("output/figures"))dir.create("output/figures")

# Load all functions
source("R/data_processing.R")
source("R/data_define.R")
source("R/functions-figures.R")
source("R/LAI-functions.R")

