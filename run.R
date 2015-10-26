

# Load libraries, functions, etc.
source("load.R")


# All figures and analyses use data up to this point.
.maxdate <- as.Date("2015-3-1")

# 30minutely radiation data
facepar <- downloadCSV("FACE_RA_P0037_PARAGG_20121016-20150301_L2.csv")
facepar$Date <- as.Date(facepar$Date)

# Rainfall data, averaged across 3 rain gauges.
facerain <- get_rain("rawmean")
faceraindaily <- get_rain("daily")


# Select 'cloudy' data, do some trimming.
# Can set threshold for diffuse fraction here
facepar_cloudy <- makeCloudy(facepar,
                              Fdiff_cutoff= 0.98,
                              PARabovecutoff = 250,
                              PARbelowcutoff = 1300,
                              minSolarElevation = 10)

# All data, including sunny.
facepar_all <- makeCloudy(facepar,
                           Fdiff_cutoff= 0.0,
                           PARabovecutoff = 250,
                           PARbelowcutoff = 3000,
                           minSolarElevation = 10)


# Aggregate into daily gapfraction
facegap_cloudy_byring <- aggFACEPARbyring(facepar_cloudy)
facegap_all_byring <- aggFACEPARbyring(facepar_all)


# Further trimming:
# 1. Minimum nr of timesteps per day 
# 2. Discard days where >1 ring has within-day SD above maxSD
facegap_cloudy_byring <- subsetFACEPARbyring(facegap_cloudy_byring,
                                      minnrHH=4,  
                                      maxSD=0.03)

# Litter fall.
litring <- downloadCSV("FACE_RA_P0037_LEAFLITTER_20121009-20140814_L2.csv")
litring$Date <- as.Date(litring$Date)
litter <- litterbyring(litring)
litter_byCO2 <- agglitter(litter)


# Find calibration constant, from 2013 drought.
calib <- calibrateToDrought(facegap_cloudy_byring)$calib

# Calculate LAI
facegap_cloudy_byring <- calculate_LAI(facegap_cloudy_byring, calib=calib)
facegap_all_byring <- calculate_LAI(facegap_all_byring, calib=calib)



# Averages across rings (LAI, LL, litter, BA)
ba <- make_ba(facegap_cloudy_byring, litter)

# LAI anomaly - difference between LAI and prediction from BA.
facegap_cloudy_byring <- merge(facegap_cloudy_byring, ba[,c("Ring","LAIfromBA")])
facegap_cloudy_byring$LAIanomaly <- with(facegap_cloudy_byring, LAI - LAIfromBA)


# Aggregate by CO2 treatment
facegap_cloudy_byCO2 <- aggfacegapbyCO2(facegap_cloudy_byring)
facegap_all_byCO2 <- aggfacegapbyCO2(facegap_all_byring)


# Dataset with dLAI from litter and PAR during 2013 drought (used in above calibration).
face_dLAIdrought2013 <- make_dLAI_drought2013(facegap_cloudy_byring,calib=calib)

# # Soil water
# facesoilwater <- get_soilwater()
# 
# # Air temperature from ROS
# airt <- get_rosTair()
# 
# simplemet <- merge(airt, facesoilwater, all=TRUE)

simplemet <- downloadCSV("FACE_RA_P0037_DAILYMET_20110619-20151026_L2.csv")


# Canopy photos.
flatcan <- get_flatcan()
flatcan_byring <- agg_flatcan(flatcan, by="Ring")
flatcan_byring <- add_PARLAI_to_flatcan(facegap_cloudy_byring,flatcan_byring)

flatcan_byCO2 <- agg_flatcan(flatcan, by="CO2")

# find optimal k for flat canopy photos
Ok <- function(k){
  flatcan_byring <- agg_flatcan(flatcan, by="Ring", k)
  flatcan_byring <- add_PARLAI_to_flatcan(facegap_cloudy_byring,flatcan_byring)
  with(flatcan_byring, sum((LAI - LAI.PAR.mean)^2))
}
kopt <- optimize(Ok, c(0.2, 0.8))$minimum

# Dataset with litter fall comparison to changes in LAI during same period.
dLAIlitter <- make_dLAI_litter(facegap_cloudy_byring, litter, kgam=20)

# ramp-up CO2 concentration
ramp <- get_ramp()

# Figures
source("make_figures.R")

# save cache.
save.image(file="cache/lai_workspace.RData")
# load("cache/lai_workspace.RData")

# save for later (ms resets it)
olddigit <- options()$digits

# Make manuscript
render("manuscript.Rmd", "word_document", "manuscript.docx")
render("manuscript_supportinginfo.Rmd", "word_document", "manuscript_SuppInfo.docx")

options(digits=olddigit)






