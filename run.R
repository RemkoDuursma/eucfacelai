

# Load libraries, functions, etc.
source("load.R")


# 30minutely radiation data
facepar <- makeFACEPAR(uploadnew=FALSE)

# Rainfall data, averaged across 3 rain gauges.
facerain <- get_rain("rawmean")
faceraindaily <- get_rain("daily")
facepar <- merge(facepar, facerain, by=c("DateTime"))


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
                           minSolarElevation =10)


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
litter <- make_litter(SLA=43)
litter_byCO2 <- agglitter(litter)

# Find calibration constant, from 2013 drought.
calib <- calibrateToDrought(facegap_cloudy_byring)$calib

# Calculate LAI
facegap_cloudy_byring <- calculate_LAI(facegap_cloudy_byring, calib=calib)
facegap_all_byring <- calculate_LAI(facegap_all_byring, calib=calib)


# TEMPORARY - WILL BE MOVED TO FUNCTION!
lai <- summaryBy(LAI ~ Ring, data=facegap_cloudy_byring, FUN=mean, na.rm=T)
ba <- eucfaceBA()
ba <- merge(ba,lai)
ba <- merge(ba, eucface())


# Litter
lit <- subset(litter, !is.na(ndays))
lagg <- summaryBy(Leaf.mean + dLAIlitter.mean + ndays ~ Ring, FUN=sum, keep.names=TRUE,
                  data=lit)
trapArea <- 0.1979
# g m-2 year-1
lagg$Litter_annual <- (lagg$Leaf.mean / trapArea) * 365.25 / lagg$ndays
lagg$LAIlitter_annual <- lagg$dLAIlitter.mean * 365.25 / lagg$ndays 
ba <- merge(ba, lagg[,c("Ring","LAIlitter_annual")])

lmBALAI <- lm(LAI.mean ~ BA, data=ba)
ba$LAIfromBA <- predict(lmBALAI, ba)

# Leaf lifespan
ba$LL <- with(ba, LAI.mean / LAIlitter_annual)

facegap_cloudy_byring <- merge(facegap_cloudy_byring, ba[,c("Ring","LAIfromBA")])
facegap_cloudy_byring$LAIanomaly <- with(facegap_cloudy_byring, LAI - LAIfromBA)


# Aggregate by CO2 treatment
facegap_cloudy_byCO2 <- aggfacegapbyCO2(facegap_cloudy_byring)
facegap_all_byCO2 <- aggfacegapbyCO2(facegap_all_byring)


# Dataset with dLAI from litter and PAR during 2013 drought (used in above calibration).
face_dLAIdrought2013 <- make_dLAI_drought2013(facegap_cloudy_byring,calib=calib)

# Soil water
facesoilwater <- get_soilwater()

# Canopy photos.
flatcan <- get_flatcan()
flatcan_byring <- agg_flatcan(flatcan, by="Ring")
flatcan_byring <- add_PARLAI_to_flatcan(facegap_cloudy_byring,flatcan_byring)

flatcan_byCO2 <- agg_flatcan(flatcan, by="CO2")

# Dataset with litter fall comparison to changes in LAI during same period.
dLAIlitter <- make_dLAI_litter(facegap_cloudy_byring, litter, kgam=20)

# Air temperature from ROS
airt <- get_rosTair()

# ramp-up CO2 concentration
ramp <- get_ramp()

# Figures
source("make_figures.R")

# save cache.
save.image(file="cache/lai_workspace.RData")

# Messages.
m1 <- paste0("Most recent day with usable cloudy data: ",max(facegap_cloudy_byring$Date))
message(m1)










