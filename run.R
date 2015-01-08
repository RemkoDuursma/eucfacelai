

# Load libraries, functions, etc.
source("load.R")


# 30minutely radiation data
facepar <- makeFACEPAR(uploadnew=FALSE)

# Rainfall data, averaged across 3 rain gauges.
facerain <- get_rain("rawmean")
facepar <- merge(facepar, facerain, by=c("DateTime"))


# Select 'cloudy' data, do some trimming.
# Can set threshold for diffuse fraction here
facepar_cloudy <- makeCloudy(facepar,
                              Fdiff_cutoff= 0.98,
                              PARabovecutoff = 250,
                              PARbelowcutoff = 1300,
                              minSolarElevation =10)

# All data, including sunny.
facepar_all <- makeCloudy(facepar,
                           Fdiff_cutoff= 0.0,
                           PARabovecutoff = 250,
                           PARbelowcutoff = 3000,
                           minSolarElevation =10)


# Aggregate into daily gapfraction, calculate LAI based on daily gapfraction.
facegap_cloudy_byring <- aggFACEPARbyring(facepar_cloudy)
facegap_all_byring <- aggFACEPARbyring(facepar_all)

# Further trimming:
# 1. Minimum nr of timesteps per day 
# 2. Discard days where >1 ring has within-day SD above maxSD
facegap_cloudy_byring <- subsetFACEPARbyring(facegap_cloudy_byring,
                                      minnrHH=4,  
                                      maxSD=0.03)

# Aggregate by CO2 treatment
facegap_cloudy_byCO2 <- aggfacegapbyCO2(facegap_cloudy_byring)

# Soil water
facesoilwater <- get_soilwater()

# Litter fall.
litter <- make_litter(SLA=43)
litter_byCO2 <- agglitter(litter)

# Figures
source("make_figures.R")

# Messages.
m1 <- paste0("Most recent day with usable cloudy data: ",max(facegap_cloudy_byring$Date))

d <- sort(unique(facegap_cloudy_byring$Date))
m2 <- paste0("On average, we have an observation of LAI every ", 
             round(as.numeric(difftime(max(d), min(d), "days")) / length(d),1), " days")
m3 <- paste0("Longest period without an observation : ", round(max(diff(d)),0), " days")

message(m1, "\n", m2,"\n",m3)

save.image(file="cache/lai_workspace.RData")







