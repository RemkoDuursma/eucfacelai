export_CSV <- function(){
  
  if(!file.exists("output/data"))dir.create("output/data", recursive=TRUE)
  
  dfr <- facegap_cloudy_byring
  dfr$maxSDring <- NULL
  
  d1 <- format(min(dfr$Date), "%Y%m%d")
  d2 <- format(max(dfr$Date), "%Y%m%d")
  filename <- paste0("FACE_P0037_RA_GAPFRACLAI_",d1,"-",d2,"_L2.csv")
  filename <- file.path("output/data",filename)
  
  write.csv(dfr, filename, row.names=FALSE)
  meta <- 
  "Estimates of canopy gap fraction and leaf area index for the EucFACE based on
diffuse transmittance data.

Transmittance is estimated as PAR below / PAR above canopy, for 30min periods where
the diffuse fraction > 0.98 (as estimated from the CUP Eddy flux tower site).

Estimates are daily averages by Ring. The number of observations varies between days, 
depending on the number of 30min cloudy periods.

Data owner : Remko Duursma (r.duursma@uws.edu.au)

Variables:
   Ring - One of six EucFACE rings (R1, R2, ..., R6)
   Date - YYYY-MM-DD
   Gapfraction.mean - Daily mean gapfraction (0-1)
   Gapfraction.sd - within-day standard deviation of gapfraction
   Gapfraction.length - Number of observations within a day
   treatment - Either 'elevated' or 'ambient'
   LAI - Inferred leaf area index (m2 m-2) using algorithm of Campbell&Norman 2000, based on the
   attenuation of diffuse radiation in a homogenous canopy."
  
  writeLines(meta, "output/data/FACE_P0037_RA_GAPFRACLAI_meta.txt")

#   HIEv:::uploadToHIEv(filename,
#                       experiment_id=15,
#                       type='PROCESSED',
#                       description=meta)
#   
}

