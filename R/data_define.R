
# EucFACE treatment key.
eucface <- function(){
  data.frame(Ring=paste0("R",1:6), 
             treatment=c("elevated","ambient","ambient","elevated","elevated","ambient"))
}

# Manually selected cloudy periods prior to 2012-12-6, when the Diffuse meter came online
# (at least, that's where HIEv starts - could find more data but really not necessary).
cloudydays <- function(){
  read.csv(text="
    Date, starthour, endhour
    2012-10-26,6,12
    2012-11-03,0,24
    2012-11-07,0,24
    2012-11-10,12,18
    2012-11-14,0,24
    2012-11-16,0,24
    2012-11-22,0,24
    2012-11-28,0,24", colClasses=c("Date","numeric","numeric"))
}

# Bad data
badPARdata <- function(){
        read.csv(text="startDate, endDate, Ring, Sensor
                    2014-10-13, 2014-10-28, R5, PAR_Den_1_Avg
                    2013-09-08, 2013-09-08, R4, PAR_Den_2_Avg",
                       colClasses=c("Date","Date","character","character"),
                       strip.white=TRUE)
}

eucfaceBA <- function(){
  read.table(text="
  Ring  nstems	BA
  R1	30	25.2
  R2	41	24.3
  R3	39	25.9
  R4	55	20.9
  R5	54	38
  R6	44	29", header=TRUE)
  
}


flushingdates <- function(){
  
  # David Ellsworth : "
#   -  R6 16th Dec. 2014; other rings 5th Jan. 2015 (no observations between 16th and 30th, 
#                                                    but just starting around the 30th)
#   
#   - R3 & R5 4th Dec. 2013; R6 & R4 16th Dec. 2013; R1 & R2 24th Dec. 2013
#   
#   - R1 16th Oct. 2012 (R2 not observed L)
#   
#  - All rings but not all trees 22nd January 2013 (this is the minor leaf flushing event you refer to below
#   - R3-R6 8th Oct. 2012
#   
#   - All rings but not all trees 17th Feb. 2012"
  
  # Averages of dates.
  dates <- as.Date(c("2015-1-5", "2013-12-15", "2012-10-12","2012-2-17","2013-1-22"))
  
  
return(dates)
}

