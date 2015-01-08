
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

