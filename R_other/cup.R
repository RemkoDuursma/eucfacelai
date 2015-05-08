source("load.R")

parfiles <- searchHIEv("ternhect_underpar")


makeCUPPAR <- function(){
  
  # CSV files are not actually CSV files, they are TOA5 
  cup <- downloadTOA5("TERNHECT_UNDERPAR", allowedExtensions = c(".dat",".csv"))
  
  cupa <- dplyr::summarize(group_by(cup,DateTime=nearestTimeStep(DateTime,30)),
                    PAR_Den_1_Avg=mean(PAR_Den_Avg.1.),
                    PAR_Den_2_Avg=mean(PAR_Den_Avg.2.),
                    PAR_Den_3_Avg=mean(PAR_Den_Avg.3.)
                    )
  facepar <- as.data.frame(makeFACEPAR(uploadnew=FALSE))
  
  facemean <- dplyr::summarize(group_by(facepar,DateTime),
                               LI190SB_PAR_Den_Avg = mean(LI190SB_PAR_Den_Avg, na.rm=TRUE),
                               TotalSS = mean(TotalSS),
                               DiffuseSS = mean(DiffuseSS))
                               
  
  cupa <- merge(cupa, facemean, all=FALSE)
  
return(cupa)
}

cupa <- makeCUPPAR()
cupa$Date <- as.Date(cupa$DateTime)

cup_cloudy <- makeCloudy(cupa, Fdiff=0.95

cupgap <- summaryBy(Gapfraction ~ Date, data=cup_cloudy, FUN=mean, na.rm=TRUE)

cupgap <- subset(cupgap, Date > as.Date("2014-7-20"))

