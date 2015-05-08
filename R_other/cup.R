source("load.R")

parfiles <- searchHIEv("ternhect_underpar")


load("cache/lai_workspace.RData")


makeCUPPAR <- function(){
  
  # CSV files are not actually CSV files, they are TOA5 
  cup <- downloadTOA5("TERNHECT_UNDERPAR", allowedExtensions = c(".dat",".csv"))
  
  cupa <- dplyr::summarize(group_by(cup,DateTime=nearestTimeStep(DateTime,30)),
                    PAR_Den_1_Avg=mean(PAR_Den_Avg.1.),
                    PAR_Den_2_Avg=mean(PAR_Den_Avg.2.),
                    PAR_Den_3_Avg=mean(PAR_Den_Avg.3.)
                    )
  # facepar <- as.data.frame(makeFACEPAR(uploadnew=FALSE))
  
  facemean <- dplyr::summarize(group_by(facepar,DateTime),
                               LI190SB_PAR_Den_Avg = mean(LI190SB_PAR_Den_Avg, na.rm=TRUE),
                               TotalSS = mean(TotalSS),
                               DiffuseSS = mean(DiffuseSS))
                               
  
  cupa <- merge(cupa, facemean, all=FALSE)
  
return(cupa)
}

cupa <- makeCUPPAR()
cupa$Date <- as.Date(cupa$DateTime)

cup_cloudy <- makeCloudy(cupa, Fdiff=0.95, bysensor=TRUE)

cupgap <- summaryBy(Gapfraction1 + Gapfraction2 + Gapfraction3 ~ Date, 
                    data=cup_cloudy, FUN=mean, na.rm=TRUE)

cupgap <- subset(cupgap, Date > as.Date("2014-7-30"))
cupgap$LAI1 <- with(cupgap, -log(Gapfraction1.mean)/0.5)
cupgap$LAI2 <- with(cupgap, -log(Gapfraction2.mean)/0.5)
cupgap$LAI3 <- with(cupgap, -log(Gapfraction3.mean)/0.5)


dfa <- summaryBy(LAI ~ Date, FUN=mean, na.rm=TRUE, data=facegap_cloudy_byring, keep.names=TRUE)
dfa <- subset(dfa, Date > min(cupgap$Date))

smoothplot(Date, LAI, data=dfa, kgam=18, ylim=c(1,2.5))
smoothplot(Date, LAI1, data=cupgap, kgam=18, linecol="red", add=TRUE)           
smoothplot(Date, LAI2, data=cupgap, kgam=18, linecol="blue", add=TRUE)           
smoothplot(Date, LAI3, data=cupgap, kgam=18, linecol="forestgreen", add=TRUE)           
           
           

