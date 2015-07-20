source("load.R")
source("R/data_processing.R")
source("R/functions-figures.R")

parfiles <- searchHIEv("ternhect_underpar")


makeCUPPAR <- function(facepar){
  
  # CSV files are not actually CSV files, they are TOA5 
  cup <- downloadTOA5("TERNHECT_UNDERPAR", allowedExtensions = ".dat")
  
  facea <- as.data.frame(dplyr::summarize(group_by(facepar, DateTime),
                            LI190SB_PAR_Den_Avg = mean(LI190SB_PAR_Den_Avg),
                            TotalSS = mean(TotalSS),
                            DiffuseSS = mean(DiffuseSS)))
  cup <- merge(cup, facea, all=FALSE)
  
  cupa <- as.data.frame(dplyr::summarize(group_by(cup,DateTime=nearestTimeStep(DateTime,30)),
                    PAR_Den_1_Avg=mean(PAR_Den_Avg.1.),
                    PAR_Den_2_Avg=mean(PAR_Den_Avg.2.),
                    PAR_Den_3_Avg=mean(PAR_Den_Avg.3.),
                    LI190SB_PAR_Den_Avg = mean(LI190SB_PAR_Den_Avg),
                    TotalSS = mean(TotalSS),
                    DiffuseSS = mean(DiffuseSS)))
  
return(cupa)
}


facepar <- downloadCSV("FACE_P0037_RA_PARAGG")

facegap <- downloadCSV("FACE_P0037_RA_GAPFRACLAI_20121026-20150630_L2.csv")

cupa <- makeCUPPAR(facepar)
cupa$Date <- as.Date(cupa$DateTime)

cup_cloudy <- makeCloudy(cupa, Fdiff=0.98, bysensor=TRUE, addManualCloudyDays=FALSE)

n <- function(x,...)length(x[!is.na(x)])
cupgap <- summaryBy(Gapfraction1 + Gapfraction2 + Gapfraction3 ~ Date, 
                    data=cup_cloudy, FUN=c(mean,n), na.rm=TRUE)
cupgap <- subset(cupgap, Gapfraction1.n > 3)
cupgap <- subset(cupgap, Gapfraction1.mean > 0.05)
cupgap$Gapfraction_CUP <- apply(cupgap[,grep("Gapfraction[1-3]{1}.mean", names(cupgap))],1,mean)

# Add mean FACE gap fraction for comparison
facemean <- as.data.frame(dplyr::summarize(group_by(facegap,Date),
                                           Gapfraction_FACE = mean(Gapfraction.mean, na.rm=TRUE)))
facemean$Date <- as.Date(facemean$Date)

cupgap <- merge(cupgap, facemean, all=FALSE)


cup_face_gapfr <- function(cupgap){
  smoothplot(Date, Gapfraction_FACE, data=cupgap, kgam=18, ylim=c(0,0.6), axes=FALSE,
             xlab="", ylab="Gapfraction (-)")
  smoothplot(Date, Gapfraction_CUP, data=cupgap, kgam=18, linecol="red", pointcol="red", add=TRUE)           
  timeseries_axis()
  axis(2)
  box()
  legend("bottomright", c("EucFACE","CUP"), col=c("black","red"), pch=19, lwd=2, bty='n', cex=0.9)
}
to.pdf(cup_face_gapfr(cupgap), "output/figures_other/CUP_FACE_gapfraction_means.pdf", width=8, height=6)
