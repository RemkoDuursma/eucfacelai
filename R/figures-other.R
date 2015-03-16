
# Plot raw data (not for manuscript)
plotone <- function(date, ring, df=facepar_cloudy, xlim=c(4,20), addlegend=TRUE){
  
  date <- as.Date(date)
  palette(c("blue","red", "forestgreen", "black", "darkgrey","darkorchid3"))
  df <- subset(df, Date == date & Ring == ring)
  if(nrow(df) == 0)return()
  
  addDat <- function(x, i, pch=19, ...){
    points(df$hourtime, df[,x], col=palette()[i], pch=pch, type='p', ...)
  }
  
  par(mar=c(5,5,2,5))
  plot(1, xlim=xlim, ylim=c(0,max(df$LI190SB_PAR_Den_Avg)),
       xlab="Hour", ylab=expression(PAR~~(mu*mol~m^-2~s^-1)), 
       main=paste(ring, as.character(date), sep=" -- "))
  
  addDat("PAR_Den_1_Avg",1)
  addDat("PAR_Den_2_Avg",2)
  addDat("PAR_Den_3_Avg",3)
  addDat("LI190SB_PAR_Den_Avg",pch=21,4,bg="orange")
  
  par(new=TRUE)
  plot(1, xlim=xlim, ylim=c(0,1), axes=FALSE, ann=FALSE)
  points(df$hourtime, df$Fdiff, type='l', lwd=2, col=palette()[5])
  points(df$hourtime, df$Gapfraction, type='p',cex=1.6, pch=15, col=palette()[6])
  axis(4)    
  mtext(side=4, line=3, text="Fdiff or Gapfraction")
  
  p <- palette()
  p[4] <- "orange"
  if(addlegend)legend("topleft", c("PAR below 1","PAR below 2","PAR below 3", "PAR above", "Fdiffuse","Gapfraction"),
                      fill=p, cex=0.6)
}

plotsix <- function(date,...){
  
  par(mfrow=c(2,3))
  rings <- paste0("R",1:6)
  for(i in 1:6)plotone(date=date, ring=rings[i], addlegend= i==1)
  
}


figure_FACEPAR_allcloudydata <- function(minnrHH, dfr=facepar_cloudy,
                                         filename="output/figures_other/facepar_allcloudydata.pdf"){
  hhtable <- table(dfr$Date)
  Dates <- as.Date(names(hhtable))[hhtable > 6*minnrHH]
  on.exit(dev.off())
  pdf(filename, width=12, height=7)
  for(i in 1:length(Dates))plotsix(Dates[i])
  
}


# Plot raw PAR data - not for manuscript.
plotFACEPAR <- function(Ring, Date, df=facepar, setmargins=TRUE){
  
  if(setmargins)par(mar=c(5,5,2,5))
  
  main <- paste(Ring, "--", as.character(Date))
  df <- df[df$Date == as.Date(Date) & df$Ring == Ring,]
  df$Fdiff <- with(df, DiffuseSS / TotalSS)
  df$hourtime <- with(df, hour(DateTime) + minute(DateTime)/60)
  
  with(df, plot(hourtime, LI190SB_PAR_Den_Avg, type='l', col="darkorange",
                main=main,xlim=c(0,24),axes=FALSE,
                xlab="Time", ylab=expression(PPFD~~(mu*mol~m^-2~s^-1))))
  axis(1, at=seq(0,24,by=4))
  axis(2)
  box()
  
  with(df, {
    points(hourtime, PAR_Den_1_Avg, col="blue", type='l')
    points(hourtime, PAR_Den_2_Avg, col="red", type='l')
    points(hourtime, PAR_Den_3_Avg, col="forestgreen", type='l')
  })
  
  par(new=TRUE)
  with(df, plot(hourtime, Fdiff, 
                type='l', col="darkgrey",
                axes=FALSE, ann=FALSE, ylim=c(0,1)))
  axis(4)
  axisTitle(side=4, text="Fraction diffuse")
  
}

plotsixFACEPAR <- function(Date, setpar=TRUE,...){
  
  if(setpar)par(mfrow=c(2,3), mar=c(5,5,2.5,5))  
  rings <- paste0("R", 1:6)
  
  for(r in rings)plotFACEPAR(r, Date=Date, setmargins=FALSE, ...)
}
