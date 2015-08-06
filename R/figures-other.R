figure3b <- function(df,
                    xlim=NULL, ylim=NULL,
                    legend=TRUE,
                    ylab=expression(italic(L)~(m^2~m^-2)),
                    cex.lab=1.1, cex.axis=0.8, cex.legend=0.7,
                    legendwhere="topleft",
                    setpar=TRUE,axisline=3,
                    horlines=TRUE,
                    greyrect=FALSE,
                    addpoints=TRUE){
  
  if(setpar)par(cex.axis=cex.axis, mar=c(3,5,2,5), las=1, cex.lab=1.2, yaxs="i", tcl=0.2)
  par(cex.lab=cex.lab)
  palette(my_co2cols())
  
  if(is.null(xlim))xlim <- with(df, c(min(Date)-15, max(Date)+15))
  if(is.null(ylim))ylim <- c(0,2.8)
  
  smoothplot(Date, LAI, g=treatment, R="Ring", ylim=ylim, xlim=xlim, 
             ylab=ylab, xlab="",
             data=df, kgam=18, axes=FALSE,
             polycolor=c(alpha("royalblue",0.4),alpha("pink",0.4)))
  
  l <- legend("topleft", c("Ambient","Elevated"), title=expression(italic(C)[a]~treatment), 
              fill=my_co2cols(), bty="n", cex=cex.legend)
  axis(2) #, at=seq(-0.75,0.75,by=0.25))
  box()
  
  timeseries_axis()  
}



#-----------------------------------------------------------------------------------------#
# Diagnostic plots. Not for manuscript.

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




figure_breakpoint <- function(df){
  dfa <- summaryBy(LAI + Rain_mm_Tot.mean ~ Date, data=df, FUN=mean, keep.names=TRUE)
  dfa$numDate <- as.numeric(dfa$Date - min(dfa$Date))
  
  breakguess <- as.numeric(as.Date(c("2012-12-02","2013-1-23",
                                     "2013-2-27","2013-7-14",
                                     "2013-10-22","2014-2-1","2014-12-1")) - min(dfa$Date))
  
  lin <- lm(LAI ~ numDate, data=dfa)
  library(segmented)
  seg <- segmented(lin, seg.Z=~numDate, 
                   control=seg.control(n.boot=100, it.max=20),
                   psi=list(numDate=breakguess))
  
  p <- predict(seg,dfa,se.fit=TRUE)
  dfa$LAIpred <- p$fit
  dfa$LAIpred_SE <- p$se.fit
  
  with(dfa, {
    
    plot(Date, LAI, pch=19, ylim=c(0.8,2))
    addpoly(Date, LAIpred+2*LAIpred_SE,LAIpred - 2*LAIpred_SE)
    lines(Date, LAIpred)
    
  })
  
  arrows(x0=flushingdates(), x1=flushingdates(), y0=2, y1=1.9, length=0.05)
}


#-----------------------------------------------------------------------------------------------------#
# Used to be in manuscript, might still be useful



figure_timeseries <- function(df,flatcan_byCO2,
                              xlim=NULL, ylim=NULL,
                              legend=TRUE,
                              ylab=expression(LAI~~(m^2~m^-2)),
                              cex.lab=1.1, cex.axis=0.9, cex.legend=0.7,
                              legendwhere="topleft",
                              setpar=TRUE,axisline=3,
                              adddata=c("rain","litter"),
                              horlines=TRUE,
                              greyrect=FALSE,
                              addpoints=TRUE){
  
  adddata <- match.arg(adddata)
  
  df <- df[order(df$Date),]
  
  if(setpar)par(cex.axis=cex.axis, mar=c(3,5,2,5), las=1, cex.lab=1.2, yaxs="i")
  par(cex.lab=cex.lab)
  palette(my_co2cols())
  
  if(is.null(xlim))xlim <- with(df, c(min(Date)-15, max(Date)+15))
  if(is.null(ylim))ylim <- with(df, c(0, 1.08*max(LAI)))
  
  with(subset(df, treatment == "ambient"),
       plot(Date,LAI, col=palette()[1], type='l', lwd=2, 
            xlab="",
            ylab=ylab,
            axes=FALSE,
            ylim=ylim,
            xlim=xlim,
            panel.first={
              smoothplot(Date, LAI, g=treatment, R="Ring", 
                         data=facegap_cloudy_byring, kgam=18, pointcols="white", 
                         linecols=alpha("lightgrey",0.7), add=TRUE)              
            }
       ))
  
  with(subset(df, treatment == "elevated"),
       points(Date, LAI, col=palette()[2], type='l', lwd=2)
  )
  
  with(subset(df, treatment == "elevated"),
       points(Date, LAI, col="white", cex=0.05, pch=19)
  )
  with(subset(df, treatment == "ambient"),
       points(Date, LAI, col="white", cex=0.05, pch=19)
  )
  
  
  if(addpoints){
    with(df, points(Date, LAI, pch=19, cex=0.05, col="white"))
  }
  
  with(flatcan_byCO2, points(Date, LAI.mean, pch=21, bg=treatment, col="black"))
  
  
  l <- legend("topleft", c("Ambient","Elevated"), title=expression(italic(C)[a]~treatment), 
              fill=my_co2cols(), bty="n", cex=cex.legend)
  legend(l$rect$left + l$rect$w, l$rect$top, c(expression(tau[d]),"Photos"), pch=c(19,21),
         lty=c(1,-1), pt.bg=c("white","grey"), cex=cex.legend, bty='n', pt.cex=c(0.05,1),
         title="Method")
  
  
  axis(2)
  box()
  
  par(new=TRUE)
  if(adddata == "rain"){
    with(faceraindaily, plot(Date, Rain.ROS, type='h', ylim=c(0,200),col="dimgrey",
                             axes=FALSE, xlim=xlim, ann=FALSE))
    axis(4, at=c(0,25,50,75,100))
    mtext(side=4, cex=cex.lab, line=axisline, text="Daily rain (mm)", las=0)
  }
  if(adddata == "litter"){
    
    with(subset(litter_byCO2, treatment == "ambient"), plot(Date, dLAIlitter.mean, pch=15, col=my_co2cols()[1],
                                                            ann=FALSE, axes=FALSE, xlim=xlim,cex=0.8,
                                                            panel.last=adderrorbars(Date,dLAIlitter.mean,
                                                                                    dLAIlitter.se,"updown",
                                                                                    col=my_co2cols()[1],barlen=0.01),
                                                            ylim=c(0,1.5)))
    with(subset(litter_byCO2, treatment == "elevated"), points(Date+10, dLAIlitter.mean, pch=15, cex=0.8,
                                                               panel.last=adderrorbars(Date+10,dLAIlitter.mean,
                                                                                       dLAIlitter.se,"updown",
                                                                                       col=my_co2cols()[2],barlen=0.01),
                                                               col=my_co2cols()[2]))
    
    axis(4, at=c(0,0.1,0.2,0.3,0.4,0.5))
    mtext(side=4, cex=cex.lab, line=axisline, text=expression(Litter~production~(m^2~m^-2)), las=0)
  }
  
  
  timeseries_axis()  
  
  return(invisible(xlim))
}






figure_litter <- function(lidLAIlitter){
  da <- summaryBy(. ~ Date + treatment, FUN=mean, data=dLAIlitter, keep.names=TRUE)
  
  da$laprod <- with(da, 30.5 * (dLAI+dLAIlitter.mean)/ndays)
  da$lit <- with(da, 30.5 * dLAIlitter.mean/ndays)
  
  par(mar=c(3,5,2,2), cex.lab=1.1,tcl=0.2,las=1)
  with(subset(da, treatment == "ambient"), plot(Date, laprod, type='l', col="blue",
                                                axes=FALSE,xlab="",
                                                ylab=expression("Leaf or litter production"~(m^2~m^-2~mon^-1)),
                                                ylim=c(-0.05,0.8)))
  with(subset(da, treatment == "elevated"), lines(Date, laprod, col="red"))
  
  with(subset(da, treatment == "ambient"), lines(Date, lit, col="blue", lty=5))
  with(subset(da, treatment == "elevated"), lines(Date, lit, col="red", lty=5))
  abline(h=0, col="darkgrey")
  axis(2)
  box()
  timeseries_axis()
  l <- legend("topleft", c("Leaf production","Litter production"), lty=c(1,5), bty='n')
  legend(l$rect$left + l$rect$w, l$rect$top, 
         c(expression(a*italic(C)[a]),expression(e*italic(C)[a])),
         col=my_co2cols(), lty=1, bty='n')
  
}





