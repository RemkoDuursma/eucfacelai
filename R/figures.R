
figure_dLAI_after_drought <- function(makepdf=TRUE,
                                      filename="output/figures/FACE_dLAI_drought2013.pdf"){
  
  
  if(makepdf){
    pdf(filename, width=5, height=6)
    on.exit(dev.off())
  }
  
  
  # diffuse transmittance after mid-July (data two weeks before stable and a bit noisy)
  df1 <- subset(facegap_byring, Date > as.Date("2013-7-14"))
  
  # litter fall since early july
  df2 <- subset(litter, Date >= as.Date("2013-7-8"))
  df2$dLAI.mean[df2$Date == min(df2$Date)] <- 0
  
  # cumulative litterfall
  df2$dLAI_cumsum <- do.call(c,with(df2, tapply(dLAI.mean,Ring,cumsum)))
  
  # change in LAI for diffuse transmittance data.
  l <-  sapply(split(df1, df1$Ring),function(x)x$LAI[x$Date==min(x$Date)])
  lai0 <- data.frame(Ring=names(l), LAI0=as.vector(l))
  df1 <- merge(df1, lai0)
  df1$dLAI <- with(df1, LAI - LAI0)
  
  # averaged by CO2.
  se <- function(x)sd(x)/sqrt(length(x))
  df2a <- summaryBy(dLAI_cumsum ~ treatment + Date, FUN=c(mean,se), data=df2)
  df1a <- summaryBy(dLAI ~ treatment + Date, FUN=c(mean,se), data=df1)
  
  
  if(!makepdf)windows(5,6)
  par(mar=c(5,5,2,2), cex.lab=1.3, cex.axis=0.9)
  palette(c("blue","red"))
  plotBy(dLAI.mean ~ Date |treatment, data=df1a, type='l', pch=19, legend=FALSE,
         ylab=expression(Delta*LAI~"since early July 2013"~~(m^2~m^-2)),
         ylim=c(-1,0.2),xlim=as.Date(c("2013-7-1","2013-12-1")))
  palette(c("royalblue","hotpink"))
  plotBy(-dLAI_cumsum.mean ~ Date |treatment, data=df2a, type='o', pch=17, legend=FALSE,
         add=TRUE, cex=1.3,lty=5)
  legend("topright", c("Diffuse transmittance","Litter fall"),
         bty='n',
         lty=c(1,5), pch=c(-1,17))
  
  
  
}




figure_dLAIdt_smooth <- function(makepdf=F, how=c("byring", "mean"), 
                                 setpar=TRUE, axislabels=TRUE, xlab="Date",
                                 kgam=15){
  
  how <- match.arg(how)
  
  smoothlai <- makesmoothLAI(facegap_byring, kgam=kgam, how=how)
  
  if(how == "mean")smoothlai <- list(smoothlai)
  
  xAT <- seq.Date(as.Date("2012-10-1"), by="2 months", length=50)
  xATminor <- seq.Date(as.Date("2012-10-1"), by="1 month", length=100)
  
  xl <- c(as.Date("2012-10-1"),max(smoothlai[[1]]$Date))
  
  if(setpar)par(mar=c(5,7,2,2), cex.lab=1.2,  cex.axis=0.8)
  with(smoothlai[[1]], plot(Date, dLAI/ndays, type='n', ylim=c(-0.02,0.02),
                            axes=FALSE,ann=FALSE,
                            xlim=xl))  
  abline(h=0, lty=5)
  if(how == "byring"){
    for(i in 1:6)with(smoothlai[[i]], lines(Date, dLAI/ndays, 
                                            col=c("red","blue","blue","red","red","blue")[i]))
  }
  if(how == "mean"){
    with(smoothlai[[1]], lines(Date, dLAI/ndays, col="black", lwd=2))
    
  }
  axis.Date(1, at=xAT, labels=FALSE)
  axis.Date(1, at=xATminor, labels=FALSE, tcl=-0.25 )
  axis(2)
  box()
  
  
  mtext(side=1, text=xlab, line=3, outer=F, cex=1.2)
  mtext(side=2, text=expression(frac(dLAI,dt)~(m^2~m^-2~d^-1)), 
        at=0, line=3, outer=F, cex=1.2)
  
  axis.Date(1, at=xAT, format="%b-'%y" , labels=axislabels)
  axis.Date(1, at=xATminor, labels=FALSE, tcl=-0.25 )
  
  return(invisible(xl))
}


# Plot raw data (not for manuscript)
plotone <- function(date, ring, df=FACE_PAR_cloudy, xlim=c(4,20), addlegend=TRUE){
  
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


figure_FACEPAR_allcloudydata <- function(minnrHH, dfr=FACE_PAR_cloudy,
                                         filename="output/figures/FACE_PAR_allcloudydata.pdf"){
  hhtable <- table(dfr$Date)
  Dates <- as.Date(names(hhtable))[hhtable > 6*minnrHH]
  on.exit(dev.off())
  pdf(filename, width=12, height=7)
  for(i in 1:length(Dates))plotsix(Dates[i])
  
}


# Plot raw PAR data - not for manuscript.
plotFACEPAR <- function(Ring, Date, df=FACE_PAR, setmargins=TRUE){
  
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




figure_gapfrac_3methods_May <- function(df,
                                        makepdf=TRUE,
                                        filename="output/figures/FACE_gapfrac_3methods_May.pdf"){
  
  
  if(makepdf){
    pdf(filename, width=9, height=4)
    on.exit(dev.off())
  }
  
  if(!makepdf)windows(9,4)
  par(mfrow=c(1,3), mar=c(5,5,1,1), cex.axis=1.1, cex.lab=1.4)
  
  plot11 <- function(x,y,...){
    plot(x,y,...)
    abline(0,1)
    ablineclip(lm(y ~ x),x1=min(x,na.rm=TRUE),
               x2=max(x,na.rm=TRUE), lty=5)
    #     pointLabel(x,y,as.character(1:6))
    thigmophobe.labels(x,y,as.character(1:6))
  }
  
  with(df, plot11(Gapfraction.PAR.mean, DIFN,
                  xlab=expression(tau~"(PAR sensors) (-)"),
                  ylab=expression(tau~"(LAI-2200) (-)"),
                  pch=19,cex=1.3,
                  ylim=c(0.15,0.3),
                  xlim=c(0.15,0.3)))
  
  with(df, plot11(Gapfraction.PAR.mean, Gapfraction.mean,
                  xlab=expression(tau~"(PAR sensors) (-)"),
                  ylab=expression(tau~"(Zenith photos) (-)"),
                  pch=19,cex=1.3,
                  xlim=c(0.15,0.3),
                  ylim=c(0.4,0.6)))
  
  with(df, plot11(DIFN, Gapfraction.mean,
                  pch=19,cex=1.3,
                  ylab=expression(tau~"(Zenith photos) (-)"),
                  xlab=expression(tau~"(LAI-2200) (-)"),
                  xlim=c(0.15,0.3),
                  ylim=c(0.4,0.6)))
  
  
}




figure_gapfraction_byCO2 <- function(filename="output/figures/gapfraction_bydateCO2.pdf",
                                     df=facegap_byCO2,
                                     makepdf=TRUE,
                                     xlim=NULL, ylim=NULL,
                                     rain=c("daily","cumulative","none")){
  
  rain <- match.arg(rain)
  
  if(makepdf){
    pdf(filename, width=8, height=4)
    on.exit(dev.off())
  }
  
  par(cex.lab=1.1, cex.axis=0.9, mar=c(5,5,2,5), las=1)
  palette(c("blue","red"))
  
  xAT <- seq.Date(as.Date("2012-11-1"), by="2 months", length=50)
  xATminor <- seq.Date(as.Date("2012-11-1"), by="1 month", length=100)
  
  if(is.null(xlim))xlim <- with(df, c(min(Date)-15, max(Date)+15))
  if(is.null(ylim))ylim <- with(df, c(0, 1.05*max(Gapfraction.mean)))
  
  with(df, plot(Date, Gapfraction.mean, col=treatment, pch=19,
                ylab="Canopy transmittance (-)",
                axes=FALSE,
                ylim=ylim,
                xlim=xlim,
                panel.first={
                  abline(h=seq(0,1,by=0.05),col="grey")
                  adderrorbars(df$Date, df$Gapfraction.mean, df$Gapfraction.SE, 
                               direction="updown", col=treatment) 
                }  
  ))
  axis.Date(1, at=xAT, format="%b-'%y" )
  axis.Date(1, at=xATminor, labels=FALSE, tcl=-0.25 )
  
  axis(2)
  box()
  
  # # option 1 : daily rainfalls
  if(rain == "daily"){
    par(new=TRUE)
    with(rosrain, plot(Date, Rain_daily, type='h', ylim=c(0,200),
                       axes=FALSE, xlim=Xlim, ann=FALSE))
    axis(4, at=c(0,25,50,75,100))
    mtext(side=4, cex=1.1, line=3, text="Daily rain (mm)", las=0)
  }
  
  # option 2: cumulative rainfall.
  if(rain == "cumulative"){
    par(new=TRUE)
    rosrain2 <- subset(rosrain, Date >= min(df$Date))
    with(rosrain2, plot(Date, cumsum(Rain_daily), type='l', ylim=c(0,1600),
                        axes=FALSE, xlim=xlim, ann=FALSE))
    axis(4, at=seq(0,800,by=200))
    mtext(side=4, cex=1.1, line=3, text="Cumulative rain (mm)", las=0)
  }
  
  
  CO2legend("bottomleft", cex=0.8, pt.cex=1, bg="white")
  
  
  return(invisible(xlim))
}



figure_gapfraction_byringdate <- function(df=facegap_byring,
                                          makepdf=TRUE,
                                          xlim=NULL, ylim=NULL,
                                          ptcex=0.7,
                                          filename="output/figures/gapfraction_bydatering.pdf"){
  
  
  if(makepdf){
    pdf(filename, width=8, height=4)
    on.exit(dev.off())
  }
  
  par(cex.lab=1.1, cex.axis=0.9)
  
  xAT <- seq.Date(as.Date("2012-11-1"), by="2 months", length=50)
  xATminor <- seq.Date(as.Date("2012-11-1"), by="1 month", length=100)
  if(is.null(xlim))xlim <- c(min(df$Date)-15, max(df$Date)+15)
  if(is.null(ylim))ylim <- c(0,max(df$Gapfraction.mean))
  
  palette(c("blue","red"))
  with(df, plot(Date, Gapfraction.mean, col=treatment, 
                pch=19,cex=ptcex,
                axes=FALSE,
                ylab="Canopy transmittance (-)",
                xlim=xlim,
                ylim=ylim,
                panel.first=abline(h=seq(0,1,by=0.05),col="grey")))
  
  axis.Date(1, at=xAT, format="%b-'%y" )
  axis.Date(1, at=xATminor, labels=FALSE, tcl=-0.25 )
  
  #   with(subset(df, Rain_mm_Tot > 5), points(Date, Gapfraction.mean, pch=19, cex=1.1, col="green"))
  
  axis(2)
  box()
  
  CO2legend("bottomleft", cex=0.8, pt.cex=ptcex, bg="white")
  
  return(invisible(xlim))  
}



figure_LAI_11 <- function(df=flatcan_byring, 
                          makepdf=TRUE,
                          filename="output/figures/LAI_PARFlatcan_11.pdf"
){
  
  
  if(makepdf){
    pdf(filename, width=6, height=6)
    on.exit(dev.off())
  }
  
  palette(brewer.pal(6, "Set2"))
  
  par(xaxs="i", yaxs="i", las=2, cex.lab=1.2, cex.axis=0.9,
      mar=c(5,5,2,2))
  with(df, plot(LAI, LAI.PAR.mean,
                xlab=expression(Total~area~index~(Photos)~(m^2~m^-2)),
                ylab=expression(Total~area~index~(Diffuse~transmittance)~(m^2~m^-2)),
                xlim=c(0.75,2.75),ylim=c(0.75,2.75),
                axes=FALSE,
                pch=19, col=Ring))
  
  a <- seq(0.75,2.75,by=0.25)
  axis(1,at=a)
  axis(2, at=a)
  abline(0,1)
  box()
  with(df, ablineclip(lm(LAI.PAR.mean ~ LAI), lty=5,
                      x1=min(LAI), x2=max(LAI)))        
  
  legend("bottomright", as.character(1:6), title="Ring", col=palette(), pch=19, 
         cex=0.9, pt.cex=1)
  
}


figure_LAI_byCO2_2 <- function(filename="output/figures/LAI_bydateCO2_2.pdf",
                               df=facegap_byCO2,
                               lit=litter_byCO2,
                               xlim=NULL, ylim=NULL,
                               legend=TRUE,
                               makepdf=TRUE){
  
  
  
  if(makepdf){
    pdf(filename, width=8, height=4)
    on.exit(dev.off())
  }
  
  par(cex.lab=1.1, cex.axis=0.9, mar=c(5,5,2,5), las=1)
  palette(c("blue","red"))
  
  xAT <- seq.Date(as.Date("2012-11-1"), by="2 months", length=50)
  xATminor <- seq.Date(as.Date("2012-11-1"), by="1 month", length=100)
  
  if(is.null(xlim))xlim <- with(df, c(min(Date)-15, max(Date)+15))
  if(is.null(ylim))ylim <- with(df, c(0, 1.08*max(LAI.mean)))
  
  #   with(df, plot(Date, LAI.mean, col=treatment, pch=19,
  #                 ylab=expression(Total~area~index~~(m^2~m^-2)),
  #                 axes=FALSE,
  #                 ylim=ylim,
  #                 xlim=xlim,
  #                 panel.first={
  #                   abline(h=seq(0,4,by=0.25),col="grey")
  #                   adderrorbars(df$Date, df$LAI.mean, df$LAI.SE, 
  #                                direction="updown", col=treatment) 
  #                 }  
  #   ))
  df$LAIup <- with(df, LAI.mean + LAI.SE)
  df$LAIdown <- with(df, LAI.mean - LAI.SE)
  with(df, plot(Date, LAI.mean, col=treatment, pch=19,
                ylab=expression(Total~area~index~~(m^2~m^-2)),
                axes=FALSE,
                ylim=ylim,
                xlim=xlim,
                panel.first={
                  plotBy(LAIup ~ Date | treatment, type='l', lty=5, data=df,add=TRUE,legend=FALSE)
                  plotBy(LAIdown ~ Date | treatment, type='l', lty=5, data=df,add=TRUE,legend=FALSE)
                }
  ))
  
  
  axis.Date(1, at=xAT, format="%b-'%y" )
  axis.Date(1, at=xATminor, labels=FALSE, tcl=-0.25 )
  
  axis(2)
  box()
  
  par(new=TRUE)
  cols <- c("royalblue","hotpink")
  lit$dLAI_SE <- with(lit, dLAI.sd/sqrt(3))
  with(lit, plot(Date, dLAI.mean, ylim=c(0,0.8),
                 pch=17, col=cols[treatment],
                 panel.first={
                   with(lit, adderrorbars(Date, dLAI.mean, dLAI_SE, 
                                          direction="updown", col=cols[treatment]))
                 },
                 axes=FALSE, xlim=xlim, ann=FALSE))
  axis(4, at=c(0,0.1,0.2,0.3,0.4))
  mtext(side=4, cex=1.1, line=3, text=expression("Litter fall LAI"~~(m^2~m^-2~month^-1)), las=0)
  
  
  if(legend)CO2legend("bottomleft", cex=0.8, pt.cex=1, bg="white")
  
  return(invisible(xlim))
}




figure_LAI_byCO2_3 <- function(filename="output/figures/LAI_bydateCO2_3.pdf",
                               df=facegap_byCO2,
                               lit=litter_byCO2,
                               xlim=NULL, ylim=NULL,
                               legend=TRUE,
                               makepdf=TRUE){
  
  
  
  if(makepdf){
    pdf(filename, width=8, height=7)
    on.exit(dev.off())
  }
  
  if(!makepdf)windows(8,7)
  par(cex.lab=1.1, cex.axis=0.9, mar=c(0,0,0,0), las=1,
      xaxs="i", yaxs="i",
      oma=c(6,6,2,2))
  layout(matrix(1:3,ncol=1),heights=c(1,0.5,0.5))
  palette(c("blue","red"))
  
  xAT <- seq.Date(as.Date("2012-11-1"), by="2 months", length=50)
  xATminor <- seq.Date(as.Date("2012-11-1"), by="1 month", length=100)
  
  if(is.null(xlim))xlim <- with(df, c(min(Date)-15, max(Date)+15))
  if(is.null(ylim))ylim <- with(df, c(0, 1.1*max(LAI.mean)))
  
  
  df$LAIup <- with(df, LAI.mean + LAI.SE)
  df$LAIdown <- with(df, LAI.mean - LAI.SE)
  with(df, plot(Date, LAI.mean, col=treatment, pch=19,cex=1.1,
                axes=FALSE,
                ylim=ylim,
                xlim=xlim,
                ann=FALSE,
                panel.first={
                  plotBy(LAIup ~ Date | treatment, type='l', lty=5, data=df,add=TRUE,legend=FALSE)
                  plotBy(LAIdown ~ Date | treatment, type='l', lty=5, data=df,add=TRUE,legend=FALSE)
                }
  ))
  
  axis.Date(1, at=xAT, labels=F)
  axis.Date(1, at=xATminor, labels=FALSE, tcl=-0.25 )
  axis(2)
  box()
  
  cols <- c("royalblue","hotpink")
  lit$dLAI_SE <- with(lit, dLAI.sd/sqrt(3))
  with(lit, plot(Date, dLAI.mean, ylim=c(0,0.45),cex=1.3,
                 pch=17, col=cols[treatment],
                 panel.first={
                   with(lit, adderrorbars(Date, dLAI.mean, dLAI_SE, 
                                          direction="updown", col=cols[treatment]))
                 },
                 xlim=xlim, ann=FALSE, axes=FALSE))
  axis.Date(1, at=xAT, labels=F)
  axis.Date(1, at=xATminor, labels=FALSE, tcl=-0.25 )
  axis(2)
  box()
  
  
  with(rosrain, plot(Date, Rain_daily, type='h', ylim=c(0,110),
                     xlim=xlim, ann=FALSE, axes=FALSE))  
  axis.Date(1, at=xAT, format="%b-'%y" )
  axis.Date(1, at=xATminor, labels=FALSE, tcl=-0.25 )
  axis(2)
  box()
  
  
  mtext(side=1, line=3, text="Date", outer=TRUE)
  mtext(side=2, line=3, text=expression("Litter"~~(m^2~m^-2~month^-1)), cex=0.8, 
        at=0.375, las=0,outer=TRUE)
  mtext(side=2, line=3, text=expression(Total~area~index~(m^2~m^-2)), cex=1.1,
        at=0.75, las=0, outer=TRUE)
  mtext(side=2, line=3, text=expression("Rain"~(mm~day^-1)), cex=0.8, 
        at=0.125, las=0,outer=TRUE)
  
  
  return(invisible(xlim))
}




figure_LAI_byCO2_4 <- function(filename="output/figures/LAI_bydateCO2_4.pdf",
                               df=facegap_byCO2,
                               xlim=NULL, ylim=NULL,
                               legend=TRUE,
                               makepdf=TRUE,
                               ylab=expression(Total~area~index~~(m^2~m^-2)),
                               cex.lab=1.1, cex.axis=0.9, cex.legend=0.8,
                               legendwhere="topleft",
                               setpar=TRUE,axisline=3,horlines=TRUE,
                               rain=c("daily","cumulative","none"),
                               greyrect=FALSE,
                               addpoints=FALSE){
  
  
  rain <- match.arg(rain)
  
  if(makepdf){
    pdf(filename, width=8, height=4)
    on.exit(dev.off())
  }
  
  if(setpar)par(cex.axis=cex.axis, mar=c(5,5,2,5), las=1)
  par(cex.lab=cex.lab)
  palette(c("blue","red"))
  
  xAT <- seq.Date(as.Date("2012-11-1"), by="2 months", length=50)
  xATminor <- seq.Date(as.Date("2012-11-1"), by="1 month", length=100)
  
  if(is.null(xlim))xlim <- with(df, c(min(Date)-15, max(Date)+15))
  if(is.null(ylim))ylim <- with(df, c(0, 1.08*max(LAI.mean)))
  
  with(subset(df, treatment == "ambient"),
       plot(Date, LAI.mean, col=palette()[1], type='l', lwd=2, 
            ylab=ylab,
            axes=FALSE,
            ylim=ylim,
            xlim=xlim,
            panel.first={
              if(greyrect){
                y <- ylim[2]/2
                cols_rect <- alpha(c("royalblue","hotpink"),0.5)
                
                rect(as.Date("2013-1-28"),-1,as.Date("2013-3-12"),100,col=cols_rect[1],border=NA)
                text(as.Date("2013-2-18"),y, "1",cex=2)
                rect(as.Date("2013-8-1"),-1,as.Date("2013-11-1"),100,col=cols_rect[2],border=NA)
                text(as.Date("2013-9-15"),y, "2",cex=2)
                rect(as.Date("2013-12-1"),-1,as.Date("2014-2-1"),100,col=cols_rect[1],border=NA)
                text(as.Date("2014-1-1"),y, "3",cex=2)
              }
            }
       ))
  
  with(subset(df, treatment == "elevated"),
       points(Date, LAI.mean, col=palette()[2], type='l', lwd=2)
  )
  
  with(subset(df, treatment == "elevated"),
       points(Date, LAI.mean, col="white", cex=0.05, pch=19)
  )
  with(subset(df, treatment == "ambient"),
       points(Date, LAI.mean, col="white", cex=0.05, pch=19)
  )
  
  
  if(addpoints){
    with(df, points(Date, LAI.mean, pch=19, cex=0.05, col="white"))
    
    
  }
  axis.Date(1, at=xAT, format="%b-'%y" )
  axis.Date(1, at=xATminor, labels=FALSE, tcl=-0.25 )
  
  axis(2)
  box()
  
  l <- legend("topleft", c("Ambient","Elevated"), title=expression(italic(C)[a]~treatment), 
              col=c("blue","red"), bty="n", lty=1, lwd=2, cex=cex.legend)
  
  r <- l$rect
  X <- as.Date("2013-7-1")
  Y <- 2.9
  txt <- "Mean SE:"
  
  text(X,Y, txt, pos=2, cex=cex.legend)
  txtwd <- strwidth(txt, cex=cex.legend)
  
  meanSE <- mean(df$LAI.SE, na.rm=TRUE)
  arrows(x0=X + 0.1*txtwd, x1=X + 0.1*txtwd,
         y0=Y-0.4*meanSE, y1=Y+0.6*meanSE,
         code=3, angle=90, length=0.03)
  
  # # option 1 : daily rainfalls
  if(rain == "daily"){
    par(new=TRUE)
    with(rosrain, plot(Date, Rain_daily, type='h', ylim=c(0,200),
                       axes=FALSE, xlim=xlim, ann=FALSE))
    axis(4, at=c(0,25,50,75,100))
    mtext(side=4, cex=cex.lab, line=axisline, text="Daily rain (mm)", las=0)
  }
  
  # option 2: cumulative rainfall.
  if(rain == "cumulative"){
    par(new=TRUE)
    rosrain2 <- subset(rosrain, Date >= min(df$Date))
    with(rosrain2, plot(Date, cumsum(Rain_daily), type='l', ylim=c(0,1600),
                        axes=FALSE, xlim=xlim, ann=FALSE))
    axis(4, at=seq(0,800,by=200))
    mtext(side=4, cex=cex.lab, line=axisline, text="Cumulative rain (mm)", las=0)
  }
  
  
  return(invisible(xlim))
}















