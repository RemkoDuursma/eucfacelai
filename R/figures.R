
my_co2cols <- function()c("blue","red")
my_ringcols <- function()rich.colors(6)

figure_smoothgapfraction <- function(df){
  
  par(mar=c(3,5,2,2), cex.axis=0.9, cex.lab=1.1)
  smoothplot(Date, Gapfraction.mean, g=Ring, data=df, k=20,axes=FALSE,
             ylim=c(0,0.4),
             xlab="",
             ylab=expression(tau[PAR]~~("-")),
             pointcols=rep("grey",8), linecols=my_ringcols())
  timeseries_axis()
  axis(2)
  box()
  legend("bottomleft", as.character(1:6), lty=1, 
         col=my_ringcols(), title="Ring", cex=0.8, lwd=2)
}



figure_LAI_timeseries <- function(df,
                               xlim=NULL, ylim=NULL,
                               legend=TRUE,
                               ylab=expression(Total~area~index~~(m^2~m^-2)),
                               cex.lab=1.1, cex.axis=0.9, cex.legend=0.8,
                               legendwhere="topleft",
                               setpar=TRUE,axisline=3,horlines=TRUE,
                               greyrect=FALSE,
                               addpoints=TRUE){
  
    
  if(setpar)par(cex.axis=cex.axis, mar=c(3,5,2,5), las=1, cex.lab=1.2)
  par(cex.lab=cex.lab)
  palette(my_co2cols())

  if(is.null(xlim))xlim <- with(df, c(min(Date)-15, max(Date)+15))
  if(is.null(ylim))ylim <- with(df, c(0, 1.08*max(LAI.mean)))
  
  with(subset(df, treatment == "ambient"),
       plot(Date, LAI.mean, col=palette()[1], type='l', lwd=2, 
            xlab="",
            ylab=ylab,
            axes=FALSE,
            ylim=ylim,
            xlim=xlim,
            panel.first={
              smoothplot(Date, LAI, g=treatment, R="Ring", 
                         data=facegap_cloudy_byring, kgam=15, pointcols="white", 
                         linecols=alpha("lightgrey",0.7), add=TRUE)              
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
  
  timeseries_axis()  

  axis(2)
  box()
  
  l <- legend("topleft", c("Ambient","Elevated"), title=expression(italic(C)[a]~treatment), 
              col=my_co2cols(), bty="n", lty=1, lwd=2, cex=cex.legend)
  
  par(new=TRUE)
  with(faceraindaily, plot(Date, Rain.ROS, type='h', ylim=c(0,200),
                     axes=FALSE, xlim=xlim, ann=FALSE))
  axis(4, at=c(0,25,50,75,100))
  mtext(side=4, cex=cex.lab, line=axisline, text="Daily rain (mm)", las=0)
  
  return(invisible(xlim))
}






figure_dLAIdrought2013 <- function(df){
  
  
  par(mar=c(5,5,2,2), cex.axis=0.9)
  palette(my_co2cols())
  with(df, plot(dLAI_litter, dLAI_PAR, pch=19, col=treatment,
                xlab=expression(Delta*LAI~from~litter~fall~~(m^2~m^-2)),
                ylab=expression(Delta*LAI~from~tau[PAR]~~(m^2~m^-2)),
                xlim=c(0,0.6), ylim=c(0,0.6)))
  abline(0,1)
  predline(lm(dLAI_PAR ~ dLAI_litter, data=df), lty=5)
}


figure_flatcan_PARLAI_comparison <- function(df){

  par(mar=c(5,5,2,2), cex.axis=0.9)
  
  with(df, plot(LAI, LAI.PAR.mean, 
                ylab=expression(LAI~from~tau[PAR]~~(m^2~m^-2)),
                xlab=expression(LAI~from~canopy~photos~~(m^2~m^-2)),
                pch=19, col=my_co2cols()[treatment],
                xlim=c(0.8,2), ylim=c(0.8,2)))
  abline(0,1)
  predline(lm(LAI.PAR.mean ~ LAI, data=df), lty=5)
  

}


figure_dLAI_litter <- function(df){

  par(mar=c(5,5,2,2), cex.lab=1.1, cex.axis=0.9, las=1, xaxs="i", yaxs="i")
  with(df, plot(dLAI.mean * 30.5/ndays, dLAI * 30.5/ndays, pch=19, 
               col=c("darkorange","forestgreen")[LAIchange],
               xlab=expression(Delta*LAI~from~litter~~(m^2~m^-2~mon^-1)),
               xlim=c(0,0.6),
               ylim=c(-0.3,0.5),
               ylab=expression(Delta*LAI~from~tau[PAR]~(m^2~m^-2~mon^-1))))
  abline(h=0, lty=5)
  abline(0,1)
  abline(0,-1)
  predline(lm(dLAI ~ dLAI.mean, data=df, subset=dLAI>0), col="forestgreen")
  predline(lm(dLAI ~ dLAI.mean, data=df, subset=dLAI<0), col="darkorange")

}
















#---------------------------------------------------------------------------------------------#

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





#---------------------------------------------------------------------------------------------#
# Figures not used at the moment.s



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















