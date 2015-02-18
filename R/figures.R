
my_co2cols <- function()c("blue","red")
# my_ringcols <- function()rich.colors(6)

my_ringcols <- function(){
  
  reds <- brewer.pal(5,"Reds")[3:5]
  blues <- brewer.pal(5,"Blues")[3:5]
  
  c(reds[1],blues[1:2],reds[2:3],blues[3])
}

timeseries_axis <- function(labels=TRUE){
  xAT <- seq.Date(as.Date("2012-7-1"), by="2 months", length=50)
  xATminor <- seq.Date(as.Date("2012-7-1"), by="1 month", length=100)
  axis.Date(1, at=xAT, format="%b-'%y", labels=labels )
  axis.Date(1, at=xATminor, labels=FALSE, tcl=-0.25)
}


converttoJPEGs <- function(){
  pdfs <- dir("output/figures", pattern="[.]pdf",full.names=TRUE)
  for(i in 1:length(pdfs)){
    fn <- gsub("[.]pdf",".jpg",pdfs[i])
    shell(paste("convert -density 600",pdfs[i],fn))
  }
}

# Smoothed gap fraction raw data
figure1 <- function(df, ramp){
  
  l <- layout(matrix(c(1,2), ncol=1), heights=c(0.8,2))
  xl <- c(min(ramp$Date), max(df$Date))
  
  par(mar=c(0.5,5,1,2), cex.axis=0.9, cex.lab=1.1, las=1, yaxs="i", tcl=0.2,
      mgp=c(3,0,0))
  with(ramp, plot(Date, CO2target, type='l', col="dimgrey", lwd=2, axes=FALSE, ylim=c(0,160),
                  xlim=xl,
                  xlab="", ylab=expression(Target~Delta*C[a]~(ppm))))
  timeseries_axis(FALSE)
  segments(x0=max(ramp$Date), x1=xl[2], y0=150, y1=150, col="dimgrey", lwd=2)
  axis(2, at=seq(0,150,by=30))
  box()
  plotlabel("(a)", "topleft", inset.x=0.04)
  
  par(mar=c(3,5,0.5,2), cex.axis=0.9, cex.lab=1.1, yaxs="i")
  
  smoothplot(Date, Gapfraction.mean, g=Ring, data=df, k=18,axes=FALSE,
             ylim=c(0,0.4),
             xlab="",xlim=xl,
             ylab=expression(tau[d]~~("-")),
             pointcols=rep("grey",8), linecols=my_ringcols())
  
  
  xAT <- seq.Date(as.Date("2012-7-1"), by="1 month", length=100)
  labs <- substr(format(mAT, "%b"),1,1)
  axis.Date(1, at=xAT, labels=labs, cex.axis=0.6)
  maj <- seq.Date(as.Date("2012-1-1"),by="1 year", length=10)
  axis.Date(1, at=maj, labels=FALSE, tcl=-0.3)
  axis.Date(1, at=maj, labels=FALSE, tcl=0.4)
  yrlab <- seq.Date(as.Date("2012-6-15"), by='1 year', length=10)
  axis.Date(1, at=yrlab, labels=as.character(seq(2012,length=length(yrlab))), tcl=0,
            mgp=c(3,1,0))
  
  axis(2)
  box()
  
  d1 <- as.Date("2013-7-14")
  d2 <- as.Date("2013-11-12")
  Y <- 0.38
  arrows(x0=d1, x1=d2, y0=Y, y1=Y, code=3, length=0.06)
  text(y=Y, x=d2 + 10, "Calibration", pos=4, cex=0.8)
  
  legend("bottomleft", as.character(1:6), lty=1, bty='n',
         col=my_ringcols(), title="Ring", cex=0.8, lwd=2)
  plotlabel("(b)", "topleft", inset.x=0.04)
}




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
  
  if(setpar)par(cex.axis=cex.axis, mar=c(3,5,2,5), las=1, cex.lab=1.2, yaxs="i")
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
                         data=facegap_cloudy_byring, kgam=18, pointcols="white", 
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


figure2 <- function(df){
  
  par(mar=c(5,5,2,2), cex.axis=0.9)
  
  with(df, plot(LAI, LAI.PAR.mean, 
                ylab=expression(LAI~from~tau[d]~~(m^2~m^-2)),
                xlab=expression(LAI~from~canopy~photos~~(m^2~m^-2)),
                pch=19, col=my_co2cols()[treatment],
                xlim=c(0.8,2), ylim=c(0.8,2)))
  abline(0,1)
  predline(lm(LAI.PAR.mean ~ LAI, data=df), lty=5)
  
}




figure3 <- function(df){
  
  par(mar=c(5,5,2,2), cex.lab=1.2,xaxs="i", yaxs="i")
  with(df, plot(BA, LAI.mean, pch=19, cex=1.2, col=my_ringcols(),
                xlab=expression(Basal~area~~(m^2~ha^-1)),
                ylab=expression(bar(LAI)~~(m^2~m^-2)),
                panel.first=predline(lm(LAI.mean ~ BA, data=ba)),
                ylim=c(1,2), xlim=c(18,40)))
  legend("bottomright", as.character(1:6), pch=19, bty='n',
         col=my_ringcols(), title="Ring", cex=0.6, pt.cex=1)

}


figure4 <- function(df,
                    xlim=NULL, ylim=NULL,
                    legend=TRUE,
                    ylab=expression(LAI~anomaly~(m^2~m^-2)),
                    cex.lab=1.1, cex.axis=0.8, cex.legend=0.7,
                    legendwhere="topleft",
                    setpar=TRUE,axisline=3,
                    horlines=TRUE,
                    greyrect=FALSE,
                    addpoints=TRUE){
    
  if(setpar)par(cex.axis=cex.axis, mar=c(3,5,2,5), las=1, cex.lab=1.2, yaxs="i")
  par(cex.lab=cex.lab)
  palette(my_co2cols())
  
  if(is.null(xlim))xlim <- with(df, c(min(Date)-15, max(Date)+15))
  if(is.null(ylim))ylim <- c(-0.8,0.8)
  
  smoothplot(Date, LAIanomaly, g=treatment, R="Ring", ylim=ylim, xlim=xlim, ylab=ylab, xlab="",
             data=df, kgam=18, axes=FALSE)
  
  l <- legend("topleft", c("Ambient","Elevated"), title=expression(italic(C)[a]~treatment), 
              fill=my_co2cols(), bty="n", cex=cex.legend)
  axis(2, at=seq(-0.75,0.75,by=0.25))
  box()
  
  timeseries_axis()  
}


figure5 <- function(df){

  Cols <- c("darkorange","forestgreen")
  
  par(mar=c(5,5,2,2), cex.lab=1.1, cex.axis=0.9, las=1, xaxs="i", yaxs="i")
  with(df, plot(dLAIlitter.mean * 30.5/ndays, dLAI * 30.5/ndays, pch=c(19,21)[treatment],
               col=Cols[LAIchange],
               xlab=expression(Leaf~litter~production~~(m^2~m^-2~mon^-1)),
               xlim=c(0,0.6),
               ylim=c(-0.3,0.5),
               ylab=expression(Delta*LAI~from~tau[d]~(m^2~m^-2~mon^-1))))
  abline(h=0, lty=5)
  abline(0,-1)
  predline(lm(dLAI ~ dLAIlitter.mean, data=df, subset=dLAI>0), col=Cols[2])
  predline(lm(dLAI ~ dLAIlitter.mean, data=df, subset=dLAI<0), col=Cols[1])
  l <- legend("topleft", c(expression(Delta*LAI < 0),expression(Delta*LAI > 0)), fill=Cols, bty='n', cex=0.8)
  
  legend(l$rect$left + l$rect$w, l$rect$top, 
         c(expression(a*italic(C)[a]),expression(e*italic(C)[a])), pch=c(19,21), bty='n', cex=0.8, pt.cex=1)
  
}


figure6 <-  function(df){

  par(mar=c(5,5,2,2), cex.lab=1.2,xaxs="i", yaxs="i", mfrow=c(1,2))
  
  with(df, plot(LAI.mean, LAIlitter_annual, pch=19, cex=1.2, col=my_ringcols(),
                ylab=expression(Litter~production~~(m^2~m^-2~yr^-1)),
                xlab=expression(bar(LAI)~~(m^2~m^-2)),
                panel.first={
                  predline(lm(LAIlitter_annual ~ LAI.mean, data=df))
                },
                xlim=c(1,2), ylim=c(1,2)))
  plotlabel("(a)", "topleft")
  box()
  
  par(xaxs="r")
  lmci <- lm(LL ~ treatment-1, data=ba)
  CI <- confint(lmci)
  barplot2(coef(lmci), names.arg=levels(ba$treatment), col=my_co2cols(),
                plot.ci=TRUE, ci.l=CI[,1], ci.u=CI[,2], ci.width=0.08,
                ylab="Leaf lifespan (yr)")
  plotlabel("(b)", "topleft")
}


figure7 <- function(df){
    
  xin <- 0.02 # for panel label x inset
  
  dfa <- summaryBy(LAI ~ Date, data=df, FUN=mean, keep.names=TRUE)
  
  xl <- range(facesoilwater$Date)
  
  par(mfrow=c(4,1), mar=c(0,7,5,6), cex.lab=1.3, las=1, mgp=c(4,1,0),yaxs="i")
  
  # panel a
  smoothplot(Date, LAI, data=dfa, kgam=18, pointcols="dimgrey", linecols="black", 
             xlim=xl,
             ylab=expression(LAI~~(m^2~m^-2)),
             ylim=c(0.8,2), axes=FALSE)
  timeseries_axis(FALSE)
  axis(2)
  box()
  plotlabel("(a)","topleft", inset.x=xin)
  
  # panel b
  par(mar=c(0,7,1.5,6))
  
 
  df$Time <- as.numeric(df$Date - min(df$Date))
  g <- gamm(LAI ~ s(Time, k=18), random = list(Ring=~1), data=df)
  
  plotCIdate(g, df, axes=FALSE,xlab="",xlim=xl,
       ylim=c(-0.016, 0.016),
       ylab=expression(dLAI/dt~(m^2~m^-2~d^-1)))
  abline(h=0)
  
  timeseries_axis(FALSE)
  axis(2)
  box()
  plotlabel("(b)","topleft", inset.x=xin)
  
  arrows(x0=flushingdates(), x1=flushingdates(), y0=0.015, y1=0.01,
         length=0.05)
  
  # panel c
  par(mar=c(1.5,7,1.5,6))
  with(subset(facesoilwater, Date > xl[1]), 
       plot(Date, VWC, type='l', lwd=2, xlim=xl, ylim=c(0,0.4), axes=FALSE,
            col="cornflowerblue",
            #panel.first=abline(h=seq(0,0.4,by=0.05), col="grey", lty=5),
            #panel.first=abline(h=0.16, col="blue", lwd=2),
            ylab=expression(SWC~~(m^3~m^-3))))

  timeseries_axis(FALSE)
  axis(2)
  box()
  par(new=TRUE)
  with(faceraindaily, plot(Date, Rain.ROS, type='h', ylim=c(0,120), xlim=xl,
                           axes=FALSE, ann=FALSE))
  axis(4)
  mtext(expression(Rain~(mm~day^-1)), line=3, side=4, las=0, cex=0.9)
  plotlabel("(c)","topleft", inset.x=xin)
  
  # panel d
  par(mar=c(5,7,0,6))
  smoothplot(Date, Tair, data=subset(airt, Date > xl[1]), 
             kgam=25, pointcols=alpha("grey",0.8), linecols="black",axes=FALSE,
#              panel.first=abline(h=seq(0,30,by=2), col="grey", lty=5),
             ylab=expression(T[air]~~(degree*C)), 
             xlab="",
             ylim=c(0,30), xlim=xl)
  timeseries_axis(TRUE)
  axis(2)
  box()
  plotlabel("(d)","topleft", inset.x=xin)
  
}


figureSI1 <- function(df1, df2){
  
  Cols <- c("grey49","black")
  
  dfa1 <- summaryBy(Gapfraction.mean ~ Date, data=df1, FUN=mean, keep.names=TRUE)
  dfa2 <- summaryBy(Gapfraction.mean ~ Date, data=df2, FUN=mean, keep.names=TRUE)
  
  par(mar=c(3,5,2,2))
  smoothplot(Date, Gapfraction.mean, data=df2, kgam=18, pointcols=alpha(Cols[1],0.5), linecols=Cols[1],
             ylim=c(0,0.4), axes=FALSE,
             xlab="", ylab=expression(tau~~("-")))
  smoothplot(Date, Gapfraction.mean, data=df1, kgam=18, add=TRUE, pointcols=alpha(Cols[2],0.5),
             linecols=Cols[2])
  timeseries_axis(TRUE)
  axis(2)
  box()
  legend("bottomleft", c(expression("Diffuse only"~(tau[d])),"All data"), pch=19, col=rev(Cols), bty='n')
  
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

