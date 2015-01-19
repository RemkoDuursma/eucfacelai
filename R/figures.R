
my_co2cols <- function()c("blue","red")
# my_ringcols <- function()rich.colors(6)

my_ringcols <- function(){
  
  reds <- brewer.pal(5,"Reds")[3:5]
  blues <- brewer.pal(5,"Blues")[3:5]
  
  c(reds[1],blues[1:2],reds[2:3],blues[3])
}



# Smoothed gap fraction raw data
figure1 <- function(df){
  
  par(mar=c(3,5,2,2), cex.axis=0.9, cex.lab=1.1)
  smoothplot(Date, Gapfraction.mean, g=Ring, data=df, k=18,axes=FALSE,
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


figure2 <- function(df){
  
  par(mar=c(5,5,2,2), cex.axis=0.9)
  palette(my_co2cols())
  with(df, plot(dLAI_litter, dLAI_PAR, pch=19, col=treatment,
                xlab=expression(Delta*LAI~from~litter~fall~~(m^2~m^-2)),
                ylab=expression(Delta*LAI~from~tau[PAR]~~(m^2~m^-2)),
                xlim=c(0,0.6), ylim=c(0,0.6)))
  abline(0,1)
  predline(lm(dLAI_PAR ~ dLAI_litter, data=df), lty=5)
}


figure3 <- function(df){
  
  par(mar=c(5,5,2,2), cex.axis=0.9)
  
  with(df, plot(LAI, LAI.PAR.mean, 
                ylab=expression(LAI~from~tau[PAR]~~(m^2~m^-2)),
                xlab=expression(LAI~from~canopy~photos~~(m^2~m^-2)),
                pch=19, col=my_co2cols()[treatment],
                xlim=c(0.8,2), ylim=c(0.8,2)))
  abline(0,1)
  predline(lm(LAI.PAR.mean ~ LAI, data=df), lty=5)
  
}



figure4 <- function(df,
                    xlim=NULL, ylim=NULL,
                    legend=TRUE,
                    ylab=expression(Total~area~index~~(m^2~m^-2)),
                    cex.lab=1.1, cex.axis=0.9, cex.legend=0.8,
                    legendwhere="topleft",
                    setpar=TRUE,axisline=3,
                    horlines=TRUE,
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



figure5 <- function(df){

  par(mar=c(5,5,2,2), cex.lab=1.1, cex.axis=0.9, las=1, xaxs="i", yaxs="i")
  with(df, plot(dLAI.mean * 30.5/ndays, dLAI * 30.5/ndays, pch=19, 
               col=c("darkorange","forestgreen")[LAIchange],
               xlab=expression(Leaf~litter~production~~(m^2~m^-2~mon^-1)),
               xlim=c(0,0.6),
               ylim=c(-0.3,0.5),
               ylab=expression(Delta*LAI~from~tau[PAR]~(m^2~m^-2~mon^-1))))
  abline(h=0, lty=5)
  abline(0,1)
  abline(0,-1)
  predline(lm(dLAI ~ dLAI.mean, data=df, subset=dLAI>0), col="forestgreen")
  predline(lm(dLAI ~ dLAI.mean, data=df, subset=dLAI<0), col="darkorange")

}




figure6 <- function(df){
  
  dfa <- summaryBy(LAI ~ Date, data=df, FUN=mean, keep.names=TRUE)
  
  xl <- range(dfa$Date)
  
  par(mfrow=c(4,1), mar=c(0,5,5,2), cex.lab=1.2)
  
  # panel a
  smoothplot(Date, LAI, data=dfa, kgam=18, pointcols="dimgrey", linecols="black", xlim=xl,
             ylab=expression(Total~area~index~~(m^2~m^-2)),
             ylim=c(1,2), axes=FALSE)
  timeseries_axis(FALSE)
  axis(2)
  box()
  
  # panel b
  par(mar=c(1.5,5,1.5,2))
  smoothlai <- makesmoothLAI(dat, kgam=15, how="byring")
  with(smoothlai[[1]], plot(Date, dLAI/ndays, type='n', 
                            ylab=expression(dLAI/dt~(m^2~m^-2~d^-1)),
                            ylim=c(-0.02,0.02),
                            
                            xlim=xl))  
  abline(h=0, lty=5)
  if(how == "byring"){
    for(i in 1:6)with(smoothlai[[i]], lines(Date, dLAI/ndays, 
                                            col=c("red","blue","blue","red","red","blue")[i]))
  }
  if(how == "mean"){
    with(smoothlai[[1]], lines(Date, dLAI/ndays, col="black", lwd=2))
    
  }
  timeseries_axis(FALSE)
  axis(2)
  box()
  
  par(mar=c(1.5,5,1.5,2))
  with(subset(facesoilwater, Date > xl[1]), 
       plot(Date, VWC, type='l', lwd=2, xlim=xl, ylim=c(0,0.4), axes=FALSE,
            panel.first=abline(h=seq(0,0.4,by=0.05), col="grey", lty=5),
            ylab=expression(SWC~~(m^3~m^-3))))
  
  timeseries_axis(FALSE)
  axis(2)
  box()
  
  par(mar=c(5,5,0,2))
  smoothplot(Date, Tair, data=subset(airt, Date > xl[1]), 
             kgam=25, pointcols=alpha("grey",0.8), linecols="black",axes=FALSE,
             panel.first=abline(h=seq(0,30,by=2), col="grey", lty=5),
             ylab=expression(T[air]~~(degree*C)),
             ylim=c(0,30), xlim=xl)
  timeseries_axis(TRUE)
  axis(2)
  box()
  
}



figure7 <- function(dat,
                    how=c("byring", "mean"), 
                                 setpar=TRUE, axislabels=TRUE, 
                                 kgam=15){

  how <- match.arg(how)
  
  smoothlai <- makesmoothLAI(dat, kgam=kgam, how=how)
  
  if(how == "mean")smoothlai <- list(smoothlai)
  
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
  timeseries_axis(FALSE)
  axis(2)
  box()
  
  mtext(side=2, text=expression(frac(dLAI,dt)~(m^2~m^-2~d^-1)), 
        at=0, line=3, outer=FALSE, cex=1.2)
  
  timeseries_axis(TRUE)
  
  return(invisible(xl))
}

