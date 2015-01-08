
readLi <- function(fn){
  
  r <- readLines(fn, 50)
  obsStart <- grep("### Observations",r)
  
  dat <- read.table(fn, skip=obsStart)
  
  names(dat) <- c("AB","Obs","Date","Time","ID","R1","R2","R3","R4","R5")
  dat$DateTime <- ymd_hms(paste(dat$Date, dat$Time), quiet=TRUE)
  
  return(dat)
} 


calcDIFN <- function(fn, fnabv, path="", what=c("mean","all")){

  what <- match.arg(what)
  
  if(path != ""){
    
    fn <- paste0(path,"/",fn)
    fnabv <- paste0(path,"/",fnabv)
  }
  
  dat <- readLi(fn)
  abv <- readLi(fnabv)
  
  # LAI2200 'ring' numbers
  R <- paste0("R",1:5)
  abvR <- abv[,R]
  datR <- dat[,R]
  
  # Interpolate
  abvi <- list()
  for(i in 1:5){
    abvi[[i]] <- approx(x=abv$DateTime, y=abvR[,i], xout=dat$DateTime)$y
  }
  abvi <- do.call(cbind,abvi)
  
  # diffuse weights
  W <- c(0.033,0.097,0.127,0.141,0.102)
  
  # Unweighted matrix of diffuse transmittances
  G <- matrix(ncol=5, nrow=nrow(datR))
  for(i in 1:5)G[,i] <- exp(log(datR[,i]/abvi[,i]))
  G <- as.data.frame(G)
  
  # G multiplied with the weights and 2, so that the 
  # rowSums equal the total diffuse transmittance.
  Dif <- t(apply(G, 1, function(x)2*x*W))
  Dif <- as.data.frame(Dif)
  
  return(list(G=G, DIFN=Dif))
}




MAKE_LI2200_data <- function(fc=flatcan_byring, returnwhat=c("flatcanmay","gapfracagg")){
  
  returnwhat <- match.arg(returnwhat)
  
  fn_base <- c("EUC1.txt","EUC2b.txt","EUC3.txt","EUC4.txt","EUC5.txt","EUC6.txt") 
  fn <- paste0("rawdata/LI2200_2013-05-22/",fn_base)
  abvfile <- "rawdata/LI2200_2013-05-22/EUCABV.txt"
  
  difs <- lapply(fn, function(x)calcDIFN(fn=x, fnabv=abvfile))
  rings <- paste0("R",1:6)
  for(i in 1:length(difs)){
    difs[[i]]$G$Ring <- rings[i]
    difs[[i]]$DIFN$Ring <- rings[i]
  }
  
  
  gapfrac <- do.call(rbind, lapply(difs, "[[","G"))
  names(gapfrac)[1:5] <- paste0("gapfrac.",1:5)
  gapfrac_w <- reshape(gapfrac, direction="long", varying=1:5)
  names(gapfrac_w)[2] <- "AngleNr"
  
  angdf <- data.frame(AngleNr=1:5,
                      zenith=c(7, 23, 38, 53 , 68))
  gapfrac_w <- merge(gapfrac_w, angdf)
  
  gapfracagg <- summaryBy(gapfrac ~ zenith + Ring, data=gapfrac_w, FUN=mean,
                          id=~AngleNr)
  
  if(returnwhat == "gapfracagg")return(gapfracagg)
  
  # Calculate total DIFN
  Wdf <- data.frame(AngleNr=1:5, W=c(0.033,0.097,0.127,0.141,0.102))
  gapfracagg <- merge(gapfracagg, Wdf)
  gapfracagg$DIFN <- with(gapfracagg, gapfrac.mean * W)
  LI2200_DIFN <- summaryBy(DIFN ~ Ring, data=gapfracagg, FUN=sum, keep.names=TRUE)
  LI2200_DIFN$DIFN <- LI2200_DIFN$DIFN * 2
  
  
  flatcanMay <- subset(fc, Date==as.Date("2013-5-22"))
  flatcanMay <- merge(flatcanMay,LI2200_DIFN )
  
  if(returnwhat == "flatcanmay")return(flatcanMay)
}


