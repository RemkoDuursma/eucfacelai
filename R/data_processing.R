
get_ramp <- function(){
  ramp <- read.csv("data/ramp_co2-data.csv", stringsAsFactors = FALSE)
  ramp$DateTime <- as.POSIXct(ramp$Date.time, format="%d/%m/%Y %H:%M", tz="UTC")
  ramp$CO2target <- as.numeric(str_extract(ramp$CO2.target, "[0-9]{2,3}"))
  ramp$Date <- as.Date(ramp$DateTime)
  ramp <- summaryBy(. ~ Date, FUN=mean, na.rm=TRUE, keep.names=TRUE, data=ramp)
return(ramp)
}

get_solvars <- function(){
  
  downloadCSV(c("P0037","SOLVARS"))
  
}


get_flatcan <- function(){
  
  # Should be posted to HIEv. For now, part of repository.
  flatcan <- read.csv("data/FACE_flatcan_gapfraction_all.csv")
  
}

agg_flatcan <- function(dfr, by=c("Ring","CO2"), k=0.5, ...){
  
  
  dfr$Date <- as.Date(dfr$Date)
  names(dfr)[names(dfr) == "gapfraction"] <- "Gapfraction"
  
  n <- length
  flatagg <- summaryBy(Gapfraction ~ Ring + Date, data=dfr,
                       FUN=c(mean,sd,n), id=~treatment)
  flatagg$Gapfraction.SE <- with(flatagg, Gapfraction.sd/sqrt(Gapfraction.n))
  
  # Add LAI
  flatagg$LAI <- -log(flatagg$Gapfraction.mean) / k
  
  
  if(by == "Ring")return(flatagg)
  
  if(by == "CO2"){
    dfr <- flatagg[,c("Date","treatment","Gapfraction.mean","LAI")]
    names(dfr)[3] <- "Gapfraction"
    flatagg2 <- summaryBy(Gapfraction + LAI ~ treatment + Date, data=dfr,
                          FUN=c(mean,sd,n))  
    flatagg2$Gapfraction.SE <- with(flatagg2, Gapfraction.sd/sqrt(Gapfraction.n))
    flatagg2$LAI.SE <- with(flatagg2, LAI.sd/sqrt(LAI.n))
    
    return(flatagg2)
  }
  
  
}



add_PARLAI_to_flatcan <- function(df1, df2){
  
  x1 <- split(df1, df1$Ring)
  x2 <- split(df2, df2$Ring)
  
  for(i in 1:length(x2)){
    
    ap <- approxExtrap(x=as.numeric(x1[[i]]$Date),
                       y=x1[[i]]$Gapfraction.mean,
                       xout=as.numeric(x2[[i]]$Date))
    x2[[i]]$Gapfraction.PAR.mean <- ap$y
    
    ap <- approxExtrap(x=as.numeric(x1[[i]]$Date),
                       y=x1[[i]]$LAI,
                       xout=as.numeric(x2[[i]]$Date))
    x2[[i]]$LAI.PAR.mean <- ap$y
  }
  
  df2 <- rbind.fill(x2)
return(df2)
}



aggfacegapbyCO2 <- function(df){
  
  SE <- function(x)sd(x)/sqrt(length(x))
  names(df)[names(df) == "Gapfraction.mean"] <- "Gapfraction"
  
  if("LAIanomaly" %in% names(df)){
    dfa <- summaryBy(Gapfraction + LAI  + LAIanomaly ~ Date + treatment, FUN=c(mean,SE), data=df)
  } else {
    dfa <- summaryBy(Gapfraction + LAI ~ Date + treatment, FUN=c(mean,SE), data=df)
  }
  return(dfa)
}


aggFACEPARbyring <- function(df, x=1){
  
  n <- function(x,...)length(x[!is.na(x)])
  facegap <- summaryBy(Gapfraction + Rain_mm_Tot ~ Date + Ring, data=df, FUN=c(mean,sd,n), na.rm=TRUE)
  
  facegap <- facegap[!is.na(facegap$Gapfraction.mean),]
  
  facegap <- merge(facegap, eucface(), by="Ring")
  
  return(facegap)
}



subsetFACEPARbyring <- function(df, minnrHH=4, maxSD=0.05){
  
  # All rings have some observations
  tab <- table(df$Date, df$Ring)
  dats <- as.Date(rownames(tab)[rowSums(tab) == 6])
  
  df$minn <- ave(df$Gapfraction.n, df$Date, FUN=min)
  dats2 <- unique(df$Date[df$minn >= minnrHH])
  
  dats <- as.Date(intersect(dats, dats2), origin="1970-1-1")
  df <- subset(df, Date %in% dats)
  df$minn <- NULL
  
  # find max SD by date
  df$maxSDring <- ave(df$Gapfraction.sd, df$Date, FUN=max, na.rm=TRUE)
  
  # discard days where at least one ring has excessive within-day variance.
  df <- subset(df, maxSDring < maxSD)
  
  # Order
  df <- df[order(df$Date, df$Ring),]
  return(df)
}



aggFACEPARbysensor <- function(df, minnrHH=4){
  
  tab <- table(df$Date, df$Ring)
  dats <- as.Date(rownames(tab)[apply(tab,1,function(x)all(x>minnrHH))])
  
  df <- subset(df, Date %in% dats)
  
  facegap <- summaryBy(Gapfraction1 + Gapfraction2 + Gapfraction3 ~ Date + Ring, 
                       data=df, FUN=c(mean,length,sd))
  
  facegap <- merge(facegap, eucface, by="Ring")
  return(facegap)
}




calibrateToDrought <- function(df){
  
  r <- make_dLAI_drought2013(df)
  
  lmfit <- lm(dLAI_litter ~ dLAI_PAR-1, data=r)
  
  return(list(calib=coef(lmfit)[[1]], lmfit=lmfit))
  
}



make_dLAI_drought2013 <- function(df, calib=1){
  
  # Calculate LAI.
  df <- df[!is.na(df$Gapfraction.mean),]
  df$LAI <- calib * with(df, mapply(LAI_Tdiff, Td=Gapfraction.mean))
  
  # diffuse transmittance after mid-July (data two weeks before stable and a bit noisy)
  df1 <- subset(df, Date > as.Date("2013-7-14") & Date < as.Date("2013-11-12"))
  
  # litter fall since early july
  df2 <- subset(litter, Date >= as.Date("2013-7-8") & Date < as.Date("2013-11-12"))

  fits <- smoothplot(Date, LAI, Ring, data=df1, plotit=FALSE)
  
  dats <- range(df1$Date)
  dLAIsm <- c()
  for(i in 1:6){
    dLAIsm[i] <- abs(diff(predict(fits[[i]], data.frame(X=as.numeric(dats), R=paste0("R",i)))))
  }
  
  lit <- subset(df2, Date  > min(Date))
  dLAIlit <- with(lit, tapply(dLAIlitter.mean, Ring, sum))
  return(data.frame(Ring=names(dLAIlit), dLAI_litter=dLAIlit, dLAI_PAR=dLAIsm))
}



calculate_LAI <- function(df,x=1, calib=1){
  
  # Add LAI estimate
  df$LAI <- calib * with(df, mapply(LAI_Tdiff, Td=Gapfraction.mean, x=x))
  
  return(df)
}



splitbydate <- function(dfr, datevec){
  
  datevec <- as.Date(datevec)
  
  l <- list()
  for(i in 2:length(datevec)){
    l[[i]] <- subset(dfr, Date >= datevec[i-1] & Date < datevec[i])
  }
  l[[1]] <- NULL
  
  return(l)
}


make_dLAI_litter <- function(dat, litter, kgam=15){
  
  # LAI by ring with smoother
  dat <- makesmoothLAI(dat, kgam=kgam, timestep="1 day")
  
  # Make LAI change, combine with litter fall
  litterDates <- sort(unique(litter$Date))
  dat <- lapply(dat, splitbydate, litterDates)
  
  # Get change in LAI for each inter-litter interval.
  getdlai <- function(x){
    do.call(rbind,lapply(x, function(z){
      n <- nrow(z)
      dLAI <- z$LAIsm[n] - z$LAIsm[1]
      d <- diff(z$LAIsm)
      dnegLAI <- sum(d[d < 0])
      dposLAI <- sum(d[d > 0])
      return(data.frame(dLAI=dLAI, dnegLAI=dnegLAI, dposLAI=dposLAI))
    }))
  }
  
  soilsp <- splitbydate(facesoilwater, litterDates) 
  meansw <- data.frame(Date=litterDates[1:(length(litterDates)-1)], 
                       VWC=sapply(soilsp, function(x)mean(x$VWC, na.rm=TRUE)))
  
  
  # r will be a dataframe with litterfall and change in LAI
  r <- list()
  for(i in 1:6){
    r[[i]] <- cbind(data.frame(Date=litterDates[2:length(litterDates)]), getdlai(dat[[i]]))
    r[[i]] <- merge(r[[i]], subset(litter, Ring == paste0("R",i)), by=c("Date"))
  }
  r <- do.call(rbind,r)
  
  # Absolute change in LAI
  r$absdLAI <- with(r, -dnegLAI + dposLAI)
  r$LAIchange <- as.factor(r$dLAI > 0)
  levels(r$LAIchange) <- c("decreasing","increasing")
  r <- merge(r, meansw)
  return(r)
}



analyzeFlat <- function(path, Date, pathout="", reProcess=FALSE, suffix="", ...){
  
  o <- getwd()
  on.exit(setwd(o))
  setwd(path)
  
  
  fk <- paste0("Filekey_",format(Date,"%Y_%m_%d"),".csv")
  fnout <- paste0(pathout, "FACE_flatcan_gapfraction_",format(Date,"%Y_%m_%d"),suffix,".csv")
  if(file.exists(fnout) && !reProcess){
    warning("Output file already exists; data not reanalyzed.")
    return(NULL)
  }
  
  if(!file.exists(fk))
    stop("Filekey does not exist (or wrong filename).")
  
  filekey <- read.csv(fk, stringsAsFactors=FALSE)
  names(filekey) <- tolower(names(filekey))
  
  filekey$filename <- paste0(filekey$file.name, ".JPG")
  ex <- file.exists(filekey$filename)
  
  if(sum(!ex) > 0){
    message("The following files do not exist:")
    print(filekey$filename[!ex])
  }
  
  filekey <- filekey[ex,]
  
  jpgfiles <- filekey$filename
  
  th <- gf <- c()
  
  for(j in 1:length(jpgfiles)){
    im <- readCanPhoto(jpgfiles[j])
    imt <- findThreshold(im, ...)
    
    th[j] <- imt$thresh
    gf[j] <- gapfraction(imt)
    message(j," of ",length(jpgfiles)," : ",round(gf[j],3))
  }
  filekey$threshold <- th
  filekey$gapfraction <- gf
  
  
  setwd(o)
  write.csv(filekey, fnout, row.names=FALSE)
  
  return(invisible(fnout))
}




get_rain <- function(how=c("daily", "raw", "rawmean")){
  
  how <- match.arg(how)
  
  # Rain.
  R1 <- downloadTOA5(filename="FACE_R1_T1_Rain")
  R3 <- downloadTOA5(filename="FACE_R3_T1_Rain")
  R4 <- downloadTOA5(filename="FACE_R4_T1_Rain")
  R1$Ring <- "R1"
  R3$Ring <- "R3"
  R4$Ring <- "R4"
  names(R3)[3] <- "Rain_mm_Tot"
  rain <- rbind(R1, R3, R4)

  # Data is 15-minutely, make 30min.
  rain <- as.data.frame(dplyr::summarize(group_by(rain,DateTime=nearestTimeStep(DateTime,30),Ring),
                                         Rain_mm_Tot=sum(Rain_mm_Tot),
                                         Source_rain=first(Source)))
  
  if(how == "daily"){
    rain$Date <- as.Date(rain$DateTime)
    rainagg <- summaryBy(Rain_mm_Tot ~ Ring + Date, FUN=sum, data=rain, keep.names=TRUE)
    names(rainagg)[3] <-"Rain"
    rainw <- reshape(rainagg, direction="wide", timevar="Ring", idvar="Date")
    
    ros <- downloadTOA5("ROS_WS_Table15")
    rosrain <- summaryBy(Rain_mm_Tot ~ Date, FUN=sum, data=ros, keep.names=TRUE)
    names(rosrain)[2] <- "Rain.ROS"
    
    rain <- merge(rosrain, rainw, all=TRUE)
  }
  
  if(how == "rawmean"){
    
    # Average across the three rings.
    rain <- as.data.frame(dplyr::summarize(group_by(rain, DateTime),
                                           Rain_mm_Tot = mean(Rain_mm_Tot, na.rm=TRUE)))
    
  }
  
  return(rain)
}


get_rosTair <- function(){
  
  d <- downloadTOA5(c("ROS_WS","Table05min"), maxnfiles=500)
  
  d <- as.data.frame(dplyr::summarize(group_by(d, Date),
                                      Tair=mean(AirTC_Avg, na.rm=TRUE)
                                      ))
return(d)  
}

get_soilwater <- function(how=c("mean","byring")){
  
  how <- match.arg(how)
  
  meanVWC <- function(dfr){
    vwccols <- grep("VWC_",names(dfr))
    dfr <- dfr[,vwccols]
    dfr[dfr > 1] <- NA
    rowMeans(dfr, na.rm=TRUE)
  }
  
  soilw <- downloadTOA5("SoilVars", maxnfiles=500)
  soilw$Ring <- as.factor(paste0("R",  str_extract(soilw$Source, "[0-9]")))
  
  if(how == "mean"){
    soilwd <- summaryBy(. ~ Date, FUN=mean, keep.names=TRUE, data=soilw, na.rm=TRUE)
    soilwd <- data.frame(Date=soilwd[,c("Date")], VWC=meanVWC(soilwd))
  } 
  if(how == "byring"){
  
    soilwd <- summaryBy(. ~ Ring + Date, FUN=mean, keep.names=TRUE, data=soilw, na.rm=TRUE)
    soilwd <- data.frame(Date=soilwd$Date, Ring=soilwd$Ring, VWC=meanVWC(soilwd))
    soilwd <- merge(soil, eucface())
  }
  
  return(soilwd)
}


getROSrain <- function(getnewdata=TRUE,
                       filename="output/data/ROS_rain_daily_all.csv"){
  
  if(getnewdata){
    
    rosmet <- downloadTOA5("ROS_WS_Table15min", maxnfiles=200)
    rosmet <- rosmet[!duplicated(rosmet$DateTime),]
    
    rosmet$Date <- as.Date(rosmet$DateTime)
    rosmet <- aggregate(Rain_mm_Tot ~ Date, FUN=sum, data=rosmet, na.rm=TRUE)
    names(rosmet)[2] <- "Rain_daily"
    
    write.csv(rosmet,filename,row.names=FALSE)
    
  } else {
    if(file.exists(filename)){
      rosmet <- read.csv(filename)
      rosmet$Date <- as.Date(rosmet$Date)
    } else {
      stop("File does not exist - first run with newdata=TRUE")
    }
  }
  return(rosmet)
}




makeCloudy <- function(df,
                       Fdiff_cutoff = 0.95,   # minimum diffuse fraction to include a halfhour
                       PARabovecutoff = 10,
                       PARbelowcutoff = 1400, # cannot have very high PAR when it is very cloudy
                       minSolarElevation = 10,
                       bysensor=FALSE){  
  
  
  
  # Add solar vars
  solVars <- get_solvars()
  
  df <- merge(df, solVars[,c("DateTime","sunelevation")], all.x=TRUE, all.y=FALSE)
  
  # Toss nighttime data. 
  df <- subset(df, sunelevation > minSolarElevation)
    
  df$PAR_Den_2_Avg[df$Ring == "R4" & df$Date == as.Date("2013-9-8")] <- NA
  
  # Fdiff , fraction diffuse radiation.
  df$Fdiff <- with(df, DiffuseSS / TotalSS)
  
  # 1. Make Fdiff for manually selected cloudydays, for period before eddy was recording data.
  addManualClouddays <- function(df,cd){
    
    df$hourtime <- hour(df$DateTime) + minute(df$DateTime)/60
    for(i in 1:nrow(cd)){
      cloudy <- df$Date == cd$Date[i] & df$hourtime >= cd$starthour[i] & df$hourtime <= cd$endhour[i]
      df$Fdiff[cloudy] <- 0.9999
    }
    return(df)
  }
  df <- addManualClouddays(df, cloudydays())
  
  #2. Select all half hours that are cloudy.
  df <- subset(df, Fdiff > Fdiff_cutoff & 
                        LI190SB_PAR_Den_Avg > PARabovecutoff & 
                        LI190SB_PAR_Den_Avg < PARbelowcutoff)
  
  # Subset where PARbelow is not zero (small subset, must be bad values)
  notneg <- function(x)is.na(x) || x > 0
  df <- subset(df, notneg(PAR_Den_1_Avg) & notneg(PAR_Den_2_Avg) & notneg(PAR_Den_3_Avg))
  
  if(!bysensor){
    # Calculate gapfraction
    df$PARbelow <- rowMeans(df[,c("PAR_Den_1_Avg","PAR_Den_2_Avg","PAR_Den_3_Avg")],
                                   na.rm=TRUE)
    df$Gapfraction <- with(df, PARbelow / LI190SB_PAR_Den_Avg)
    
  } else {
    df$Gapfraction1 <- with(df, PAR_Den_1_Avg / LI190SB_PAR_Den_Avg)
    df$Gapfraction2 <- with(df, PAR_Den_2_Avg / LI190SB_PAR_Den_Avg)
    df$Gapfraction3 <- with(df, PAR_Den_3_Avg / LI190SB_PAR_Den_Avg)
  }
  
  
  return(df)
}


make_litter <- function(filename="output/data/FACE_leaflitter_all.csv",
                       trapArea=0.1979,  # m2
                       SLA=43    # cm2 g-1
                       
){
  
  # Part 1
  dfr1 <- downloadCSV("FACE_P0017_RA_Litter_20121001-20131231-R.csv")[,1:9]
  
  # Part 2 - not on HIEv just yet.
  dfr2 <- read.csv("data/FACE_P0017_RA_Litter_20140101-20140228-R.csv")[,1:9]
  
  names(dfr1) <- names(dfr2)
  dfr <- rbind(dfr1,dfr2)
  
  names(dfr)[1] <- "Ring"
  dfr$Ring <- paste0("R", dfr$Ring)
  
  dfr$Date <- as.Date(dfr$Date)
  dfr <- merge(dfr, eucface())
  
  dfr$dLAIlitter <- with(dfr, (Leaf / trapArea) * SLA * 10^-4)
  
  # Average
  n <- function(x,...)length(x[!is.na(x)])
  litagg <- summaryBy(Leaf + dLAIlitter ~ Ring + Date, FUN=c(mean,sd,n),na.rm=TRUE, 
                      data=dfr, id=~treatment)
  
  litagg$n <- litagg$LEAF.n
  litagg$LEAF.n <- litagg$dLAIlitter.n <- NULL
  
  dats <- sort(unique(litagg$Date))
  datdf <- data.frame(Date=dats, ndays=c(NA,diff(dats)))
  litagg <- merge(litagg, datdf)
  
  return(litagg)
}

agglitter <- function(dfr){
  
  names(dfr) <- gsub(".mean","",names(dfr))
  se <- function(x)sd(x, na.rm=TRUE)/sqrt(length(x[!is.na(x)]))
  
  dfra <- summaryBy(Leaf + dLAIlitter ~ treatment + Date, FUN=c(mean,se), data=dfr)
  
  return(dfra)
}





makeFACEPAR <- function(uploadnew=FALSE){
  
  # Make monthly chunks and upload whole months to HIEv, if not exists already.
  if(uploadnew){
    makeAllMonthPAR()
    uploadPARAGGtoHIEv()
  }
  
  # Get previously compiled PARAGG from HIEv
  PARAGG <- downloadCSV("P0037_RA_PARAGG")
  # Delete _v1
  PARAGG <- PARAGG[!grepl("_v1", PARAGG$Source),]
  
  # Add more data
  newFACEPAR <- makePARAGG(startdate=max(as.Date(PARAGG$DateTime)))
  
  # Last row has NAs?
  newFACEPAR <- newFACEPAR[!is.na(newFACEPAR$DateTime),]
  
  # Combine, addDate
  FACE_PAR <- rbind_list(PARAGG, newFACEPAR)
  FACE_PAR$Date <- as.Date(FACE_PAR$DateTime)
  
  return(FACE_PAR)
}


# Functions to make 30 minutely radiation data at the EucFACE
makePARAGG <- function(startdate, enddate=as.Date(Sys.time())){
  
  addRing <- function(x)as.factor(str_extract(x$Source, "R[1-6]"))
  
  # AirVars
  airvars <- downloadTOA5(c("FACE","airvars"), startDate=startdate, endDate=enddate,
                          maxnfiles=200)
  airvars$Ring <- addRing(airvars)
  
  airvarsagg <- dplyr::summarize(group_by(airvars,DateTime=nearestTimeStep(DateTime,30),Ring),
                                 PAR_Den_1_Avg=mean(PAR_Den_1_Avg),
                                 PAR_Den_2_Avg=mean(PAR_Den_2_Avg),
                                 PAR_Den_3_Avg=mean(PAR_Den_3_Avg),
                                 Source_airvars=first(Source))
  
  # General
  general <- downloadTOA5(c("FACE","general"), startDate=startdate, endDate=enddate,
                          maxnfiles=200)
  general$Ring <- addRing(general)
  
  generalagg <- dplyr::summarize(group_by(general,DateTime=nearestTimeStep(DateTime,30),Ring),
                                 LI190SB_PAR_Den_Avg=mean(LI190SB_PAR_Den_Avg),
                                 SlrW_Avg=mean(SlrW_Avg),
                                 Source_general=first(Source))
  
  # Eddy
  eddy <- downloadTOA5("Eddyflux_slow_met", startDate=startdate, endDate=enddate,
                       maxnfiles=200, allowedExtensions=c(".dat"))
  
  # glitch fix, hopefully remove soon
  eddysept <- readTOA5("c:/hievdata/EddyFlux_slow_met_20140930.dat")
  eddy <- rbind(eddy, eddysept)
  
  # don't actually have to aggregate; but this fixes duplicate entries quickly and safely
  eddyagg <- dplyr::summarize(group_by(eddy,DateTime=nearestTimeStep(DateTime,30)),
                              TotalSS=mean(TotalSS),
                              DiffuseSS=mean(DiffuseSS),
                              PAR_Den=mean(PAR_Den),
                              Source_eddy=first(Source))
  
  FACE_PAR <- merge(airvarsagg, generalagg, by=c("DateTime","Ring"), all=TRUE)
  FACE_PAR <- merge(FACE_PAR, eddyagg, by="DateTime", all=TRUE)
  
  
  # Remove bad data
  badpar <- badPARdata()
  for(i in 1:nrow(badpar)){
    x <- badpar[i,]
    dats <- seq.Date(x$startDate, x$endDate, by="1 day")
    d <- as.Date(FACE_PAR$DateTime)
    FACE_PAR[d %in% dats & as.character(FACE_PAR$Ring) == x$Ring, x$Sensor] <- NA
  }

  
  return(FACE_PAR)
}


# Make monthly chunk
makeMonthPAR <- function(month, overwrite=FALSE){
  
  enddate <- seq.Date(as.Date(month),length=2,by="1 month")[2] - 1
  fn <- paste0("output/data/paragg/FACE_P0037_RA_PARAGG_",as.Date(month),"_",enddate,".csv")
  
  if(file.exists(fn) && !overwrite){
    message("PARAGG : ", fn, " already exists.")
    return()
  }
  
  FACE_PAR <- makePARAGG(startdate=month, enddate=enddate)
  
  message(fn, "\ndone")
  write.csv(FACE_PAR,fn,row.names=FALSE)
}

# Make all monthly chunks; do not do current month.
makeAllMonthPAR <- function(){
  mbegin <- seq.Date(from=as.Date("2013-7-1"), to=floor_date(today(), "month"), by="1 month")
  for(i in 1:(length(mbegin)-1))makeMonthPAR(mbegin[i])
}


# Upload all monthly PARAGG to HIEv unless already on HIEv.
uploadPARAGGtoHIEv <- function(){
  
  fns <- dir("output/data/paragg", full.names=TRUE)
  for(i in 1:length(fns)){
    
    if(!is.null(searchHIEv(basename(fns[i]), quiet=TRUE))){
      message("File ", basename(fns[i])," is on HIEv, skipping.")
      next
    }
    
    HIEv:::uploadToHIEv(fns[i], experiment=39, description=readLines("docs/faceparmetadata.txt"))
    
  }
}




makesmoothLAI <- function(dat, timestep="3 days", kgam=15, how=c("byring","mean")){
  
  how <- match.arg(how)
  
  if(how == "mean"){
    
    x <- dat
    x <- x[order(x$Date),]
    gamfit <- smoothplot(as.numeric(Date),LAI,data=x,kgam=kgam, plotit=FALSE)
    
    dfr <- data.frame(X=as.numeric(seq.Date(min(x$Date), max(x$Date), by=timestep)))
    dfr$LAIsmooth <- predict(gamfit[[1]],dfr)
    names(dfr)[1] <- "Date"
    dfr$Date <- as.Date(dfr$Date, origin="1970-1-1")
    
    dfr$dLAI <- c(NA, diff(dfr$LAIsmooth))
    dfr$ndays <- c(NA, diff(dfr$Date))
    return(dfr)
    
  }
  
  if(how == "byring"){
    rings <- split(dat, dat$Ring)
    smoothlai <- lapply(rings, function(x){
      
      x <- x[order(x$Date),]
      
      gamfit <- smoothplot(as.numeric(Date),LAI,data=x,kgam=kgam, plotit=FALSE)
      
      dfr <- data.frame(X=as.numeric(seq.Date(min(x$Date), max(x$Date), by=timestep)))
      dfr$LAIsmooth <- predict(gamfit[[1]],dfr)
      names(dfr)[1] <- "Date"
      dfr$Date <- as.Date(dfr$Date, origin="1970-1-1")
      
      dfr$dLAI <- c(NA, diff(dfr$LAIsmooth))
      dfr$ndays <- c(NA, diff(dfr$Date))
      return(dfr)
    })
  }
  
  return(smoothlai)
}



get_hawkrain <- function(){
  
  rain <- read.csv("data/IDCJAC0009_067021_1800_Data.csv")[,3:6]
  rain$Date <- as.Date(with(rain, ISOdate(Year,Month,Day)))
  
  names(rain)[4] <- "Rain"
return(rain)
}





