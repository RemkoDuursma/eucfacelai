
# - passive litter production should be function of moving average soil water,
# avoiding immediate drop when soil water briefly dips below some threshold.
# - leaf production should also have a smoothing term, avoiding immediate leaf growth when 
# soil water has increased above some threshold.
# - active litter production should be proportional to leaf production (coefficient can be 
# taken from regression of dLAIdt vs. Litter prod.
# - LAImax is a parameter, leaf production a function of LAI/LAImax avoiding further leaf
# growth when LAI = LAImax.
# - threshold for leaf growth can be something like:
# curve(pnorm(x, mean=0.18, sd=0.01), from=0, to=0.5)
# where x is sum(max(0, swmin-sw)) over X previous days.
# parameters:
# - 'wet' passive litter production
# - increase of passive litter production below drought threshold
# - drought threshold for litter production
# - LAImax
# - threshold for growth and SD (which determines smoothness)
# - active shedding fraction of leaf production rate.





soilwd <- facesoilwater

library(zoo)

# Measured LAI
dfa <- summaryBy(LAI ~ Date, data=facegap_cloudy_byring, FUN=mean, keep.names=TRUE)
startDate <- min(dfa$Date)




LAImodel <- function(swmin=0.05, memorydry=30, wetthresh=0.18, memorywet=15, 
                     turnoverbase=0.05/30, turnoverdry=0.15/30,
                     flushrate = 0.0085, LAImax =2, LAI0=1.0,
                     plotit=TRUE){
  
  # drought summation, similar to water stress integral.
  # maybe should instead integrate over past with decreasing weights as done by Knorr et al. 2010.
  soilwd$dryness <- rollapply(soilwd$VWC, width=memorydry, FUN=function(x)mean(pmax(0, swmin - x)),
                 fill=0, align="right")
  
  
  # 'it being wet' summation
  soilwd$wetness <- rollapply(soilwd$VWC, width=memorywet, FUN=function(x)mean(pmax(0, x - wetthresh)),
                        fill=1, align="right")
  
  
  # turn into 0,1 switch for leaf flushing
  soilwd$flushindex <- 1-exp(-50*soilwd$wetness)

  # convert dryness into litter fall rate
  soilwd$turnover <- turnoverbase + (turnoverdry - turnoverbase) * pnorm(soilwd$dryness, mean=0, sd=0.001)
   
  # ceiling effect of reaching maxLAI
  fmaxLAI <- function(LAI, maxLAI=LAImax){
    z <- 1-LAI/maxLAI
    pmax(0, 1-exp(-50*z))
  }
  
  soilwd <- subset(soilwd, Date >= startDate)
  
  LAI <- c()
  LAI[1] <- LAI0
  vwc <- soilwd$VWC
  turnover <- soilwd$turnover
  dryness <- soilwd$dryness
  flushindex <- soilwd$flushindex
  
  
  for(i in 2:length(vwc)){
      
    LAIincrease <- flushrate * flushindex[i-1] * fmaxLAI(LAI[i-1])
    LAIdecrease <- turnover[i-1]
    
    LAI[i] <- LAI[i-1] + LAIincrease - LAIdecrease
  }
  
  
  if(plotit){
    plot(soilwd$Date, LAI, type='l', ylim=c(0,2))
    points(dfa$Date, dfa$LAI)
  }
  
  m <- data.frame(Date=soilwd$Date, LAImodel=LAI)
  d <- merge(dfa, m)
  
  O <- sum((d$LAI - d$LAImodel)^2)
return(invisible(O))
}



laiwrap <- function(pars){
  
  LAImodel(turnoverbase=pars[1], turnoverdry=pars[2], 
           flushrate=pars[3],
           plotit=TRUE)
  
  
}
opt <- optim(c(0.05/30, 0.15/30, 0.0085), laiwrap)

LAImodel(turnoverbase=opt$par[1], turnoverdry=opt$par[2], flushrate=opt$par[3])



#-----


with(soilwd, plot(Date, VWC, type='l'))
par(new=TRUE)
with(soilwd, plot(Date, wetness, type='l', col="blue", ann=F, axes=FALSE))
axis(4)

with(soilwd, plot(Date, VWC, type='l'))
par(new=TRUE)
with(soilwd, plot(Date, dryness, type='l', col="red", ann=F, axes=FALSE))
axis(4)



