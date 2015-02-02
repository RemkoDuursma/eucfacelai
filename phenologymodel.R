
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





saveRDS(soilwd, "soilwd.rds")


library(zoo)

# drought summation, similar to water stress integral.
# maybe should instead integrate over past with decreasing weights as done by Knorr et al. 2010.
swmin <- 0.05
memory <- 60
soilwd$r <- rollapply(soilwd$VWC, width=memory, FUN=function(x)sum(pmax(0, swmin - x)),
               fill=NA, align="right")

with(soilwd, plot(Date, VWC, type='l'))
par(new=TRUE)
with(soilwd, plot(Date, r, type='l', col="red", ann=F))
