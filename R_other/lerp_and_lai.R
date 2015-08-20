# data
lerp <- read.csv("data/lerp_count.csv", stringsAsFactors = FALSE)
lerp$ring <- paste0("R", lerp$ring)
names(lerp)[1] <- "Ring"
names(lerp)[4] <- "treatment"

litdf <- dLAIlitter[,c("Date","Ring","LAI","dLAI","dLAIlitter.mean")]
litdf$date <- format(litdf$Date, "%b-%y")

lerplai <- merge(litdf, lerp)
lerplai$species <- as.factor(lerplai$species)


litprev <- do.call(rbind, lapply(split(litdf, litdf$Ring), function(x){
  x <- x[order(x$Date),]
  y <- x[c(1,1:(nrow(x)-1)),]
  y[1,3:5] <- NA
  y$Date <- x$Date
  y$date <- NULL
  
  names(y)[3:5] <- paste(names(y)[3:5], "prev", sep="_")
  
  return(y)
}))

lerplai <- merge(lerplai, litprev)

lerplai$leafgrowth <- with(lerplai, dLAI + dLAIlitter.mean)
lerplai$leafgrowth_prev <- with(lerplai, dLAI_prev + dLAIlitter.mean_prev)

write.csv(lerplai, "output/data/lerplai.csv", row.names=FALSE)



# figures
palette(c("forestgreen","darkorange","royalblue"))
pchs <- c(19,15,17)

with(lerplai, plot(dLAIlitter.mean, log10(abundance_m2), col=as.factor(species),
                   pch=pchs[species]))

with(lerplai, plot(dLAIlitter.mean_prev, log10(abundance_m2), col=as.factor(species),
                   pch=pchs[species]))



speccols <- c("forestgreen","darkorange","royalblue")
treatcols <- c("blue","red")
lerplai$treatment <- as.factor(lerplai$treatment)

windows(7,9)
par(mfrow=c(4,1), mar=c(0,0,0,0), oma=c(5,5,2,2))
with(lerplai, plot(Date, LAI, pch=19, col=treatcols[treatment], axes=FALSE))
timeseries_axis(labels=FALSE)
axis(2)
box()
CO2legend("topleft")
with(lerplai, plot(Date, leafgrowth, pch=19, col=treatcols[treatment], axes=FALSE))
timeseries_axis(labels=FALSE)
axis(2)
box()
with(lerplai, plot(Date, dLAIlitter.mean,pch=19, col=treatcols[treatment], axes=FALSE))
timeseries_axis(labels=FALSE)
axis(2)
box()
with(lerplai, plot(Date, log10(abundance_30days), pch=15, col=speccols[species],
                   axes=FALSE))
timeseries_axis(labels=TRUE)
axis(2)
box()
legend("topleft", levels(lerplai$species), col=speccols, pch=15, cex=0.8)

mtext(side=2, line=3, at=0.125, text="log10 Abundance", outer=TRUE)
mtext(side=2, line=3, at=0.375, text=expression(Litter~fall~(m^2~m^-2~mon^-1)), outer=TRUE)
mtext(side=2, line=3, at=0.625, text=expression(Leaf~growth~(m^2~m^-2~mon^-1)), outer=TRUE)
mtext(side=2, line=3, at=0.875, text=expression(LAI~(m^2~m^-2)), outer=TRUE)
dev.copy2pdf(file="output/abundance_LAI_litter_timeseries.pdf")



windows(7,9)
par(mfrow=c(4,1), mar=c(0,0,0,0), oma=c(5,5,2,2))
with(lerplai, plot(Date, LAI_prev, pch=19, col=treatcols[treatment], axes=FALSE))
timeseries_axis(labels=FALSE)
axis(2)
box()
CO2legend("topleft")
with(lerplai, plot(Date, leafgrowth_prev, pch=19, col=treatcols[treatment], axes=FALSE))
timeseries_axis(labels=FALSE)
axis(2)
box()
with(lerplai, plot(Date, dLAIlitter.mean_prev,pch=19, col=treatcols[treatment], axes=FALSE))
timeseries_axis(labels=FALSE)
axis(2)
box()
with(lerplai, plot(Date, log10(abundance_30days), pch=15, col=speccols[species],
                   axes=FALSE))
timeseries_axis(labels=TRUE)
axis(2)
box()
legend("topleft", levels(lerplai$species), col=speccols, pch=15, cex=0.8)

mtext(side=2, line=3, at=0.125, text="log10 Abundance", outer=TRUE)
mtext(side=2, line=3, at=0.375, text=expression(Litter~fall~(m^2~m^-2~mon^-1)), outer=TRUE)
mtext(side=2, line=3, at=0.625, text=expression(Leaf~growth~(m^2~m^-2~mon^-1)), outer=TRUE)
mtext(side=2, line=3, at=0.875, text=expression(LAI~(m^2~m^-2)), outer=TRUE)
dev.copy2pdf(file="output/abundance_LAI_litter_timeseries_prevmonth.pdf")

