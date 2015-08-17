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


write.csv(lerplai, "output/data/lerplai.csv", row.names=FALSE)



# figures
palette(c("forestgreen","darkorange","royalblue"))
pchs <- c(19,15,17)

with(lerplai, plot(dLAIlitter.mean, log10(abundance_m2), col=as.factor(species),
                   pch=pchs[species]))

with(lerplai, plot(dLAIlitter.mean_prev, log10(abundance_m2), col=as.factor(species),
                   pch=pchs[species]))


windows(7,7)
par(mfrow=c(3,1), mar=c(0,0,0,0), oma=c(5,5,2,2))
with(lerplai, plot(Date, leafgrowth))
with(lerplai, plot(Date, dLAIlitter.mean))
with(lerplai, plot(Date, log10(abundance_30days), pch=19, col=species))









