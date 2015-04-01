

litring <- make_litter(SLA=52.6,what="byring")
df2 <- subset(litring, Date >= as.Date("2013-7-8") & Date < as.Date("2013-11-12"))

dfa <- summaryBy(dLAIlitter ~ Ring + Trap, FUN=sum, na.rm=TRUE, data=df2)

se <- function(x)sd(x)/sqrt(length(x))
dft <- summaryBy(dLAIlitter.sum ~ Ring, FUN=c(mean,se), data=dfa)

anova(lm(dLAIlitter.sum ~ Ring, data=dfa))




df <- facegap_cloudy_byring
df1 <- subset(df, Date > as.Date("2013-7-14") & Date < as.Date("2013-11-12"))

df1$numDate <- with(df1, as.numeric(Date - min(Date)))
df1$taud_0 <- with(df1, ave(Gapfraction.mean, Ring, FUN=function(x)x[which.min(Date)]))
df1$deltagapfrac <- with(df1, Gapfraction.mean - taud_0)

# with(df1, plot(Date, Gapfraction.mean, pch=19, col=my_ringcols()))
# 
# 
# library(plotBy)
# plotBy(Gapfraction.mean ~ as.numeric(Date-min(Date))|Ring, 
#        enhance="lm", data=df1)

lm0 <- lm(deltagapfrac ~ Ring + numDate, data=df1)
lmtaud <- lm(deltagapfrac ~ Ring + Ring:numDate,
             data=df1)



lms <- lapply(split(df1, df1$Ring),
              function(x)lm(Gapfraction.mean ~ as.numeric(Date-min(Date)),
                            data=x))

delta_gapfr_mu <- ndays * sapply(lms, coef)[2,]

ndays <- as.numeric(max(df1$Date) - min(df1$Date))

cis <- sapply(lms, confint, 2)
delta_gapfr_ci <- cis*ndays


library(gplots)


# sort, ambient left, elevated right
ind <- c(2,3,6,1,4,5)

windows(5,8)
par(mfrow=c(2,1), mar=c(0,0,0,0),
    oma=c(5,5,1,1), cex.lab=1.2)
plotCI(1:6, delta_gapfr_mu[ind], 
       ui=delta_gapfr_ci[2,][ind],
       li=delta_gapfr_ci[1,][ind],
       col=my_ringcols()[ind], pch=15,
       ylim=c(0,0.2), 
       axes=FALSE)
axis(1, labels=FALSE)
axis(2)
box()

plotCI(1:6, dft$dLAIlitter.sum.mean[ind], 
       uiw=2*dft$dLAIlitter.sum.se[ind],
       col=my_ringcols()[ind], pch=15,
       axes=FALSE,
       ylim=c(0,1.1))
axis(1, at=1:6, labels=as.character(ind))
axis(2)
box()
mtext(side=2, at=0.75, text=expression(Delta*tau[d]), line=3, outer=TRUE,
      cex=1.2)
mtext(side=2, at=0.25, text=expression(Litter~production~(m^2~m^-2)),
      line=3, outer=TRUE, cex=1.2)
mtext(side=1, at=0.5, text="Ring", outer=TRUE, line=3, cex=1.2)







