
library(lme4)
library(lmerTest)
library(car)

# leaf lifespan Ca effect
with(ba, t.test(LL ~ treatment, var.equal=TRUE))


# Non-significance of Ca on LAI, litter
df <- litter
df$Time <- as.factor(df$Date - min(df$Date))
m1 <- lmer(Leaf.mean ~ treatment*Time + (1|Ring), data=df)
Anova(m1)

# LAI
df <- facegap_cloudy_byring
df$Time <- as.factor(df$Date - min(df$Date))
m2 <- lmer(LAI ~ treatment*Time + (1|Ring), data=df)
Anova(m2)

# LAI anomaly

# LAI production
df <- dLAIlitter
df$Time <- as.factor(df$Date - min(df$Date))
df$leafprod <- with(df, dLAI + dLAIlitter.mean)
m4 <- lmer(leafprod ~ treatment*Time + (1|Ring), data=df)
Anova(m4)

# photos vs. PARLAI
lmphoto <- lm(LAI.PAR.mean ~ LAI, data=flatcan_byring)
summary(lmphoto)


# litter production and LAI
lmlit <- lm(LAIlitter_annual ~ LAI.mean-1, data=ba)



# Average CI width in Fig 4.
fit <- smoothplot(Date, LAI, g=treatment, R="Ring", 
           data=facegap_cloudy_byring, plot=FALSE)
p <- predict(fit$ambient, data.frame(X=as.numeric(facegap_cloudy_byring$Date)), se.fit=TRUE)
mean(p$se.fit)*2
p <- predict(fit$elevated, data.frame(X=as.numeric(facegap_cloudy_byring$Date)), se.fit=TRUE)
mean(p$se.fit)*2

# Average CI width in Fig 4.
fit <- smoothplot(Date, LAIanomaly, g=treatment, R="Ring", 
                  data=facegap_cloudy_byring, plot=FALSE)
p <- predict(fit$ambient, data.frame(X=as.numeric(facegap_cloudy_byring$Date)), se.fit=TRUE)
mean(p$se.fit)*2
p <- predict(fit$elevated, data.frame(X=as.numeric(facegap_cloudy_byring$Date)), se.fit=TRUE)
mean(p$se.fit)*2



# rainfall stats
# ROS rain
d <- subset(faceraindaily, Date > as.Date("2013-7-1") & Date < as.Date("2013-11-1"))
sum(d$Rain.ROS)

# Hawk rain same period (OK!)
d2 <- subset(hawkrain, Date > as.Date("2013-7-1") & Date < as.Date("2013-11-1"))
sum(d2$Rain, na.rm=TRUE)

# Hawk long-term averages.
hawkrain <- get_hawkrain()
dh <- subset(hawkrain, Month %in% 7:10)
n <- function(x,...)length(x[!is.na(x)])
map <- summaryBy(Rain ~ Year, FUN=c(sum,n), data=dh)
map <- subset(map, Rain.n > 0)

# 'second driest period ever recorded'
head(sort(map$Rain.sum))

mean(map$Rain.sum, na.rm=T)
range(map$Rain.sum, na.rm=T)



# regression LAI - BA
balm <- lm(LAI.mean ~ BA, data=ba)

# litter - LA
litlm <- lm(LAIlitter_annual ~ LAI.mean-1, data=ba)




#--------------------------------

df <- facegap_cloudy_byring
df$Time <- as.numeric(df$Date - min(df$Date))

d <- split(df, df$Ring)

fits <- lapply(d, function(x)gam(LAI ~ s(Time, k=21), data=x))

for(i in 1:6){
  d[[i]]$LAIpred <- predict(fits[[i]], d[[i]])
}
dat <- do.call(rbind,d)

with(dat, plot(LAI, LAIpred))
abline(0,1)




               
               
               




