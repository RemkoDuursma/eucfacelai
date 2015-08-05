
#' This script is called by Manuscript.Rmd, and includes additional calculations 
#' used in the manuscript.
#

source("R/utils.R")  # Library()

Library(knitr)
Library(broom)
Library(lme4)
Library(lmerTest)
Library(car)
Library(reporttools)

# Load analysis cache.
load("cache/lai_workspace.RData")

# Trim data to this date (note: has to be consistent with make_figures.R!) 
maxdate <- as.Date("2015-3-1")

# More packages, functions, etc.
source("load.R")

# Functions for figures
source("R/functions-figures.R")
source("R/figures.R")

# CI on GAM derivative (Gavin Simpson)
source("R/derivSimulCI.R")


# shortcut to formatPval
pval <- function(...)formatPval(..., includeEquality=TRUE)

# Number of digits in printing of numbers
# options(scipen = 1, digits = 3)

# Shortcut to significant digits formatting
f <- function(x, digits=2)sprintf(x, fmt=paste0("%.",digits,"f"))

# Some definitirons of constants
.gambase <- 18
.maxSD <- 0.03

# Main results
df <- subset(facegap_cloudy_byring, Date < maxdate)

# Calibration constant to litter fall.
caliblm <- calibrateToDrought(df)$lmfit

# Number of months of study
r <- range(df$Date)
r[2] <- maxdate
nummon <- as.numeric(floor(difftime(r[2],r[1],units="weeks")/(52/12)))

# Range mean LAI
dfa <- summaryBy(LAI ~ Date, FUN=mean, na.rm=TRUE, data=df)
rangeLAI <- round(range(dfa$LAI),1)

# LAI ~ BA
balm <- lm(LAI.mean ~ BA, data=ba)

# Comparison flat canopy photo with transmittance
lmphoto <- lm(LAI.PAR.mean ~ LAI, data=flatcan_byring)

# Significance CO2 on LAI anomaly
df$Time <- as.factor(df$Date - min(df$Date))
m3 <- lmer(LAIanomaly ~ treatment*Time + (1|Ring), data=df)
LanomAOV <- Anova(m3)

# significance of LAI when using BA as actual covariate
df2 <- merge(df, ba[,c("Ring","BA")])
df2 <- subset(df2, Date < as.Date("2014-9-1"))
m4 <- lmer(LAI ~ treatment * as.factor(Date) + BA + (1|Ring), data=df2)
lmer_LAI <- Anova(m4, test="F")
rownames(lmer_LAI) <- c("C~a~","Date","BA","C~a~ x Date")
colnames(lmer_LAI) <- c("F","numDF","denDF","p-value")


# Average CI width in Fig 4.
fit <- smoothplot(Date, LAIanomaly, g=treatment, R="Ring", 
                  data=facegap_cloudy_byring, plot=FALSE, kgam=.gambase)
pa <- predict(fit$ambient, data.frame(X=as.numeric(facegap_cloudy_byring$Date)), se.fit=TRUE)
pe <- predict(fit$elevated, data.frame(X=as.numeric(facegap_cloudy_byring$Date)), se.fit=TRUE)
LanomCIamb <- mean(pa$se.fit)*2
LanomCIele <- mean(pe$se.fit)*2

# Detectable Ca effect on LAI
Ldetect <- 100 * (LanomCIamb + LanomCIele) / mean(ba$LAI.mean)

# Litter - dLAI correlations Figure 4
dl <- dLAIlitter
dl$Y <- with(dl, dLAI * 30.5/ndays)
dl$X <- with(dl, dLAIlitter.mean * 30.5/ndays)
dlup <- dl[dl$LAIchange == "increasing",]
dldown <- dl[dl$LAIchange == "decreasing",]
lmlitup <- lm(Y ~ X, data=dlup)
lmlitdown <- lm(Y ~ X, data=dldown)

# Leaf lifespan
LLtest <- tidy(t.test(LL ~ treatment,data=ba))

# Canopy photo - Ltau comparison
lmcanphot <- lm(LAI.PAR.mean ~ LAI, data=flatcan_byring)

# With optimized k
flatcan_byring2 <- agg_flatcan(flatcan, by="Ring", k=kopt)
flatcan_byring2 <- add_PARLAI_to_flatcan(facegap_cloudy_byring,flatcan_byring2)
lmcanphot2 <- lm(LAI.PAR.mean ~ LAI, data=flatcan_byring2)



