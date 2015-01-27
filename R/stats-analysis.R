
# Non-significance of Ca on LAI, litter
df <- litter
df$Time <- as.factor(df$Date - min(df$Date))
m1 <- lmer(Leaf.mean ~ treatment*Time + (1|Ring), data=df)
Anova(m1)


df <- facegap_cloudy_byring
df$Time <- as.factor(df$Date - min(df$Date))
m2 <- lmer(LAI ~ treatment*Time + (1|Ring), data=df)
Anova(m2)


df <- dLAIlitter
df$Time <- as.factor(df$Date - min(df$Date))
df$leafprod <- with(df, dLAI + dLAI.mean)
m3 <- lmer(leafprod ~ treatment*Time + (1|Ring), data=df)
Anova(m3)



# Average CI width in Fig 4.
fit <- smoothplot(Date, LAI, g=treatment, R="Ring", 
           data=facegap_cloudy_byring, plot=FALSE)
p <- predict(fit$ambient, data.frame(X=as.numeric(facegap_cloudy_byring$Date)), se.fit=TRUE)
mean(p$se.fit)*2
p <- predict(fit$elevated, data.frame(X=as.numeric(facegap_cloudy_byring$Date)), se.fit=TRUE)
mean(p$se.fit)*2

