

par(cex.axis=0.8, mar=c(3,5,2,5), las=1, cex.lab=1.2, yaxs="i")
to.pdf(smoothplot(Date, LAI, g=treatment, R="Ring", 
           data=facegap_cloudy_byring, kgam=18, pointcols=c("blue","red"), 
           linecols=c("blue","red")), 
       filename="LAI_smoothed_withCI.pdf", width=8, height=4)

to.pdf(smoothplot(Date, LAIanomaly, g=treatment, R="Ring", 
           data=facegap_cloudy_byring, kgam=18, pointcols=c("blue","red"), 
           linecols=c("blue","red")), 
        filename="LAIanomaly_smoothed_withCI.pdf", width=8, height=4)
