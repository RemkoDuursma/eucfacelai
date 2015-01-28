

library(HIEv)
setToPath("cache")

gettime <- function(x){  
  as.POSIXct(str_extract(x,"[0-9]{8}-[0-9]{2}-[0-9]{2}-[0-9]{2}"),
             format="%Y%m%d-%H-%M-%S", tz="UTC")
}

key <- downloadCSV("FACE_RA_SECURPHOT-KEY_20150123_L1.csv")

fns <- dir("c:/data/eucface/securecam/images", recursive=TRUE, full.names=TRUE)


res <- list()
for(i in 1:24){
  
  lab <- gsub(" ","-", key[i,"Label"])
  ring <- paste0("FACE_R",key[i,"Ring"])
  
  img_fns <- intersect(fns[grep(lab, fns)], fns[grep(ring,fns)])
  img <- setImages(fns=img_fns, getTimeFun=gettime)
  
  res[[i]] <- processImages(img, ROI=c(0,1,0,1))
}
df <- do.call(rbind,res)

df$Ring <- rep(key$Ring[1:24], each=sum(sapply(res,nrow))/24)
df$Label <- rep(key$Label[1:24], each=sum(sapply(res,nrow))/24)


library(RColorBrewer)

greens <- brewer.pal(8,"Greens")[5:8]

palette(greens)
with(subset(df, Ring == 6), plot(Date, GCC, pch=19, col=as.factor(Label)))


windows()
par(mfrow=c(3,2))
for(i in 1:6){
  smoothplot(Date,GCC, Label,pointcols=greens, linecols=greens, data=subset(df,Ring==i), k=10)
  plotlabel(paste("Ring",i),"topleft")
}


pdf("tmp.pdf")
for(i in 1:6){
  smoothplot(Date,GCC, Label,pointcols=greens, linecols=greens, data=subset(df,Ring==i), k=10,
             xlim=as.Date(c("2014-11-1","2015-1-30")))
  par(new=TRUE)
  smoothplot(Date, Gapfraction.mean, 
             data=subset(facegap_all_byring, Ring ==paste0("R",i) & Date > as.Date("2014-11-1")) ,
            axes=FALSE,ylab="",linecols="red", pointcols="red",k=5,
            xlim=as.Date(c("2014-11-1","2015-1-30")))
  axis(4)
}
dev.off()


#------------------------------------------------------------------------------------------#
# recent ring 6 data, one view only

setToPath("cache")

f <- searchHIEv("FACE_R6_P0037_SECURPHOT-Wide-view-right-down")
d <- downloadHIEv(f, maxnfiles=500)

fn <- file.path(getToPath(),d)

library(phenora)

gettime <- function(x){  
  as.POSIXct(str_extract(x,"[0-9]{8}-[0-9]{2}-[0-9]{2}-[0-9]{2}"),
             format="%Y%m%d-%H-%M-%S", tz="UTC")
}

r6 <- setImages(fns=fn, getTimeFun=gettime)

selectROI(r6,1)

rois <- list(bottom=c(0.0023148152334374 ,0.608145364609622 ,0.006172589448972 ,0.449845509191133),
             topleft=c(0.013165511640175 ,0.253689281989526 ,0.356609895622128 ,0.88708838661819),
             topright=c(0.450810266711926 ,0.684100239456785 ,0.42734036108827 ,0.861368217357775))

df <- processImages(r6,rois)
