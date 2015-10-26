
fn <- "output/FACE_RA_P0037_GAPFRACLAI_20121026-20150225_L2.csv"
write.csv(facegap_cloudy_byring, fn, row.names=FALSE)
HIEv:::uploadToHIEv(fn, experiment_id=39, description=readLines("docs/gapfraclaimetadata.txt"))

fn <- "output/FACE_RA_P0037_PHOTOGAPFRAC_20121020-20131022_L2.csv"
write.csv(flatcan, fn, row.names=FALSE)
HIEv:::uploadToHIEv(fn, experiment_id=39, description=readLines("docs/photogapfracmetadata.txt"))


fn <- "output/FACE_RA_P0037_DAILYMET_20110619-20151026_L2.csv"
write.csv(simplemet, fn, row.names=FALSE)
HIEv:::uploadToHIEv(fn, experiment_id=39, description=readLines("docs/simplemetmetadata.txt"))

