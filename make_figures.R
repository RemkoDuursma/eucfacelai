
source("R/functions-figures.R")
source("R/figures.R")
source("R/derivSimulCI.R")


maxdate <- as.Date("2015-3-1")
# maxdate <- as.Date(today())

to.pdf(figure1(subset(facegap_cloudy_byring,Date < maxdate), ramp),
       filename="output/figures/Figure1.pdf",
       width=8, height=6)

to.pdf(figure2(ba), 
       filename="output/figures/Figure2.pdf",
       width=4, height=4)

to.pdf(figure3(subset(facegap_cloudy_byring, Date < maxdate)),
       filename="output/figures/Figure3.pdf",
       width=8, height=4)

to.pdf(figure4(dLAIlitter),
       filename="output/figures/Figure4.pdf",
       width=6, height=4)

to.pdf(figure5(dLAIlitter),
       filename="output/figures/Figure5.pdf",
       width=8, height=4)

to.pdf(figure6(ba), 
       filename="output/figures/Figure6.pdf",
       width=8, height=4)

to.pdf(figure7(subset(facegap_cloudy_byring, Date < maxdate),
               subset(facesoilwater, Date < maxdate),
               subset(faceraindaily, Date < maxdate),
               subset(airt, Date < maxdate)),
       filename="output/figures/Figure7.pdf",
       width=10, height=8)

to.pdf(figureSI1(litring, subset(facegap_cloudy_byring, Date < maxdate)),
       filename="output/figures/FigureSI1.pdf",
       width=5, height=8)

to.pdf(figureSI2(flatcan_byring),
       filename="output/figures/FigureSI2.pdf",
       width=8, height=4)

to.pdf(figureSI3(subset(facegap_cloudy_byring, Date < maxdate), subset(facegap_all_byring, Date < maxdate)),
       filename="output/figures/FigureSI3.pdf",
       width=8, height=5)


