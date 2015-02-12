
source("R/functions-figures.R")
source("R/figures.R")
source("R/derivFun.R")

to.pdf(figure1(facegap_cloudy_byring, ramp),
       filename="output/figures/Figure1.pdf",
       width=8, height=6)


to.pdf(figure3(flatcan_byring),
       filename="output/figures/Figure3.pdf",
       width=5, height=5)

to.pdf(figure4(facegap_cloudy_byCO2,flatcan_byCO2, adddata="litter"),
       filename="output/figures/Figure4.pdf",
       width=8, height=4)

to.pdf(figure4_anomaly(facegap_cloudy_byring),
       filename="output/figures/Figure4_anomaly.pdf",
       width=8, height=4)


to.pdf(figure5(dLAIlitter),
       filename="output/figures/Figure5.pdf",
       width=6, height=4)

to.pdf(figure6(facegap_cloudy_byring),
       filename="output/figures/Figure6.pdf",
       width=10, height=8)

to.pdf(figureSI1(facegap_cloudy_byring, facegap_all_byring),
       filename="output/figures/FigureSI1.pdf",
       width=8, height=4)



to.pdf(figure7(ba), 
       filename="output/figures/Figure7.pdf",
       width=8, height=4)

