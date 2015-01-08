
source("R/functions-figures.R")


to.pdf(figure_dLAIdrought2013(face_dLAIdrought2013),
       filename="output/figures/dLAI_litter_PAR_drought2013.pdf")

to.pdf(figure_LAI_byCO2_4(facegap_cloudy_byCO2),
       filename="output/figures/FACE_LAI_timeseries_byCO2_cloudy.pdf",
       width=10, height=7)

