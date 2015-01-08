
source("R/functions-figures.R")
source("R/figures.R")

to.pdf(figure_dLAIdrought2013(face_dLAIdrought2013),
       filename="output/figures/dLAI_litter_PAR_drought2013.pdf",
       width=5, height=4)

to.pdf(figure_LAI_byCO2_4(facegap_cloudy_byCO2),
       filename="output/figures/FACE_LAI_timeseries_byCO2_cloudy.pdf",
       width=8, height=4)

