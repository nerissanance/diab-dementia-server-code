

source(here::here("0_config.R"))


##Run analyses
source("./2_analysis/glp1_vs_any_static.R")
source("./2_analysis/glp1_vs_sglt2_static.R")



##knit report files

rmarkdown::render("EDA.Rmd","html_document")
rmarkdown::render("weight_distributions.Rmd","html_document")