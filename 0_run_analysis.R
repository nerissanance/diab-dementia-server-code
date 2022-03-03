

source(here::here("0_config.R"))


##Run analyses
source("./analysis/TCP1_glp1_vs_any_static.R")
source("./analysis/TCP2_MSM.R")
source("./analysis/TCP3_glp1_vs_sglt2_static.R")



##knit report files

# rmarkdown::render("EDA.Rmd","html_document")
# rmarkdown::render("weight_distributions.Rmd","html_document")
