


rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))


library(parallel)
library(doParallel)
registerDoParallel(cores=64)

gc()
d_wide_list <- readRDS(file=here("data/simulated_data_list.RDS"))
d_wide_list <- d_wide_list[[1]]
gc()


dput(d_wide_list)

dput(colnames(d_wide_list))

set.seed(1234)
d_wide_list$other_drugs_0 <- d_wide_list$other_drugs_1 <- d_wide_list$other_drugs_2 <- d_wide_list$other_drugs_3 <- rbinom(nrow(d_wide_list),1, 0.3)


d <- d_wide_list %>% subset(., select=c("age_base", "sex", "ie.type", "code5txt", "quartile_income",
                                        "insulin_0", "ie_type",
                                        "hypertension_0",  "glp1_0", "sglt2_inhib_0", "other_drugs_0",
                                        "event_dementia_0", "censor_0", "event_death_0", "insulin_1",
                                         "hypertension_1",
                                        "renal.disease_1", "glp1_1", "sglt2_inhib_1", "other_drugs_1", "event_dementia_1",
                                        "censor_1", "event_death_1", "insulin_2",
                                         "hypertension_2",
                                        "glp1_2", "sglt2_inhib_2", "other_drugs_2", "event_dementia_2", "censor_2", "event_death_2",
                                        "insulin_3",
                                        "hypertension_3", "glp1_3", "sglt2_inhib_3", "other_drugs_3",
                                        "event_dementia_3", "censor_3", "event_death_3", "event_dementia_4"))

d <- d %>% slice(1:1000)
table(d$event_dementia_4)
saveRDS(d, here("data/example_simulated_data.RDS"))

