

rm(list=ls())
library(lava)
library(data.table)
source(paste0(here::here(),"/synthesizeDD.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))


sim_list <- readRDS(paste0(here::here(),"/data/simulated_data_list.RDS"))


sim_list_null<-list()
for(i in 1:length(sim_list)){
  d <- sim_list[[i]]
  Y <- d %>% select(starts_with("event_")|starts_with("censor_"))
  set.seed(123)
  Y <- Y[sample(nrow(Y)),]
  d <- d %>% select(!starts_with("event_")&!starts_with("censor_"))
  d <- bind_cols(d, Y)
  sim_list_null[[i]] <- d
}

saveRDS(sim_list_null, paste0(here::here(),"/data/simulated_data_list_old_null.RDS"))
