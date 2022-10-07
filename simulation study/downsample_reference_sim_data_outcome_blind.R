
rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))


gc()
d_wide_list <- readRDS(file=here("data/simulated_data_list_outcome_blind.RDS"))

i=1
for(i in 1:length(d_wide_list)){
 d <- d_wide_list[[i]]
 d_A1 <- d %>% filter(glp1_0==1&glp1_1==1&glp1_2==1&glp1_3==1&glp1_4==1&glp1_5==1&glp1_6==1&glp1_7==1&glp1_8==1&glp1_9==1&glp1_10==1&glp1_11==1)
 d_A0 <- d %>% filter(!(glp1_0==1&glp1_1==1&glp1_2==1&glp1_3==1&glp1_4==1&glp1_5==1&glp1_6==1&glp1_7==1&glp1_8==1&glp1_9==1&glp1_10==1&glp1_11==1))
  #subsample twice the size of A1
 d_A0 <- d_A0[sample(.N, nrow(d_A1)*2, replace=FALSE)]
 d_wide_list[[i]] <- bind_rows(d_A1, d_A0)
}





saveRDS(d_wide_list, file=here("data/simulated_data_list_outcome_blind_subsampled.RDS"))
