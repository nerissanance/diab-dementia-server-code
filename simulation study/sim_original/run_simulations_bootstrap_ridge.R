

rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))

library(parallel)
library(doParallel)
registerDoParallel(cores=60)

rm(list=ls())
gc()
d_wide_list <- readRDS(file=here("data/simulated_data_list.RDS"))
d_wide_list <- d_wide_list[1:200]
gc()

i<-j<-1
resdf_boot = NULL

for(i in 1:200){

  cat(i,"\n")
  d <- d_wide_list[[i]]
  d$id <- 1:nrow(d)
  d<-d %>% select(id, everything())

  res_df <- NULL
  res_df <- foreach(j = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {

    source(here::here("0_config.R"))
    source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
    source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))

    set.seed(j)
    dboot <- d[sample(.N, nrow(d),replace=TRUE)]


    res <- NULL
    try(res <- run_ltmle_glmnet_test(dboot, N_time = 11, resdf=NULL, Qint=FALSE, det.Q=F, varmethod = "ic", id=dboot$id, override_function=SuperLearner_override_ridge), silent=TRUE)
    return(res)
  }
  res_df

  gc()
  res_df$iteration <- i
  resdf_boot <- bind_rows(resdf_boot, res_df)
  saveRDS(res_df, paste0(here::here(),"/data/bootstrap/sim_res_boot_old_sim_ridge_v3_T11_",i,".RDS"))

}


