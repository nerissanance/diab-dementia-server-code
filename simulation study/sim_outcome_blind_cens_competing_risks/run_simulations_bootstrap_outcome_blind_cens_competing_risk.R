

rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))

library(parallel)
library(doParallel)
registerDoParallel(cores=25)

rm(list=ls())
gc()
d_wide_list <- readRDS(file=here("data/simulated_data_list_outcome_blind_cens_competing_risks.RDS"))
d_wide_list <- d_wide_list[1:200]
gc()

i<-j<-1
resdf_boot = NULL
#for(i in 1:length(d_wide_list)){
#temp rerun
int.start.time <- Sys.time()
for(i in 1:200){
#for(i in 1:length(d_wide_list)){

  cat(i,"\n")
  d <- d_wide_list[[i]]
  d$id <- 1:nrow(d)


  res_df <- NULL
  res_df <- foreach(j = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {

    source(here::here("0_config.R"))
    source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
    source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))

    set.seed(j)
    dboot <- d[sample(.N, nrow(d),replace=TRUE)]

    # #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    # #run this code to drop ties, or use the ID argument to CV by ID
    # dboot <- dboot %>% distinct()
    # #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

    res <- NULL
    try(res <- run_ltmle_glmnet(dboot, N_time = 11, resdf=NULL, Qint=FALSE, det.Q=TRUE, varmethod = "ic", id=dboot$id), silent=TRUE)
    return(res)
  }
  res_df

  gc()
  res_df$iteration <- i
  resdf_boot <- bind_rows(resdf_boot, res_df)
  saveRDS(res_df, paste0(here::here(),"/data/bootstrap/sim_res_boot_outcome_blind_cens_competing_risks_T11_",i,".RDS"))

}
int.end.time <- Sys.time()
time1 <- difftime(int.end.time, int.start.time, units="mins")
time1


saveRDS(resdf_boot, paste0(here::here(),"/data/sim_res_boot_outcome_blind_cens_competing_risks_T11.RDS"))


