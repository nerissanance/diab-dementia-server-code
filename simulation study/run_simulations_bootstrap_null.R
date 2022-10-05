

rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))



rm(list=ls())
gc()
d_wide_list <- readRDS(file=here("data/null_simulated_data_list_subsampled.RDS"))
d_wide_list <- d_wide_list[1:200]
gc()

i<-j<-1
resdf_boot = NULL
#for(i in 1:length(d_wide_list)){
#temp rerun
for(i in 132:length(d_wide_list)){

  cat(i,"\n")
  d <- d_wide_list[[i]]
  d$id <- 1:nrow(d)

  # res_df <- foreach(j = 1:2, .combine = 'bind_rows',
  #                   #.errorhandling = 'remove',
  #                   .errorhandling = 'pass',
  #                   .packages='ltmle') %dopar% {

  res_df <- NULL
  for(j in 1:200){
    set.seed(j)
    dboot <- d[sample(.N, nrow(d),replace=TRUE)]

    # #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    # #run this code to drop ties, or use the ID argument to CV by ID
    # dboot <- dboot %>% distinct()
    # #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

    res <- NULL
    try(res <- run_ltmle_glmnet(dboot, N_time = 2, resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "ic", id=dboot$id), silent=TRUE)

    res_df <- bind_rows(res_df, res)
  }
  res_df

  gc()
  res_df$iteration <- i
  resdf_boot <- bind_rows(resdf_boot, res_df)
  saveRDS(res_df, paste0(here::here(),"/data/bootstrap/sim_res_boot_null_T2_",i,".RDS"))

}



saveRDS(resdf_boot, paste0(here::here(),"/data/sim_res_boot_null_T2.RDS"))



