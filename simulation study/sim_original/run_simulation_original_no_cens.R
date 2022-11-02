
rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))


library(parallel)
library(doParallel)
registerDoParallel(cores=64)

gc()
d_wide_list <- readRDS(file=here("data/simulated_data_list_no_cens.RDS"))
d_wide_list <- d_wide_list[1:200]
gc()

table(d_wide_list[[1]]$event_death_10)
table(d_wide_list[[1]]$censor_10)



int.start.time <- Sys.time()
resdf_noDetQ_ic <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q =FALSE, varmethod = "ic"))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_noDetQ_ic, paste0(here::here(),"/data/sim_res_noDetQ_ic_no_cens.RDS"))
exp(summary(log(resdf_noDetQ_ic$estimate)))
summary(resdf_noDetQ_ic$ate)

int.start.time <- Sys.time()
resdf_noDetQ_tmle <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "tmle"))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_noDetQ_tmle, paste0(here::here(),"/data/sim_res_noDetQ_tmle_no_cens.RDS"))


int.start.time <- Sys.time()
resdf_DetQ_ic <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q =TRUE, varmethod = "ic"))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_DetQ_ic, paste0(here::here(),"/data/sim_res_DetQ_ic_no_cens.RDS"))
