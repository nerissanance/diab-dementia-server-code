
rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))




gc()
d_wide_list <- readRDS(file=here("data/simulated_data_list.RDS"))
d_wide_list <- d_wide_list[1:100]
gc()



resdf_noDetQ_Qint_tmle_ridge <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=FALSE, gbound = c(0.1, 0.9), varmethod = "tmle", override_function=SuperLearner_override_ridge))
  return(res)
}
int.end.time <- Sys.time()

saveRDS(resdf_noDetQ_Qint_tmle_ridge, paste0(here::here(),"/data/sim_res_ridge_noDetQ_Qint_tmle_gbound1_9.RDS"))

resdf_noDetQ_Qint_tmle_ridge <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=FALSE, gbound = c(0.05, 1), varmethod = "tmle", override_function=SuperLearner_override_ridge))
  return(res)
}
int.end.time <- Sys.time()

saveRDS(resdf_noDetQ_Qint_tmle_ridge, paste0(here::here(),"/data/sim_res_ridge_noDetQ_Qint_tmle_gbound05.RDS"))

resdf_noDetQ_Qint_tmle_ridge <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=FALSE, gbound = c(0.005, 0.9), varmethod = "tmle", override_function=SuperLearner_override_ridge))
  return(res)
}
int.end.time <- Sys.time()

saveRDS(resdf_noDetQ_Qint_tmle_ridge, paste0(here::here(),"/data/sim_res_ridge_noDetQ_Qint_tmle_gbound005_9.RDS"))

#Ridge
resdf_noDetQ_Qint_tmle_ridge <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=FALSE, gbound = c(0.001, 1), varmethod = "tmle", override_function=SuperLearner_override_ridge))
  return(res)
}
int.end.time <- Sys.time()

saveRDS(resdf_noDetQ_Qint_tmle_ridge, paste0(here::here(),"/data/sim_res_ridge_noDetQ_Qint_tmle_gbound001.RDS"))



