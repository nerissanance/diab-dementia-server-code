
rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))

library(parallel)
library(doParallel)
registerDoParallel(cores=64)

gc()
d_wide_list <- readRDS(file=here("data/simulated_data_list_null_no_cens.RDS"))
d_wide_list <- d_wide_list[1:200]
gc()

# try(res <- run_ltmle_glmnet_no_cens(d_wide_list[[1]], Qint=F, det.Q=FALSE, varmethod = "ic",N_time=2))
# res


#Ntime
Ntime=4

#primary
int.start.time <- Sys.time()
resdf_noDetQ_Qint_tmle_T4 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_no_cens(d_wide_list[[i]], Qint=TRUE, det.Q=FALSE, varmethod = "tmle",N_time=4))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_noDetQ_Qint_tmle_T4, paste0(here::here(),"/sim_res/null_no_cens_sim_res_noDetQ_Qint_tmle_T4.RDS"))


int.start.time <- Sys.time()
resdf_noDetQ_tmle_T4 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_no_cens(d_wide_list[[i]], Qint=FALSE, det.Q=FALSE, varmethod = "tmle",N_time=4))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_noDetQ_tmle_T4, paste0(here::here(),"/sim_res/null_no_cens_sim_res_noDetQ_tmle_T4.RDS"))



resdf_noDetQ_Qint_ic_T4 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_no_cens(d_wide_list[[i]], Qint=TRUE, det.Q=FALSE, varmethod = "ic",N_time=4))
  return(res)
}
saveRDS(resdf_noDetQ_Qint_ic_T4, paste0(here::here(),"/sim_res/null_no_cens_sim_res_noDetQ_Qint_ic_T4.RDS"))


int.start.time <- Sys.time()
resdf_noDetQ_ic_T4 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_no_cens(d_wide_list[[i]], Qint=FALSE, det.Q=FALSE, varmethod = "ic",N_time=4))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_noDetQ_ic_T4, paste0(here::here(),"/sim_res/null_no_cens_sim_res_noDetQ_ic_T4.RDS"))

#DetQ
resdf_ic_T4 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_no_cens(d_wide_list[[i]], Qint=FALSE, det.Q=TRUE, varmethod = "ic",N_time=4))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_ic_T4, paste0(here::here(),"/sim_res/null_no_cens_sim_res_ic_T4.RDS"))

#GLM
resdf_glm_noDetQ_ic_T4 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_no_cens(d_wide_list[[i]], Qint=FALSE, det.Q=FALSE, varmethod = "ic",N_time=4, glm=TRUE))
  return(res)
}
saveRDS(resdf_glm_noDetQ_ic_T4, paste0(here::here(),"/sim_res/null_glm_no_cens_sim_res_noDetQ_ic_T4.RDS"))

resdf_glm_ic_T4 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_no_cens(d_wide_list[[i]], Qint=FALSE, det.Q=TRUE, varmethod = "ic",N_time=4, glm=TRUE))
  return(res)
}
saveRDS(resdf_glm_ic_T4, paste0(here::here(),"/sim_res/null_glm_no_cens_sim_res_ic_T4.RDS"))


resdf_glm_noDetQ_tmle_T4 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_no_cens(d_wide_list[[i]], Qint=FALSE, det.Q=FALSE, varmethod = "tmle",N_time=4, glm=TRUE))
  return(res)
}

saveRDS(resdf_glm_noDetQ_tmle_T4, paste0(here::here(),"/sim_res/null_glm_no_cens_sim_res_noDetQ_tmle_T4.RDS"))

resdf_glm_Qint_noDetQ_tmle_T4 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_no_cens(d_wide_list[[i]], Qint=TRUE, det.Q=FALSE, varmethod = "tmle",N_time=4, glm=TRUE))
  return(res)
}

saveRDS(resdf_glm_Qint_noDetQ_tmle_T4, paste0(here::here(),"/sim_res/null_glm_Qint_no_cens_sim_res_noDetQ_tmle_T4.RDS"))


#SL.mean


# #10 year followup
# Ntime=11
#
# #primary
# int.start.time <- Sys.time()
# resdf_noDetQ_Qint_tmle_T11 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
#   res <- NULL
#   try(res <- run_ltmle_glmnet_no_cens(d_wide_list[[i]], Qint=FALSE, det.Q=FALSE, varmethod = "tmle",N_time=11))
#   return(res)
# }
# saveRDS(resdf_noDetQ_Qint_tmle_T11, paste0(here::here(),"/sim_res/null_no_cens_sim_res_noDetQ_tmle_T11.RDS"))
#
# int.start.time <- Sys.time()
# resdf_noDetQ_Qint_ic_T11 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
#   res <- NULL
#   try(res <- run_ltmle_glmnet_no_cens(d_wide_list[[i]], Qint=FALSE, det.Q=FALSE, varmethod = "ic",N_time=11))
#   return(res)
# }
# saveRDS(resdf_noDetQ_Qint_ic_T11, paste0(here::here(),"/sim_res/null_no_cens_sim_res_noDetQ_ic_T11.RDS"))


