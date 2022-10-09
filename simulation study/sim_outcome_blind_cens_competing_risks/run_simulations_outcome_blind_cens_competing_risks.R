
rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))

library(parallel)
library(doParallel)
registerDoParallel(cores=64)

gc()
d_wide_list <- readRDS(file=here("data/simulated_data_list_outcome_blind_cens_competing_risks.RDS"))
d_wide_list <- d_wide_list[1:200]
gc()

# try(res <- run_ltmle_glmnet(d_wide_list[[1]], resdf=NULL, Qint=F, det.Q=FALSE, varmethod = "ic",N_time=2))
# res

#Ntime
Ntime=2

#primary
int.start.time <- Sys.time()
resdf_noDetQ_Qint_tmle <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=FALSE, varmethod = "tmle",N_time=2))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_noDetQ_Qint_tmle, paste0(here::here(),"/sim_res/outcome_blind_cens_competing_risks_sim_res_noDetQ_Qint_tmle_T2.RDS"))


int.start.time <- Sys.time()
resdf_noDetQ_tmle <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "tmle",N_time=2))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_noDetQ_tmle, paste0(here::here(),"/sim_res/outcome_blind_cens_competing_risks_sim_res_noDetQ_tmle_T2.RDS"))



resdf_noDetQ_Qint_ic <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=FALSE, varmethod = "ic",N_time=2))
  return(res)
}
saveRDS(resdf_noDetQ_Qint_ic, paste0(here::here(),"/sim_res/outcome_blind_cens_competing_risks_sim_res_noDetQ_Qint_ic_T2.RDS"))


int.start.time <- Sys.time()
resdf_ic <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "ic",N_time=2))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_ic, paste0(here::here(),"/sim_res/outcome_blind_cens_competing_risks_sim_res_noDetQ_ic_T2.RDS"))



#Ntime
Ntime=4

#primary
int.start.time <- Sys.time()
resdf_noDetQ_Qint_tmle_T4 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=FALSE, varmethod = "tmle",N_time=4))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_noDetQ_Qint_tmle_T4, paste0(here::here(),"/sim_res/outcome_blind_cens_competing_risks_sim_res_noDetQ_Qint_tmle_T4.RDS"))


int.start.time <- Sys.time()
resdf_noDetQ_tmle_T4 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "tmle",N_time=4))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_noDetQ_tmle_T4, paste0(here::here(),"/sim_res/outcome_blind_cens_competing_risks_sim_res_noDetQ_tmle_T4.RDS"))



resdf_noDetQ_Qint_ic_T4 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=FALSE, varmethod = "ic",N_time=4))
  return(res)
}
saveRDS(resdf_noDetQ_Qint_ic_T4, paste0(here::here(),"/sim_res/outcome_blind_cens_competing_risks_sim_res_noDetQ_Qint_ic_T4.RDS"))


int.start.time <- Sys.time()
resdf_ic_T4 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "ic",N_time=4))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_ic_T4, paste0(here::here(),"/sim_res/outcome_blind_cens_competing_risks_sim_res_noDetQ_ic_T4.RDS"))


int.start.time <- Sys.time()
resdf_DetQ_ic_T4 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=TRUE, varmethod = "ic",N_time=4))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_DetQ_ic_T4, paste0(here::here(),"/sim_res/outcome_blind_cens_competing_risks_sim_res_ic_T4.RDS"))




#10 year followup
Ntime=11

#primary
int.start.time <- Sys.time()
resdf_noDetQ_Qint_tmle_T11 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=FALSE, varmethod = "tmle",N_time=11))
  return(res)
}
saveRDS(resdf_noDetQ_Qint_tmle_T11, paste0(here::here(),"/sim_res/outcome_blind_cens_competing_risks_sim_res_Qint_noDetQ_tmle_T11.RDS"))

int.start.time <- Sys.time()
resdf_noDetQ_Qint_ic_T11 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=FALSE, varmethod = "ic",N_time=11))
  return(res)
}
saveRDS(resdf_noDetQ_Qint_ic_T11, paste0(here::here(),"/sim_res/outcome_blind_cens_competing_risks_sim_res_noDetQ_ic_T11.RDS"))



#primary
int.start.time <- Sys.time()
resdf_noDetQ_tmle_T11 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "tmle",N_time=11))
  return(res)
}
saveRDS(resdf_noDetQ_tmle_T11, paste0(here::here(),"/sim_res/outcome_blind_cens_competing_risks_sim_res_noDetQ_tmle_T11.RDS"))

int.start.time <- Sys.time()
resdf_noDetQ_ic_T11 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "ic",N_time=11))
  return(res)
}
saveRDS(resdf_noDetQ_ic_T11, paste0(here::here(),"/sim_res/outcome_blind_cens_competing_risks_sim_res_noDetQ_ic_T11.RDS"))





#primary
int.start.time <- Sys.time()
resdf_glm_noDetQ_tmle_T11 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "tmle",N_time=11, glm=T))
  return(res)
}
saveRDS(resdf_glm_noDetQ_tmle_T11, paste0(here::here(),"/sim_res/outcome_blind_cens_competing_risks_sim_res_glm_noDetQ_tmle_T11.RDS"))

int.start.time <- Sys.time()
resdf_glm_noDetQ_ic_T11 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "ic",N_time=11, glm=T))
  return(res)
}
saveRDS(resdf_glm_noDetQ_ic_T11, paste0(here::here(),"/sim_res/outcome_blind_cens_competing_risks_sim_res_glm_noDetQ_ic_T11.RDS"))


