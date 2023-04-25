
rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))

library(parallel)
library(doParallel)
registerDoParallel(cores=64)

d_wide_list <- readRDS(file=here("data/simulated_data_list_old_null.RDS"))
d_wide_list <- d_wide_list[1:200]
gc()


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

saveRDS(resdf_noDetQ_Qint_tmle, paste0(here::here(),"/sim_res/old_null_sim_res_noDetQ_Qint_tmle_T2.RDS"))


int.start.time <- Sys.time()
resdf_noDetQ_tmle <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "tmle",N_time=2))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_noDetQ_tmle, paste0(here::here(),"/sim_res/old_null_sim_res_noDetQ_tmle_T2.RDS"))



resdf_noDetQ_Qint_ic <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=FALSE, varmethod = "ic",N_time=2))
  return(res)
}
saveRDS(resdf_noDetQ_Qint_ic, paste0(here::here(),"/sim_res/old_null_sim_res_noDetQ_Qint_ic_T2.RDS"))


int.start.time <- Sys.time()
resdf_ic <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "ic",N_time=2))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_ic, paste0(here::here(),"/sim_res/old_null_sim_res_noDetQ_ic_T2.RDS"))



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

saveRDS(resdf_noDetQ_Qint_tmle_T4, paste0(here::here(),"/sim_res/old_null_sim_res_noDetQ_Qint_tmle_T4.RDS"))


int.start.time <- Sys.time()
resdf_noDetQ_tmle_T4 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "tmle",N_time=4))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_noDetQ_tmle_T4, paste0(here::here(),"/sim_res/old_null_sim_res_noDetQ_tmle_T4.RDS"))



resdf_noDetQ_Qint_ic_T4 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=FALSE, varmethod = "ic",N_time=4))
  return(res)
}
saveRDS(resdf_noDetQ_Qint_ic_T4, paste0(here::here(),"/sim_res/old_null_sim_res_noDetQ_Qint_ic_T4.RDS"))


int.start.time <- Sys.time()
resdf_ic_T4 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "ic",N_time=4))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_ic_T4, paste0(here::here(),"/sim_res/old_null_sim_res_noDetQ_ic_T4.RDS"))





#10 year followup
Ntime=11


#Random forest
int.start.time <- Sys.time()
resdf_RF <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows') %dopar% {
  res <- run_ltmle_glmnet(d_wide_list[[i]], varmethod = "ic", resdf=NULL, SL.library="SL.randomForest", N_time=11, override_function=SuperLearner_override_RF)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")
resdf_RF

gc()
saveRDS(resdf_RF, paste0(here::here(),"/sim_res/old_null_sim_res_rf_ic_T11.RDS"))


#Ridge
int.start.time <- Sys.time()
resdf_RF <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows') %dopar% {
  res <- run_ltmle_glmnet(d_wide_list[[i]], varmethod = "ic", resdf=NULL, SL.library="SL.glmnet", N_time=11, override_function=SuperLearner_override_ridge)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")
resdf_RF

gc()
saveRDS(resdf_RF, paste0(here::here(),"/sim_res/old_null_sim_res_ridge_ic_T11.RDS"))

#Elastic Net
int.start.time <- Sys.time()
resdf_EN <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows') %dopar% {
  res <- run_ltmle_glmnet(d_wide_list[[i]], varmethod = "ic", resdf=NULL, SL.library="SL.glmnet", N_time=11, override_function=SuperLearner_override_EN)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")
resdf_EN

gc()
saveRDS(resdf_EN, paste0(here::here(),"/sim_res/old_null_sim_res_EN_ic_T11.RDS"))


#lasso prescreen
resdf_Qint_noDetQ_lasso_prescreen <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=FALSE, varmethod = "ic", override_function=SuperLearner_override_lasso_prescreen))

  return(res)
}
saveRDS(resdf_Qint_noDetQ_lasso_prescreen, paste0(here::here(),"/sim_res/old_null_sim_res_ic_Qint_noDetQ_lasso_prescreen_T11.RDS"))

#This ones running forever
# resdf_Qint_noDetQ_lasso_prescreen <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
#   res <- NULL
#   try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=TRUE, varmethod = "ic", override_function=SuperLearner_override_lasso_prescreen))
#
#   return(res)
# }
# saveRDS(resdf_Qint_noDetQ_lasso_prescreen, paste0(here::here(),"/sim_res/old_null_sim_res_ic_Qint_DetQ_lasso_prescreen_T11.RDS"))
#

resdf_AUC <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, varmethod = "ic",override_function=SuperLearner_override_AUC))
  return(res)
}
saveRDS(resdf_AUC, paste0(here::here(),"/sim_res/old_null_sim_res_AUC.RDS"))


resdf_Qint_AUC <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, varmethod = "ic",override_function=SuperLearner_override_AUC))
  return(res)
}
saveRDS(resdf_Qint_AUC, paste0(here::here(),"/sim_res/old_null_sim_res_Qint_AUC.RDS"))


#primary
int.start.time <- Sys.time()
resdf_noDetQ_tmle_T11 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "tmle",N_time=11))
  return(res)
}
saveRDS(resdf_noDetQ_tmle_T11, paste0(here::here(),"/sim_res/old_null_sim_res_noDetQ_tmle_T11.RDS"))

int.start.time <- Sys.time()
resdf_noDetQ_ic_T11 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "ic",N_time=11))
  return(res)
}
saveRDS(resdf_noDetQ_ic_T11, paste0(here::here(),"/sim_res/old_null_sim_res_noDetQ_ic_T11.RDS"))

summary(resdf_noDetQ_ic_T11$ate)
summary(resdf_noDetQ_ic_T11$iptw.ate)

int.start.time <- Sys.time()
resdf_noDetQ_ic_T11 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=TRUE, varmethod = "ic",N_time=11))
  return(res)
}
saveRDS(resdf_noDetQ_ic_T11, paste0(here::here(),"/sim_res/old_null_sim_res_ic_T11.RDS"))



#primary
int.start.time <- Sys.time()
resdf_noDetQ_Qint_tmle_T11 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=T, det.Q=FALSE, varmethod = "tmle",N_time=11))
  return(res)
}
saveRDS(resdf_noDetQ_Qint_tmle_T11, paste0(here::here(),"/sim_res/old_null_sim_res_noDetQ_Qint_tmle_T11.RDS"))

int.start.time <- Sys.time()
resdf_noDetQ_Qint_ic_T11 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=T, det.Q=FALSE, varmethod = "ic",N_time=11))
  return(res)
}
saveRDS(resdf_noDetQ_Qint_ic_T11, paste0(here::here(),"/sim_res/old_null_sim_res_noDetQ_Qint_ic_T11.RDS"))




#primary
resdf_noDetQ_tmle_T11_glm <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "tmle",N_time=11, SL.library="glm"))
  return(res)
}
saveRDS(resdf_noDetQ_tmle_T11_glm, paste0(here::here(),"/sim_res/old_null_sim_res_noDetQ_tmle_glm_T11.RDS"))


resdf_Qint_noDetQ_tmle_T11_glm <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=FALSE, varmethod = "tmle",N_time=11, SL.library="glm"))
  return(res)
}
saveRDS(resdf_Qint_noDetQ_tmle_T11_glm, paste0(here::here(),"/sim_res/old_null_sim_res_Qint_noDetQ_glm_tmle_T11.RDS"))

resdf_noDetQ_ic_glm_T11 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "ic",N_time=11, SL.library="glm"))
  return(res)
}
saveRDS(resdf_noDetQ_ic_glm_T11, paste0(here::here(),"/sim_res/old_null_sim_res_noDetQ_ic_glm_T11.RDS"))

resdf_ic_glm_T11 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=TRUE, varmethod = "ic",N_time=11, SL.library="glm"))
  return(res)
}
saveRDS(resdf_ic_glm_T11, paste0(here::here(),"/sim_res/old_null_sim_res_ic_glm_T11.RDS"))
