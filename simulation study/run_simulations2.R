
rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))




gc()
d_wide_list <- readRDS(file=here("data/simulated_data_list.RDS"))
d_wide_list <- d_wide_list[1:100]
gc()


#primary-no Qint
resdf_noDetQ_tmle <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "tmle"))
  return(res)
}
saveRDS(resdf_noDetQ_tmle, paste0(here::here(),"/data/sim_res_noDetQ_tmle.RDS"))

int.start.time <- Sys.time()
resdf_Qint_EN <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE,det.Q=FALSE, varmethod = "tmle", override_function=SuperLearner_override_EN))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_Qint_EN, paste0(here::here(),"/data/sim_res_noDetQ_Qint_EN_tmle.RDS"))




#Ridge
resdf_noDetQ_Qint_tmle_ridge <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=FALSE, varmethod = "tmle", override_function=SuperLearner_override_ridge))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_noDetQ_Qint_tmle_ridge, paste0(here::here(),"/data/sim_res_ridge_noDetQ_Qint_tmle.RDS"))

#EN
resdf_noDetQ_Qint_tmle_EN <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=FALSE, varmethod = "tmle", override_function=SuperLearner_override_EN))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_noDetQ_Qint_tmle_EN, paste0(here::here(),"/data/sim_res_EN_noDetQ_Qint_tmle.RDS"))




XXXXXXXXXXXXXXXXXXXXXX







int.start.time <- Sys.time()
resdf_noDetQ_Qint_tmle <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=FALSE, varmethod = "tmle"))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_noDetQ_Qint_tmle, paste0(here::here(),"/data/sim_res_noDetQ_Qint_tmle.RDS"))











resdf_Qint_1se <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, override_function=SuperLearner_override_1se))
  return(res)
}
saveRDS(resdf_Qint_1se, paste0(here::here(),"/data/sim_res_Qint_1se.RDS"))

resdf_Qint_1se_altOrd <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, override_function=SuperLearner_override_1se, alt=TRUE))
  return(res)
}
saveRDS(resdf_Qint_1se_altOrd, paste0(here::here(),"/data/sim_res_Qint_1se_altOrd.RDS"))



int.start.time <- Sys.time()
resdf_RF_gcomp <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows') %dopar% {
  res <- run_ltmle_glmnet(d_wide_list[[i]], varmethod = "ic", resdf=NULL, gcomp=TRUE, SL.library="SL.randomForest", override_function=SuperLearner_override_RF)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")
resdf_RF_gcomp

gc()
saveRDS(resdf_RF_gcomp, paste0(here::here(),"/data/sim_res_rf_gcomp.RDS"))


