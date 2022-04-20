
rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))




gc()
d_wide_list <- readRDS(file=here("data/simulated_data_list.RDS"))
d_wide_list <- d_wide_list[1:50]
gc()


int.start.time <- Sys.time()
resdf_glm <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows') %dopar% {
  res <- run_ltmle(d_wide_list[[i]], varmethod = "ic", resdf=NULL, SL.library="glm")
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")
resdf_glm

gc()
saveRDS(resdf_glm, paste0(here::here(),"/data/sim_res_glm_ic.RDS"))



int.start.time <- Sys.time()
resdf_ic <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_ic, paste0(here::here(),"/data/sim_res_ic.RDS"))



int.start.time <- Sys.time()
resdf_EN <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, override_function=SuperLearner_override_EN))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_EN, paste0(here::here(),"/data/sim_res_EN.RDS"))


int.start.time <- Sys.time()
resdf_Qint_EN <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, override_function=SuperLearner_override_EN))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_Qint_EN, paste0(here::here(),"/data/sim_res_Qint_EN.RDS"))


#-

resdf_AUC <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, override_function=SuperLearner_override_AUC))
  return(res)
}
saveRDS(resdf_AUC, paste0(here::here(),"/data/sim_res_AUC.RDS"))


resdf_Qint_AUC <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, override_function=SuperLearner_override_AUC))
  return(res)
}
saveRDS(resdf_Qint_AUC, paste0(here::here(),"/data/sim_res_Qint_AUC.RDS"))



resdf_1se <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, override_function=SuperLearner_override_1se))
  return(res)
}
saveRDS(resdf_1se, paste0(here::here(),"/data/sim_res_1se.RDS"))

resdf_Qint_1se <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, override_function=SuperLearner_override_1se))
  return(res)
}
saveRDS(resdf_Qint_1se, paste0(here::here(),"/data/sim_res_Qint_1se.RDS"))

resdf_AUC_1se <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, override_function=SuperLearner_override_AUC_1se))
  return(res)
}
saveRDS(resdf_AUC_1se, paste0(here::here(),"/data/sim_res_AUC_1se.RDS"))

int.start.time <- Sys.time()
resdf_Qint_AUC_1se <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, override_function=SuperLearner_override_AUC_1se))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_Qint_AUC_1se, paste0(here::here(),"/data/sim_res_Qint_AUC_1se.RDS"))
