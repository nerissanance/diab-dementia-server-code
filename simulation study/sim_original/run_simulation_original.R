
rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))


library(parallel)
library(doParallel)
registerDoParallel(cores=64)

gc()
d_wide_list <- readRDS(file=here("data/simulated_data_list.RDS"))
d_wide_list <- d_wide_list[1:200]
gc()

#Add:

int.start.time <- Sys.time()
resdf_noDetQ_ic <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q =FALSE, varmethod = "ic"))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_noDetQ_ic, paste0(here::here(),"/data/sim_res_noDetQ_ic_v2.RDS"))
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

saveRDS(resdf_noDetQ_tmle, paste0(here::here(),"/data/sim_res_noDetQ_tmle_v2.RDS"))


int.start.time <- Sys.time()
resdf_DetQ_ic <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q =TRUE, varmethod = "ic"))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_DetQ_ic, paste0(here::here(),"/data/sim_res_DetQ_ic_v2.RDS"))




#lasso prescreen
resdf_Qint_noDetQ_lasso_prescreen <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=FALSE, varmethod = "tmle", override_function=SuperLearner_override_lasso_prescreen))

  return(res)
}

# resdf_noDetQ_Qint_tmle <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
#   res <- NULL
#   try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=FALSE, varmethod = "tmle",N_time=2))
#   return(res)
# }
saveRDS(resdf_Qint_noDetQ_lasso_prescreen, paste0(here::here(),"/data/sim_res_Qint_noDetQ_lasso_prescreen.RDS"))


#all interactions with A - check on simulation? no interactions in the data
#try(res <- run_ltmle_glmnet_interaction(d_wide_list[[1]], resdf=NULL, Qint=TRUE, override_function=SuperLearner_override_1se))

#Make sglt2+glp1
#Make all non-deterministic with tmle variance option
#Make more common and rerun


#unadjusted


# resdf_Qint_1se_int <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
#   res <- NULL
#   try(res <- run_ltmle_glmnet_interaction(d_wide_list[[i]], resdf=NULL, Qint=TRUE, override_function=SuperLearner_override_1se))
#   return(res)
# }
# saveRDS(resdf_Qint_1se_int, paste0(here::here(),"/data/sim_res_Qint_1se_int.RDS"))


# resdf_1se_int <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
#   res <- NULL
#   try(res <- run_ltmle_glmnet_interaction(d_wide_list[[i]], resdf=NULL, Qint=FALSE, override_function=SuperLearner_override_1se))
#   return(res)
# }
# saveRDS(resdf_1se_int, paste0(here::here(),"/data/sim_res_1se_int.RDS"))




int.start.time <- Sys.time()
resdf_glm <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows') %dopar% {
  res <- run_ltmle_glmnet(d_wide_list[[i]], varmethod = "ic", resdf=NULL, SL.library="glm")
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")
resdf_glm

gc()
saveRDS(resdf_glm, paste0(here::here(),"/data/sim_res_glm_ic.RDS"))






int.start.time <- Sys.time()
resdf_noDetQ_Qint_ic <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q =FALSE, varmethod = "ic"))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_noDetQ_Qint_ic, paste0(here::here(),"/data/sim_res_noDetQ_Qint_ic.RDS"))


int.start.time <- Sys.time()
resdf_noDetQ_Qint_tmle <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=FALSE, varmethod = "tmle"))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_noDetQ_Qint_tmle, paste0(here::here(),"/data/sim_res_noDetQ_Qint_tmle.RDS"))




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
resdf_ic <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_ic, paste0(here::here(),"/data/sim_res_Qint_ic.RDS"))



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




int.start.time <- Sys.time()
resdf_RF <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows') %dopar% {
  res <- run_ltmle_glmnet(d_wide_list[[i]], varmethod = "ic", resdf=NULL, SL.library="SL.randomForest", override_function=SuperLearner_override_RF)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")
resdf_RF

gc()
saveRDS(resdf_RF, paste0(here::here(),"/data/sim_res_rf.RDS"))







#NEW
int.start.time <- Sys.time()
resdf_ridge <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, override_function=SuperLearner_override_ridge))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_ridge, paste0(here::here(),"/data/sim_res_ridge.RDS"))
resdf_ridge


int.start.time <- Sys.time()
resdf_ridge_AUC <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, override_function=SuperLearner_override_ridge_AUC))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_ridge_AUC, paste0(here::here(),"/data/sim_res_ridge_AUC.RDS"))

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



int.start.time <- Sys.time()
resdf_gcomp <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, gcomp=TRUE))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_gcomp, paste0(here::here(),"/data/sim_res_gcomp.RDS"))
