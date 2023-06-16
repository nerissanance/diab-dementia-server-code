

rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))


library(parallel)
library(doParallel)
registerDoParallel(cores=100)

gc()
d_wide_list <- readRDS(file=here("data/simulated_data_list.RDS"))
d_wide_list <- d_wide_list[1:200]
gc()


# resdf_noDetQ_ic_glm <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
#   res <- NULL
#   try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "ic",N_time=11, SL.library="glm"))
#   return(res)
# }
# saveRDS(resdf_noDetQ_ic_glm, paste0(here::here(),"/sim_res/sim_res_noDetQ_ic_glm_v5.RDS"))
#
# int.start.time <- Sys.time()
# resdf_ridge <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
#   res <- NULL
#   try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=FALSE, override_function=SuperLearner_override_ridge))
#   return(res)
# }
# int.end.time <- Sys.time()
# difftime(int.end.time, int.start.time, units="mins")
#
# saveRDS(resdf_ridge, paste0(here::here(),"/data/sim_res_ridge_noDetQ_v5.RDS"))
#
# int.start.time <- Sys.time()
# resdf_ridge <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
#   res <- NULL
#   try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=FALSE, override_function=SuperLearner_override_ridge))
#   return(res)
# }
# int.end.time <- Sys.time()
# difftime(int.end.time, int.start.time, units="mins")
#
# saveRDS(resdf_ridge, paste0(here::here(),"/data/sim_res_ridge_Qint_noDetQ_v5.RDS"))
# int.start.time <- Sys.time()
# resdf_ridge <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
#   res <- NULL
#   try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=TRUE, override_function=SuperLearner_override_ridge))
#   return(res)
# }
# int.end.time <- Sys.time()
# difftime(int.end.time, int.start.time, units="mins")
#
# saveRDS(resdf_ridge, paste0(here::here(),"/data/sim_res_ridge_Qint_v5.RDS"))


# int.start.time <- Sys.time()
# resdf_noDetQ_ic_v5 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
#   res <- NULL
#   try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "ic"))
#   return(res)
# }
# int.end.time <- Sys.time()
# difftime(int.end.time, int.start.time, units="mins")
#
# saveRDS(resdf_noDetQ_ic_v5, paste0(here::here(),"/sim_res/protective/sim_res_noDetQ_ic_v5.RDS"))
#
#
# resdf_DetQ_ic_v5 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
#   res <- NULL
#   try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=TRUE, varmethod = "ic"))
#   return(res)
# }
# saveRDS(resdf_DetQ_ic_v5, paste0(here::here(),"/sim_res/protective/sim_res_DetQ_ic_v5.RDS"))



# int.start.time <- Sys.time()
# resdf_RF <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows') %dopar% {
#   res <- run_ltmle_glmnet(d_wide_list[[i]], varmethod = "ic", resdf=NULL, Qint=FALSE, det.Q=TRUE, SL.library="SL.randomForest", override_function=SuperLearner_override_RF)
# }
# int.end.time <- Sys.time()
# difftime(int.end.time, int.start.time, units="mins")
# resdf_RF
#
# gc()
# saveRDS(resdf_RF, paste0(here::here(),"/sim_res/protective/sim_res_detQ_rf_v5.RDS"))
#
#
# int.start.time <- Sys.time()
# resdf_RF <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows') %dopar% {
#   res <- run_ltmle_glmnet(d_wide_list[[i]], varmethod = "ic", resdf=NULL, Qint=FALSE, det.Q=FALSE, SL.library="SL.randomForest", override_function=SuperLearner_override_RF)
# }
# int.end.time <- Sys.time()
# difftime(int.end.time, int.start.time, units="mins")
# resdf_RF
#
# gc()
# saveRDS(resdf_RF, paste0(here::here(),"/sim_res/protective/sim_res_noDetQ_rf_v5.RDS"))
#
#
# int.start.time <- Sys.time()
# resdf_RF <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows') %dopar% {
#   res <- run_ltmle_glmnet(d_wide_list[[i]], varmethod = "ic", resdf=NULL, Qint=TRUE, det.Q=TRUE, SL.library="SL.randomForest", override_function=SuperLearner_override_RF)
# }
# int.end.time <- Sys.time()
# difftime(int.end.time, int.start.time, units="mins")
# resdf_RF
#
# gc()
# saveRDS(resdf_RF, paste0(here::here(),"/sim_res/protective/sim_res_detQ_Qint_rf_v5.RDS"))
#
#
# int.start.time <- Sys.time()
# resdf_RF <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows') %dopar% {
#   res <- run_ltmle_glmnet(d_wide_list[[i]], varmethod = "ic", resdf=NULL, Qint=TRUE, det.Q=FALSE, SL.library="SL.randomForest", override_function=SuperLearner_override_RF)
# }
# int.end.time <- Sys.time()
# difftime(int.end.time, int.start.time, units="mins")
# resdf_RF
#
# gc()
# saveRDS(resdf_RF, paste0(here::here(),"/sim_res/protective/sim_res_noDetQ_Qint_rf_v5.RDS"))
#
#
# resdf_noDetQ_Qint_ic_v5 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
#   res <- NULL
#   try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=FALSE, varmethod = "ic"))
#   return(res)
# }
# saveRDS(resdf_noDetQ_Qint_ic_v5, paste0(here::here(),"/sim_res/protective/sim_res_noDetQ_Qint_ic_v5.RDS"))
#
# resdf_DetQ_Qint_ic_v5 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
#   res <- NULL
#   try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=TRUE, varmethod = "ic"))
#   return(res)
# }
# saveRDS(resdf_DetQ_Qint_ic_v5, paste0(here::here(),"/sim_res/protective/sim_res_DetQ_Qint_ic_v5.RDS"))



# #gbounds
# resdf_DetQ_ic_gbound_001 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
#   res <- NULL
#   try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE,gbound = c(0.001, 1), det.Q=TRUE, varmethod = "ic"))
#   return(res)
# }
# saveRDS(resdf_DetQ_ic_gbound_001, paste0(here::here(),"/sim_res/protective/resdf_DetQ_ic_gbound_001.RDS"))
#
# resdf_DetQ_ic_gbound_005_9 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
#   res <- NULL
#   try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE,gbound = c(0.005, 0.9), det.Q=TRUE, varmethod = "ic"))
#   return(res)
# }
# saveRDS(resdf_DetQ_ic_gbound_005_9, paste0(here::here(),"/sim_res/protective/resdf_DetQ_ic_gbound_005_9.RDS"))
#
#
# resdf_DetQ_ic_gbound_05 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
#   res <- NULL
#   try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE,gbound = c(0.05, 1), det.Q=TRUE, varmethod = "ic"))
#   return(res)
# }
# saveRDS(resdf_DetQ_ic_gbound_05, paste0(here::here(),"/sim_res/protective/resdf_DetQ_ic_gbound_05.RDS"))


resdf_lasso_prescreen <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=TRUE, varmethod = "ic", override_function=SuperLearner_override_lasso_prescreen))

  return(res)
}

saveRDS(resdf_lasso_prescreen, paste0(here::here(),"/sim_res/protective/sim_res_lasso_DetQ_prescreen_ic.RDS"))

resdf_Qint_lasso_prescreen <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=TRUE, varmethod = "ic", override_function=SuperLearner_override_lasso_prescreen))

  return(res)
}

saveRDS(resdf_Qint_lasso_prescreen, paste0(here::here(),"/sim_res/protective/sim_res_Qint_lasso_DetQ_prescreen_ic.RDS"))

