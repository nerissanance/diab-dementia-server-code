
rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))




gc()
d_wide_list <- readRDS(file=here("data/simulated_data_list.RDS"))
d_wide_list <- d_wide_list[1:100]
gc()

#Unadjusted
resdf_noDetQ_Qint_tmle_unadj <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_unadj(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=FALSE, varmethod = "tmle", override_function=SuperLearner_override, SL.library = c("glm")))
  return(res)
}
saveRDS(resdf_noDetQ_Qint_tmle_unadj, paste0(here::here(),"/data/sim_res_noDetQ_Qint_tmle_unadj.RDS"))

resdf_noDetQ_tmle_unadj <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_unadj(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "tmle", override_function=SuperLearner_override, SL.library = c("glm")))
  return(res)
}
saveRDS(resdf_noDetQ_tmle_unadj, paste0(here::here(),"/data/sim_res_noDetQ_tmle_unadj.RDS"))

resdf_noDetQ_Qint_ic_unadj <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_unadj(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=TRUE, varmethod = "ic", override_function=SuperLearner_override, SL.library = c("glm")))
  return(res)
}
saveRDS(resdf_noDetQ_Qint_ic_unadj, paste0(here::here(),"/data/sim_res_noDetQ_Qint_ic_unadj.RDS"))


resdf_noDetQ_ic_unadj <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_unadj(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=TRUE, varmethod = "ic", override_function=SuperLearner_override, SL.library = c("glm")))
  return(res)
}
saveRDS(resdf_noDetQ_ic_unadj, paste0(here::here(),"/data/sim_res_noDetQ_ic_unadj.RDS"))

