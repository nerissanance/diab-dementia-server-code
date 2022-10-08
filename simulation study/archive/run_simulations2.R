
rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))




gc()
d_wide_list <- readRDS(file=here("data/simulated_data_list.RDS"))
d_wide_list <- d_wide_list[1:100]
gc()

#temp
d=d_wide_list[[1]]
N_time = 11 #number of time points you want to look at
SL.library = c("SL.glmnet")
resdf=NULL
Qint=F
gcomp=F
det.Q=F
gbound = c(0.01, 1)
override_function=SuperLearner_override
varmethod = "ic" #variance method
alt=FALSE
label=""
try(res <- run_ltmle_glmnet(d_wide_list[[1]], resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "ic"))



#primary-no Qint
resdf_noDetQ_tmle <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "tmle"))
  return(res)
}
saveRDS(resdf_noDetQ_tmle, paste0(here::here(),"/data/sim_res_noDetQ_tmle_new.RDS"))

resdf_noDetQ_tmle_alt <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, alt = TRUE, det.Q=FALSE, varmethod = "tmle"))
  return(res)
}
saveRDS(resdf_noDetQ_tmle, paste0(here::here(),"/data/sim_res_noDetQ_tmle_alt.RDS"))


resdf_noDetQ_Qint_tmle_alt <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, alt = TRUE, det.Q=FALSE, varmethod = "tmle"))
  return(res)
}
saveRDS(resdf_noDetQ_tmle, paste0(here::here(),"/data/sim_res_noDetQ_tmle_alt.RDS"))


#lasso prescreen
resdf_noDetQ_lasso_prescreen <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "tmle", override_function=SuperLearner_override_lasso_prescreen))

  return(res)
}
saveRDS(resdf_Qint_noDetQ_lasso_prescreen, paste0(here::here(),"/data/sim_res_noDetQ_lasso_prescreen_100_200_reps.RDS"))



int.start.time <- Sys.time()
resdf_Qint_EN <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE,det.Q=FALSE, varmethod = "tmle", override_function=SuperLearner_override_EN))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_Qint_EN, paste0(here::here(),"/data/sim_res_noDetQ_EN_tmle.RDS"))

