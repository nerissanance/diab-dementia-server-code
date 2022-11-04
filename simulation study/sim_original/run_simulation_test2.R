
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

d=d_wide_list[[1]]
N_time = 2
SL.library = c("SL.glmnet")
resdf=NULL
Qint=F
gcomp=F
det.Q=T
gbound = c(0.01, 1)
override_function=SuperLearner_override
varmethod = "ic" #variance method
label=""
glm=FALSE
id=NULL





# #Ntime=11
# resdf_ic_t11  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
#   res <- NULL
#   try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]],  N_time=11))
#   return(res)
# }
# saveRDS(resdf_ic_t11, paste0(here::here(),"/sim_res/sim_res_noDetQ_ic_v3.RDS"))

resdf_Qint_ic_t11  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]],  Qint=T, N_time=11))
  return(res)
}
saveRDS(resdf_Qint_ic_t11, paste0(here::here(),"/sim_res/sim_res_Qint_noDetQ_ic_v3.RDS"))


resdf_noDetQ_ic_t11  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]], det.Q=F, N_time=11))
  return(res)
}
saveRDS(resdf_noDetQ_ic_t11, paste0(here::here(),"/sim_res/sim_res_noDetQ_ic_v3.RDS"))


#Ntime=11
resdf_tmle_t11  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]],  N_time=11, varmethod = "tmle"))
  return(res)
}
saveRDS(resdf_tmle_t11, paste0(here::here(),"/sim_res/sim_res_noDetQ_tmle_v3.RDS"))


#SuperLearner_override_ridge
resdf_ic_ridge_t11  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]],  N_time=11, override_function=SuperLearner_override_ridge))
  return(res)
}
saveRDS(resdf_ic_ridge_t11, paste0(here::here(),"/sim_res/sim_res_ridge_ic_v3.RDS"))

#SuperLearner_override_lasso_prescreen
resdf_ic_lasso_prescreen_t11  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]],  N_time=11, override_function=SuperLearner_override_lasso_prescreen))
  return(res)
}
saveRDS(resdf_ic_lasso_prescreen_t11, paste0(here::here(),"/sim_res/sim_res_lasso_prescreen_ic_v3.RDS"))



#SuperLearner_override_ridge_1se
resdf_ic_ridge_1se_t11  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]],  N_time=11, override_function=SuperLearner_override_ridge_1se))
  return(res)
}
saveRDS(resdf_ic_ridge_1se_t11, paste0(here::here(),"/sim_res/sim_res_ridge_1se_ic_v3.RDS"))


#SuperLearner_override_1se
resdf_ic_1se_t11  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]],  N_time=11, override_function=SuperLearner_override_1se))
  return(res)
}
saveRDS(resdf_ic_1se_t11, paste0(here::here(),"/sim_res/sim_res_1se_ic_v3.RDS"))


#KEEP running these:

#SuperLearner_override_EN_1se
resdf_ic_EN_1se_t11  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]],  N_time=11, override_function=SuperLearner_override_EN_1se))
  return(res)
}
saveRDS(resdf_ic_EN_1se_t11, paste0(here::here(),"/sim_res/sim_res_EN_1se_ic_v3.RDS"))

#SuperLearner_override_AUC
resdf_ic_AUC_t11  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]],  N_time=11, override_function=SuperLearner_override_AUC))
  return(res)
}
saveRDS(resdf_ic_AUC_t11, paste0(here::here(),"/sim_res/sim_res_AUC_ic_v3.RDS"))


#SuperLearner_override_RF
resdf_ic_RF_t11  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]],  N_time=11, override_function=SuperLearner_override_RF))
  return(res)
}
saveRDS(resdf_ic_RF_t11, paste0(here::here(),"/sim_res/sim_res_RF_ic_v3.RDS"))



#SuperLearner_override_ridge_AUC
resdf_ic_ridge_AUC_t11  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]],  N_time=11, override_function=resdf_ic_ridge_AUC_t11))
  return(res)
}
saveRDS(resdf_ic_ridge_AUC_t11, paste0(here::here(),"/sim_res/sim_res_ridge_AUC_ic_v3.RDS"))


# SuperLearner_override_EN_AUC
# SuperLearner_override_ridge_AUC
# SuperLearner_override_AUC_1se


# save(resdf_ic_t2, resdf_ic_t3, resdf_ic_t4, resdf_ic_t5, resdf_ic_t6, resdf_ic_t7, resdf_ic_t8, resdf_ic_t9, resdf_ic_t10, resdf_ic_t11, file=paste0(here::here(),"/sim_res/sim_res_ic_t1-11.Rdata"))
# saveRDS(resdf_ic_t11, paste0(here::here(),"/sim_res/sim_res_noDetQ_ic_v3.RDS"))
# saveRDS(d, paste0(here::here(),"/sim_res/sim_res_ic_t1-11.RDS"))
#
# exp(summary(log(resdf_ic_t11$estimate)))
#
# perf_tab_RR <- d %>% group_by(analysis) %>%
#   mutate(variance=mean((log(estimate)-mean(log(estimate)))^2),
#          RD.variance=mean((mean(ate)-ate)^2),
#          o.ci.lb = log(estimate) - 1.96 * sqrt(variance),
#          o.ci.ub = log(estimate) + 1.96 * sqrt(variance)) %>%
#   summarize(
#     bias=mean(log(estimate))-log(true.RR),
#     variance=mean((mean(log(estimate))-log(estimate))^2),
#     mse = bias^2 + variance,
#     bias_se_ratio= bias/sqrt(variance),
#     coverage=mean(CI.2.5.<=true.RR & true.RR<=CI.97.5.)*100,
#     #oracle coverage
#     o.coverage=mean(o.ci.lb<=log(true.RR) & log(true.RR)<= o.ci.ub)*100
#   ) %>% filter(!is.na(variance)) %>%
#   distinct()
#
# perf_tab_RR
#
# perf_tab_diff <- d %>% filter(!is.na(ate)) %>%
#   subset(., select = -c(estimate, std.dev, CI.2.5., CI.97.5.)) %>%
#   rename(estimate= ate, std.dev= ate.sd, CI.2.5.=ate.ci.lb, CI.97.5.=ate.ci.ub)%>%
#   group_by(analysis) %>%
#   mutate(variance=mean((estimate-mean(estimate))^2),
#          o.ci.lb = estimate - 1.96 * sqrt(variance),
#          o.ci.ub = estimate + 1.96 * sqrt(variance)) %>%
#   summarize(
#     bias=mean((estimate))-(true.RD),
#     variance=mean((estimate-mean(estimate))^2),
#     mse = bias^2 + variance,
#     bias_se_ratio= bias/sqrt(variance),
#     coverage=mean(CI.2.5.<=true.RD & true.RD<=CI.97.5.)*100,
#     #oracle coverage
#     o.coverage=mean(o.ci.lb<=true.RD & true.RD<= o.ci.ub)*100,
#     mean_ci_width=mean((CI.97.5.)-(CI.2.5.))
#   ) %>%
#   distinct()
#
# perf_tab_RR
# perf_tab_diff
