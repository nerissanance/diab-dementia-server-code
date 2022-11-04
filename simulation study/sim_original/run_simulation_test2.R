
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


run_ltmle_glmnet_test <- function(d, N_time = 3,
                                       SL.library = c("SL.glmnet"),
                                       resdf=NULL,
                                       Qint=F,
                                       gcomp=F,
                                       det.Q=T,
                                       gbound = c(0.01, 1),
                                       override_function=SuperLearner_override,
                                       varmethod = "ic", #variance method
                                       label="",
                                       glm=FALSE,
                                       id=NULL){



warn = getOption("warn")
options(warn=-1)

#clean competing events
d <-clean_sim_data(d, N_time=N_time)

if(!is.null(id)){
  baseline_vars <- c(baseline_vars,"id")
}

#Use only first N time points
d <- d %>%
  dplyr::select(!!(baseline_vars),matches(paste0("_(",paste0(0:(N_time-1),collapse="|"),")$")))


spec_ltmle <- spec_analysis_sim(data=d, c(long_covariates,"event_death_"),
                                baseline_vars, N_time,
                                Avars=c("glp1_"),
                                Yvars=c("event_dementia_"),
                                Cvars=c("censor_"))
#abar_spec = list(rep(1,N_time-1),rep(0,N_time-1))
abar_spec = list(rep(1,N_time),rep(0,N_time))

set.seed(12345)
fit = NULL

if(Qint){
if(N_time==11){

  qform = c(
    insulin_0="Q.kplus1 ~ 1",
    event_dementia_1="Q.kplus1 ~ 1",
    event_dementia_2="Q.kplus1 ~ 1",
    event_dementia_3="Q.kplus1 ~ 1",
    event_dementia_4="Q.kplus1 ~ 1",
    event_dementia_5="Q.kplus1 ~ 1",
    event_dementia_6="Q.kplus1 ~ 1",
    event_dementia_7="Q.kplus1 ~ 1",
    event_dementia_8="Q.kplus1 ~ 1",
    event_dementia_9="Q.kplus1 ~ 1",
    event_dementia_10="Q.kplus1 ~ 1"
  )
}
if(N_time==4){
  qform = c(
    insulin_0="Q.kplus1 ~ 1",
    event_dementia_1="Q.kplus1 ~ 1",
    event_dementia_2="Q.kplus1 ~ 1",
    event_dementia_3="Q.kplus1 ~ 1")
}
if(N_time==2){
  qform = c(
    insulin_0="Q.kplus1 ~ 1",
    event_dementia_1="Q.kplus1 ~ 1")
}
}else{
  qform=NULL
}


if(det.Q){
  det.q.fun = det.Q.function
}else{
  det.q.fun = NULL
}

if(!is.null(id)){
  id <- spec_ltmle$data[["id"]]
}



package_stub("SuperLearner", "SuperLearner", override_function, {
  testthatsomemore::package_stub("ltmle", "Estimate", Estimate_override, {
    try(fit <- ltmle(data=spec_ltmle$data,
                     Anodes = spec_ltmle$Anodes,
                     Cnodes = spec_ltmle$Cnodes[-1],
                     Lnodes = spec_ltmle$Lnodes[spec_ltmle$Lnodes!="event_death_0"],
                     Ynodes = spec_ltmle$Ynodes[-1],
                     gbound=gbound,
                     survivalOutcome = T,
                     abar = abar_spec,
                     gcomp=gcomp,
                     Qform = qform,
                     estimate.time=F,
                     deterministic.Q.function = det.q.fun,
                     SL.library = SL.library,
                     variance.method = varmethod,
                     id=id
    ))
  })})


if(!is.null(fit)){
  res <- summary(fit)
  res.iptw <- summary(fit, estimator="iptw")
  res.RR <- as.data.frame(res$effect.measures$RR)
  res.ate <- as.data.frame(res$effect.measures$ATE) %>% rename(ate.long.name=long.name,ate=estimate, ate.sd=std.dev , ate.pval=pvalue, ate.ci.lb=CI.2.5., ate.ci.ub=  CI.97.5., ate.log.std.err=log.std.err)

  res.RR.iptw <- as.data.frame(res.iptw$effect.measures$RR) %>% rename(iptw.long.name=long.name, iptw.estimate=estimate, iptw.sd=std.dev , iptw.pval=pvalue, iptw.ci.lb=CI.2.5., iptw.ci.ub=  CI.97.5., iptw.log.std.err=log.std.err)
  res.ate.iptw <- as.data.frame(res$effect.measures$ATE) %>% rename(iptw.ate.long.name=long.name, iptw.ate=estimate, iptw.ate.sd=std.dev , iptw.ate.pval=pvalue, iptw.ate.ci.lb=CI.2.5., iptw.ate.ci.ub=  CI.97.5., iptw.ate.log.std.err=log.std.err)

  res <- cbind(res.RR, res.ate, res.RR.iptw, res.ate.iptw)
  res$label <- label
}
if(!is.null(resdf)){
  res <- bind_rows(resdf, res)
}

options(warn=warn)
return(res)
}




# #Ntime=11
# resdf_ic_t11  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
#   res <- NULL
#   try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]],  N_time=11))
#   return(res)
# }
# saveRDS(resdf_ic_t11, paste0(here::here(),"/data/sim_res_noDetQ_ic_v3.RDS"))

resdf_Qint_ic_t11  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]],  Qint=T, N_time=11))
  return(res)
}
saveRDS(resdf_Qint_ic_t11, paste0(here::here(),"/data/sim_res_Qint_noDetQ_ic_v3.RDS"))


resdf_DetQ_ic_t11  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]], det.Q=T, N_time=11))
  return(res)
}
saveRDS(resdf_DetQ_ic_t11, paste0(here::here(),"/data/sim_res_DetQ_ic_v3.RDS"))


#Ntime=11
# resdf_tmle_t11  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
#   res <- NULL
#   try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]],  N_time=11, varmethod = "tmle"))
#   return(res)
# }
# saveRDS(resdf_tmle_t11, paste0(here::here(),"/data/sim_res_noDetQ_tmle_v3.RDS"))





#SuperLearner_override_ridge
resdf_ic_ridge_t11  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]],  N_time=11, override_function=SuperLearner_override_ridge))
  return(res)
}
saveRDS(resdf_ic_ridge_t11, paste0(here::here(),"/data/sim_res_ridge_ic_v3.RDS"))

#SuperLearner_override_lasso_prescreen
resdf_ic_lasso_prescreen_t11  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]],  N_time=11, override_function=SuperLearner_override_lasso_prescreen))
  return(res)
}
saveRDS(resdf_ic_lasso_prescreen_t11, paste0(here::here(),"/data/sim_res_lasso_prescreen_ic_v3.RDS"))



#SuperLearner_override_ridge_1se
resdf_ic_ridge_1se_t11  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]],  N_time=11, override_function=SuperLearner_override_ridge_1se))
  return(res)
}
saveRDS(resdf_ic_ridge_1se_t11, paste0(here::here(),"/data/sim_res_ridge_1se_ic_v3.RDS"))


#SuperLearner_override_1se
resdf_ic_1se_t11  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]],  N_time=11, override_function=SuperLearner_override_1se))
  return(res)
}
saveRDS(resdf_ic_1se_t11, paste0(here::here(),"/data/sim_res_1se_ic_v3.RDS"))


#KEEP running these:

#SuperLearner_override_EN_1se
resdf_ic_EN_1se_t11  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]],  N_time=11, override_function=SuperLearner_override_EN_1se))
  return(res)
}
saveRDS(resdf_ic_EN_1se_t11, paste0(here::here(),"/data/sim_res_EN_1se_ic_v3.RDS"))

#SuperLearner_override_AUC
resdf_ic_AUC_t11  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]],  N_time=11, override_function=SuperLearner_override_AUC))
  return(res)
}
saveRDS(resdf_ic_AUC_t11, paste0(here::here(),"/data/sim_res_AUC_ic_v3.RDS"))


#SuperLearner_override_RF
resdf_ic_RF_t11  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]],  N_time=11, override_function=SuperLearner_override_RF))
  return(res)
}
saveRDS(resdf_ic_RF_t11, paste0(here::here(),"/data/sim_res_RF_ic_v3.RDS"))



#SuperLearner_override_ridge_AUC
resdf_ic_ridge_AUC_t11  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]],  N_time=11, override_function=resdf_ic_ridge_AUC_t11))
  return(res)
}
saveRDS(resdf_ic_ridge_AUC_t11, paste0(here::here(),"/data/sim_res_ridge_AUC_ic_v3.RDS"))


# SuperLearner_override_EN_AUC
# SuperLearner_override_ridge_AUC
# SuperLearner_override_AUC_1se


# save(resdf_ic_t2, resdf_ic_t3, resdf_ic_t4, resdf_ic_t5, resdf_ic_t6, resdf_ic_t7, resdf_ic_t8, resdf_ic_t9, resdf_ic_t10, resdf_ic_t11, file=paste0(here::here(),"/data/sim_res_ic_t1-11.Rdata"))
# saveRDS(resdf_ic_t11, paste0(here::here(),"/data/sim_res_noDetQ_ic_v3.RDS"))
# saveRDS(d, paste0(here::here(),"/data/sim_res_ic_t1-11.RDS"))
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
