
rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))
<<<<<<< HEAD
source(paste0(here::here(),"/simulation study/sim_simple/0_simple_sim_functions.R"))

library(parallel)
library(doParallel)
registerDoParallel(cores=50)
=======

library(parallel)
library(doParallel)
registerDoParallel(cores=64)
>>>>>>> b8a879fbe6a71830cba3e7f49e7c904231c09262

d_wide_list <- readRDS(file=here("data/simulated_data_list_simple.RDS"))


<<<<<<< HEAD
#run IC and TMLE
resdf_ic_glm <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  res <- run_ltmle_simple(d_wide_list[[i]],  varmethod = "ic", SL.library = c("glm"))
  return(res)}
head(resdf_ic_glm)
summary(resdf_ic_glm$estimate)

resdf_tmle_glm <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  res <- run_ltmle_simple(d_wide_list[[i]],  varmethod = "tmle", SL.library = c("glm"))
  return(res)}

#use robust if necessary
i<-1
resdf_ic_glmnet <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  res <- run_ltmle_simple(d_wide_list[[i]],  varmethod = "ic", SL.library = c("SL.glmnet"))
  return(res)}
head(resdf_ic_glm)
summary(resdf_ic_glm$estimate)

resdf_tmle_glmnet <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  res <- run_ltmle_simple(d_wide_list[[i]],  varmethod = "tmle", SL.library = c("SL.glmnet"))
  return(res)}


save(resdf_ic_glm, resdf_tmle_glm, resdf_ic_glmnet, resdf_tmle_glmnet, file=here("sim_res/simulated_data_list_simple.Rdata"))

=======

run_ltmle_simple <- function(d,
                             SL.library = c("SL.glm"),
                             varmethod = "tmle", #variance method
                             id=NULL){
  abar_spec = list(rep(1,2),rep(0,2))
  set.seed(12345)
  fit <- res <- NULL
  try(fit <- ltmle(data=d,
                   Anodes = c("A1","A2"),
                   Lnodes = c("L2"),
                   Ynodes = c("Y2","Y3"),
                   survivalOutcome = T,
                   abar = abar_spec,
                   estimate.time=F,
                   SL.library = SL.library,
                   variance.method = varmethod,
                   id=id))

  if(!is.null(fit)){
    res <- summary(fit)
    res.iptw <- summary(fit, estimator="iptw")
    res.RR <- as.data.frame(res$effect.measures$RR)
    res.ate <- as.data.frame(res$effect.measures$ATE) %>% rename(ate.long.name=long.name,ate=estimate, ate.sd=std.dev , ate.pval=pvalue, ate.ci.lb=CI.2.5., ate.ci.ub=  CI.97.5., ate.log.std.err=log.std.err)

    res.RR.iptw <- as.data.frame(res.iptw$effect.measures$RR) %>% rename(iptw.long.name=long.name, iptw.estimate=estimate, iptw.sd=std.dev , iptw.pval=pvalue, iptw.ci.lb=CI.2.5., iptw.ci.ub=  CI.97.5., iptw.log.std.err=log.std.err)
    res.ate.iptw <- as.data.frame(res$effect.measures$ATE) %>% rename(iptw.ate.long.name=long.name, iptw.ate=estimate, iptw.ate.sd=std.dev , iptw.ate.pval=pvalue, iptw.ate.ci.lb=CI.2.5., iptw.ate.ci.ub=  CI.97.5., iptw.ate.log.std.err=log.std.err)

    res <- cbind(res.RR, res.ate, res.RR.iptw, res.ate.iptw)
  }
  return(res)
}

#run IC and TMLE
resdf_ic_glm <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  res <- run_ltmle_simple(d_wide_list[[i]],  varmethod = "ic", SL.library = c("SL.glm"))
  return(res)}
head(resdf_ic_glm)
summary(resdf_ic_glm)

resdf_tmle_glm <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  run_ltmle_simple(d_wide_list[[i]],  varmethod = "tmle", SL.library = c("SL.glm"))}


>>>>>>> b8a879fbe6a71830cba3e7f49e7c904231c09262
