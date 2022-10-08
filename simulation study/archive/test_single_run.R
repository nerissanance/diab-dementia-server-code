
rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))




gc()
d_wide_list <- readRDS(file=here("data/null_simulated_data_list.RDS"))

d_wide_list <- d_wide_list[1:200]
d=d_wide_list[[1]]
resdf=NULL
Qint=F
det.Q=FALSE
varmethod = "ic"
N_time=2


SL.library = c("SL.glmnet")
resdf=NULL
gcomp=F
gbound = c(0.01, 1)
override_function=SuperLearner_override
label=""
id=NULL

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


  spec_ltmle <- spec_analysis(data=d, c(long_covariates,"event_death_"),
                              baseline_vars, N_time,
                              Avars=c("glp1_"),
                              Yvars=c("event_dementia_"),
                              Cvars=c("censor_"))
  abar_spec = list(rep(1,N_time-1),rep(0,N_time-1))

  set.seed(12345)
  fit = NULL



    qform=NULL



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
                       Cnodes = spec_ltmle$Cnodes,
                       Lnodes = spec_ltmle$Lnodes,
                       Ynodes = spec_ltmle$Ynodes,
                       gbound=gbound,
                       survivalOutcome = T,
                       abar = abar_spec,
                       gcomp=gcomp,
                       Qform = qform,
                       estimate.time=T,
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
