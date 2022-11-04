

rm(list=ls())
library(lava)
library(data.table)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))
source(paste0(here::here(),"/synthesizeDD.R"))

cc <- fread(paste0(here::here(),"/data/coefficients.txt"))
u <- synthesizeDD(cc)

set.seed(12345)
sim_list <- NULL
n<-200
N_time=10


library(parallel)
library(doParallel)
registerDoParallel(cores=64)


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

  qform=NULL
  det.q.fun = NULL


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


resdf_ic_t11_iter1000  <- foreach(i = 1:1000, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  set.seed(i*2 + 400)
  res <- NULL
  df <- sim(u,115698)
  try(res <- run_ltmle_glmnet_test(d=df,  N_time=11))
  return(res)
}

saveRDS(resdf_ic_t11_iter1000, paste0(here::here(),"/data/sim_res_noDetQ_ic_iter1000.RDS"))




resdf_ic_t11_iter1000$analysis="Y10"
resdf_ic_t11_iter1000$true.RR <-  0.6924793
resdf_ic_t11_iter1000$true.RD <- (-0.005929)



exp(summary(log(resdf_ic_t11_iter1000$estimate)))
d<-resdf_ic_t11_iter1000
perf_tab_RR <- d %>% group_by(analysis) %>%
  mutate(variance=mean((log(estimate)-mean(log(estimate)))^2),
         RD.variance=mean((mean(ate)-ate)^2),
         o.ci.lb = log(estimate) - 1.96 * sqrt(variance),
         o.ci.ub = log(estimate) + 1.96 * sqrt(variance)) %>%
  summarize(
    bias=mean(log(estimate))-log(true.RR),
    variance=mean((mean(log(estimate))-log(estimate))^2),
    mse = bias^2 + variance,
    bias_se_ratio= bias/sqrt(variance),
    coverage=mean(CI.2.5.<=true.RR & true.RR<=CI.97.5.)*100,
    #oracle coverage
    o.coverage=mean(o.ci.lb<=log(true.RR) & log(true.RR)<= o.ci.ub)*100
  ) %>% filter(!is.na(variance)) %>%
  distinct()

perf_tab_RR

perf_tab_diff <- d %>% filter(!is.na(ate)) %>%
  subset(., select = -c(estimate, std.dev, CI.2.5., CI.97.5.)) %>%
  rename(estimate= ate, std.dev= ate.sd, CI.2.5.=ate.ci.lb, CI.97.5.=ate.ci.ub)%>%
  group_by(analysis) %>%
  mutate(variance=mean((estimate-mean(estimate))^2),
         o.ci.lb = estimate - 1.96 * sqrt(variance),
         o.ci.ub = estimate + 1.96 * sqrt(variance)) %>%
  summarize(
    bias=mean((estimate))-(true.RD),
    variance=mean((estimate-mean(estimate))^2),
    mse = bias^2 + variance,
    bias_se_ratio= bias/sqrt(variance),
    coverage=mean(CI.2.5.<=true.RD & true.RD<=CI.97.5.)*100,
    #oracle coverage
    o.coverage=mean(o.ci.lb<=true.RD & true.RD<= o.ci.ub)*100,
    mean_ci_width=mean((CI.97.5.)-(CI.2.5.))
  ) %>%
  distinct()

perf_tab_RR
perf_tab_diff
