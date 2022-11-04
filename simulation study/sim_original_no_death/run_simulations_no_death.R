

rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))


library(parallel)
library(doParallel)
registerDoParallel(cores=64)

gc()
d_wide_list <- readRDS(file=here("data/simulated_data_list_no_death.RDS"))
d_wide_list <- d_wide_list[1:200]
gc()


d=d_wide_list[[i]]
N_time = 3
SL.library = c("SL.glmnet")
resdf=NULL
Qint=F
gcomp=F
det.Q=T
gbound = c(0.01, 1)
override_function=SuperLearner_override
varmethod = "ic"
label=""
glm=FALSE
id=NULL



spec_analysis_sim_no_death <- function(data, long_covariates, baseline_vars, N_time, Avars=c("glp1_"), Yvars=c("event_dementia_"), Cvars=NULL, alt=FALSE){

  if(!alt){
    node_names <- spec_nodes(baseline_vars=baseline_vars,
                             longitudinal_vars=c(Avars,"censor_",long_covariates,Yvars),
                             num_time=0:(N_time-1))
  }else{
    node_names <- spec_nodes(baseline_vars=baseline_vars,
                             longitudinal_vars=c(Avars,"censor_",Yvars, long_covariates),
                             num_time=0:(N_time-1))
  }


  Lnode_names <- c(baseline_vars, expand.grid(long_covariates,0:(N_time-1)) %>% apply(1, function(row) paste0(row, collapse = "")))
  Lnode_names <- gsub(" ","", Lnode_names)
  # for(i in long_covariates){
  #   Lnode_names <- Lnode_names[!grepl(paste0(i, (N_time-1)), Lnode_names)]
  # }

  #subset to analysis columns and arrange
  d_ltmle <- data %>% dplyr::select(!!(node_names))
  colnames(d_ltmle)

  #clean censoring nodes to right format
  Cnode_names = node_names[grep("^censor", node_names)]
  for(i in Cnode_names){
    d_ltmle[[i]] <- BinaryToCensoring(is.censored=d_ltmle[[i]])
  }

  d_ltmle <- d_ltmle %>% subset(., select = -c(censor_0, event_dementia_0))
  d_ltmle <- d_ltmle %>% select(  ie_type,                      age_base,                     sex,                          code5txt,                     quartile_income,
                                  insulin_0,                    any.malignancy_0,             chronic.pulmonary.disease_0,  hypertension_0,               myocardial.infarction_0,
                                  ischemic.heart.disease_0,     heart.failure_0, renal.disease_0, sglt2_inhib_0, glp1_0,   everything())



  return(list(
    data=d_ltmle,
    node_names=node_names,
    Anodes = node_names[sort(grep(paste("^",Avars, collapse="|", sep=""), node_names))],
    Cnodes = Cnode_names,
    Lnodes = Lnode_names,
    Ynodes = node_names[sort(grep(paste("^",Yvars, collapse="|", sep=""), node_names))]
  ))
}

run_ltmle_glmnet_test_no_death <- function(d, N_time = 3,
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


  if(!is.null(id)){
    baseline_vars <- c(baseline_vars,"id")
  }

  #Use only first N time points
  d <- d %>%
    dplyr::select(!!(baseline_vars),matches(paste0("_(",paste0(0:(N_time-1),collapse="|"),")$")))


  spec_ltmle <- spec_analysis_sim_no_death(data=d, c(long_covariates),
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
                       Lnodes = spec_ltmle$Lnodes,
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


#Ntime=3
resdf_ic_t3  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test_no_death(d=d_wide_list[[i]],  N_time=3))
  return(res)
}
exp(summary(log(resdf_ic_t3$estimate)))


#Ntime=11
resdf_ic_t11  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test_no_death(d=d_wide_list[[i]],  N_time=11))
  return(res)
}

resdf_ic_t11$analysis="Y10"
resdf_ic_t11$true.RR <-  0.3930283
resdf_ic_t11$true.RD <- (-0.005929)



#d <- bind_rows(resdf_ic_t2, resdf_ic_t3, resdf_ic_t4, resdf_ic_t5, resdf_ic_t6, resdf_ic_t7, resdf_ic_t8, resdf_ic_t11)
d <- bind_rows(resdf_ic_t11) %>% mutate(analysis=factor(analysis, levels=unique(analysis)))


exp(summary(log(resdf_ic_t11$estimate)))

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
