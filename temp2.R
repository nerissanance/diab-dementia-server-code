

d=d_wide_list[[1]]
N_time = 3 #number of time points you want to look at
SL.library = c("SL.glmnet")
resdf=NULL
Qint=T
gcomp=F
det.Q=F
gbound = c(0.01, 1)
override_function=SuperLearner_override
varmethod = "ic" #variance method
alt=FALSE
label=""


warn = getOption("warn")
options(warn=-1)

#clean competing events
d <-clean_sim_data(d, N_time=N_time)

#Use only first N time points
d <- d %>%
  dplyr::select(!!(baseline_vars),matches(paste0("_(",paste0(0:(N_time-1),collapse="|"),")$")))


spec_ltmle <- spec_analysis(data=d, c(long_covariates,"event_death_"),
                            baseline_vars, N_time,
                            Avars=c("glp1_"),
                            Yvars=c("event_dementia_"),
                            alt=alt)
abar_spec = list(rep(1,N_time),rep(0,N_time))

# #Drop the baseline events
spec_ltmle$data <- spec_ltmle$data %>% subset(., select = -c(event_death_0, censor_0, event_dementia_0))
spec_ltmle$Cnodes = spec_ltmle$Cnodes[spec_ltmle$Cnodes!="censor_0"]
spec_ltmle$Lnodes = spec_ltmle$Lnodes[spec_ltmle$Lnodes!="event_death_0"]
spec_ltmle$Ynodes = spec_ltmle$Ynodes[spec_ltmle$Ynodes!="event_dementia_0"]

df<-spec_ltmle$data
table(df$glp1_0, df$glp1_1)
df <- df %>% subset(., select = -c(glp1_0))

set.seed(12345)
fit = NULL

abar_spec = list(rep(1,N_time-1),rep(0,N_time-1))


package_stub("SuperLearner", "SuperLearner", override_function, {
  testthatsomemore::package_stub("ltmle", "Estimate", Estimate_override, {
    try(fit <- ltmle(data=df,
                     Anodes = spec_ltmle$Anodes[-1],
                     Cnodes = spec_ltmle$Cnodes,
                     Lnodes = spec_ltmle$Lnodes,
                     Ynodes = spec_ltmle$Ynodes,
                     gbound=gbound,
                     survivalOutcome = T,
                     abar = abar_spec,
                     gcomp=gcomp,
                     Qform = NULL,
                     estimate.time=T,
                     deterministic.Q.function = NULL,
                     SL.library = SL.library,
                     variance.method = varmethod
    ))
  })})



  res <- summary(fit)
  res.iptw <- summary(fit, estimator="iptw")
  res.RR <- as.data.frame(res$effect.measures$RR)
  res.ate <- as.data.frame(res$effect.measures$ATE) %>% rename(ate.long.name=long.name,ate=estimate, ate.sd=std.dev , ate.pval=pvalue, ate.ci.lb=CI.2.5., ate.ci.ub=  CI.97.5., ate.log.std.err=log.std.err)

  res.RR.iptw <- as.data.frame(res.iptw$effect.measures$RR) %>% rename(iptw.long.name=long.name, iptw.estimate=estimate, iptw.sd=std.dev , iptw.pval=pvalue, iptw.ci.lb=CI.2.5., iptw.ci.ub=  CI.97.5., iptw.log.std.err=log.std.err)
  res.ate.iptw <- as.data.frame(res$effect.measures$ATE) %>% rename(iptw.ate.long.name=long.name, iptw.ate=estimate, iptw.ate.sd=std.dev , iptw.ate.pval=pvalue, iptw.ate.ci.lb=CI.2.5., iptw.ate.ci.ub=  CI.97.5., iptw.ate.log.std.err=log.std.err)

  res <- cbind(res.RR, res.ate, res.RR.iptw, res.ate.iptw)
  res$label <- label
   res
