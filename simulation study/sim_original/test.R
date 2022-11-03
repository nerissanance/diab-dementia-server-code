





rm(list=ls())
library(lava)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))
source(paste0(here::here(),"/simulation study/0_simulation_cleaning_functions.R"))
cc <- fread(paste0(here::here(),"/data/coefficients.txt"))
u <- synthesizeDD(cc)

set.seed(12345)

# >   (cRD <-  prop.always[2] - prop.never[2])
# TRUE
# -0.005929
# >   (cRR <- (prop.always[2])/(prop.never[2]))
# TRUE
# 0.6924793

set.seed(12345)
  d <- sim(u,115698)




  d<- clean_sim_data(d, 10)




mean(1*(d$event_dementia_1 +
          d$event_dementia_2 +
          d$event_dementia_3 +
          d$event_dementia_4 +
          d$event_dementia_5 +
          d$event_dementia_6 +
          d$event_dementia_7 +
          d$event_dementia_8 +
          d$event_dementia_9 +
          d$event_dementia_10 >0))


#run_ltmle_glmnet()

N_time = 11 #number of time points you want to look at
SL.library = c("SL.glmnet")
resdf=NULL
Qint=F
gcomp=F
det.Q=F
gbound = c(0.01, 1)
override_function=SuperLearner_override
varmethod = "ic" #variance method
label=""
glm=FALSE
id=NULL

  warn = getOption("warn")
  options(warn=-1)

  #clean competing events
  #d <-clean_sim_data(d, N_time=N_time)


  #Use only first N time points
  d <- d %>%
    dplyr::select(!!(baseline_vars),matches(paste0("_(",paste0(0:(N_time-1),collapse="|"),")$")))


  spec_ltmle <- spec_analysis(data=d, c(long_covariates,"event_death_"),
                              baseline_vars, N_time,
                              Avars=c("glp1_"),
                              Yvars=c("event_dementia_"),
                              Cvars=c("censor_"))
  abar_spec = list(rep(1,N_time-1),rep(0,N_time-1))

  # #Drop the baseline events
  set.seed(12345)
  fit = NULL
    qform=NULL
    det.q.fun = NULL

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
                         estimate.time=F,
                         deterministic.Q.function = det.q.fun,
                         SL.library = SL.library,
                         variance.method = varmethod,
                         id=id
        ))
      })})


    res <- summary(fit)
    res.RR <- as.data.frame(res$effect.measures$RR)
    res.ate <- as.data.frame(res$effect.measures$ATE) %>% rename(ate.long.name=long.name,ate=estimate, ate.sd=std.dev , ate.pval=pvalue, ate.ci.lb=CI.2.5., ate.ci.ub=  CI.97.5., ate.log.std.err=log.std.err)

    res.RR
    res.ate
