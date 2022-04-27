#sink(file=paste0(here::here(),"/simulation study/sim_TCP1_glp1_vs_any_static-IC.Rout"),append=F)

rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))




gc()
d_wide_list <- readRDS(file=here("data/simulated_data_list.RDS"))

d_wide_list <- d_wide_list[1:50]
gc()


# res <- run_ltmle_glmnet(d_wide_list[[1]], varmethod = "ic", resdf=NULL, Qint=FALSE, gcomp=TRUE)
#
# int.start.time <- Sys.time()
# resdf_gcomp <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
#   res <- NULL
#   try(res <- run_ltmle_glmnet(d_wide_list[[i]], varmethod = "ic", resdf=NULL, Qint=FALSE, gcomp=TRUE))
#   return(res)
# }
# int.end.time <- Sys.time()
# difftime(int.end.time, int.start.time, units="mins")
#
#
#
#
# gc()
# saveRDS(resdf_gcomp, paste0(here::here(),"/data/sim_res_gcomp.RDS"))

#sink()



d = d_wide_list[[1]]
N_time = 2
SL.library = c("SL.glmnet")
resdf=NULL
Qint=F
gcomp=T
det.Q=FALSE
varmethod = "ic"
override_function=SuperLearner_override
varmethod = "tmle"
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
                              Yvars=c("event_dementia_"))
  abar_spec = list(rep(1,N_time),rep(0,N_time))

  #Drop the baseline events
  spec_ltmle$data <- spec_ltmle$data %>% subset(., select = -c(event_death_0, censor_0, event_dementia_0))
  spec_ltmle$Cnodes = spec_ltmle$Cnodes[spec_ltmle$Cnodes!="censor_0"]
  spec_ltmle$Lnodes = spec_ltmle$Lnodes[spec_ltmle$Lnodes!="event_death_0"]
  spec_ltmle$Ynodes = spec_ltmle$Ynodes[spec_ltmle$Ynodes!="event_dementia_0"]

  set.seed(12345)
  res = NULL


  if(Qint){

    if(N_time==11){
      qform = c(
        insulin_0="Q.kplus1 ~ 1",
        insulin_1="Q.kplus1 ~ 1",
        event_dementia_1="Q.kplus1 ~ 1",
        insulin_2="Q.kplus1 ~ 1",
        event_dementia_2="Q.kplus1 ~ 1",
        insulin_3="Q.kplus1 ~ 1",
        event_dementia_3="Q.kplus1 ~ 1",
        insulin_4="Q.kplus1 ~ 1",
        event_dementia_4="Q.kplus1 ~ 1",
        insulin_5="Q.kplus1 ~ 1",
        event_dementia_5="Q.kplus1 ~ 1",
        insulin_6="Q.kplus1 ~ 1",
        event_dementia_6="Q.kplus1 ~ 1",
        insulin_7="Q.kplus1 ~ 1",
        event_dementia_7="Q.kplus1 ~ 1",
        insulin_8="Q.kplus1 ~ 1",
        event_dementia_8="Q.kplus1 ~ 1",
        insulin_9="Q.kplus1 ~ 1",
        event_dementia_9="Q.kplus1 ~ 1",
        insulin_10="Q.kplus1 ~ 1",
        event_dementia_10="Q.kplus1 ~ 1"
      )
    }

    if(N_time==2){
      qform = c(
        insulin_0="Q.kplus1 ~ 1",
        insulin_1="Q.kplus1 ~ 1",
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


  package_stub("SuperLearner", "SuperLearner", override_function, {
    testthatsomemore::package_stub("ltmle", "Estimate", Estimate_override, {
      try(res <- ltmle(data=spec_ltmle$data,
                       Anodes = spec_ltmle$Anodes,
                       Cnodes = spec_ltmle$Cnodes,
                       Lnodes = spec_ltmle$Lnodes,
                       Ynodes = spec_ltmle$Ynodes,
                       survivalOutcome = T,
                       abar = abar_spec,
                       gcomp=gcomp,
                       Qform = qform,
                       estimate.time=T,
                       deterministic.Q.function = det.q.fun,
                       SL.library = SL.library,
                       variance.method = varmethod
      ))
    })})


  package_stub("SuperLearner", "SuperLearner", override_function, {
    testthatsomemore::package_stub("ltmle", "Estimate", Estimate_override, {
      try(res <- ltmle(data=spec_ltmle$data,
                       Anodes = spec_ltmle$Anodes,
                       Cnodes = spec_ltmle$Cnodes,
                       Lnodes = spec_ltmle$Lnodes,
                       Ynodes = spec_ltmle$Ynodes,
                       survivalOutcome = T,
                       abar = abar_spec,
                       SL.library = SL.library
      ))
    })})


  if(!is.null(res)){
    fit<-res
    res <- summary(res)
    res <- as.data.frame(res$effect.measures$RR)
    res$label <- label
  }
  if(!is.null(resdf)){
    res <- bind_rows(resdf, res)
  }

  options(warn=warn)
  return(res)


