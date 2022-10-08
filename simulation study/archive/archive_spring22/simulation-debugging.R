



# - to do:
#debug on simple ltmle dataset

#compare AUC vs default
#make is so SL runs in 10-20 parallelized chunks, then saves


#run LTMLE + LTMLE with override on example data from LTMLE package examples to debug, comparing LASSO and EN and IC vs tmle.

#sink(file=paste0(here::here(),"/simulation study/sim_TCP1_glp1_vs_any_static-IC.Rout"),append=F)

rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))





d_wide_list <- readRDS(file=here("data/simulated_data_list.RDS"))
d_wide <- d_wide_list[[1]]

#subset to glp1 or smaller data
d_wide <- d_wide %>% group_by(event_dementia_2) %>% slice(1:1000) %>% ungroup()
table(d_wide$glp1_2, d_wide$event_dementia_2)
prop.table(table(d_wide$glp1_2, d_wide$event_dementia_2),1)

res = NULL
res <- run_ltmle_glmnet(d_wide, varmethod = "ic", resdf=NULL, Qint=F, N_time = 2,  override_function=SuperLearner_override, label="0- min")
res2 <- run_ltmle_glmnet(d_wide, varmethod = "tmle", resdf=NULL, Qint=F, N_time = 2,  override_function=SuperLearner_override, label="0- min")
res3 <- run_ltmle_glmnet(d_wide, varmethod = "tmle", resdf=NULL, gcomp=T, Qint=F, N_time = 2,  override_function=SuperLearner_override, label="0- min")

fit<-res$fit
fit2<-res2$fit
fit3<-res3$fit

summary(fit)
summary(fit2)
#why do both say Estimator:  tmle
fit$IC

#variance.method="tmle" or "iptw" are not yet available with non-binary outcomes, gcomp=TRUE, stratify=TRUE, or deterministic.Q.function.


#res <- run_ltmle_glmnet(d_wide, varmethod = "ic", resdf=res, Qint=F, N_time = 2,  override_function=SuperLearner_override(alpha=0.5), label="0- min")

res <- run_ltmle_glmnet(d_wide, varmethod = "ic", resdf=res, Qint=F, N_time = 2,  override_function=SuperLearner_override_EN, label="0- min")
res <- run_ltmle_glmnet(d_wide, varmethod = "ic", resdf=res, Qint=F, N_time = 2,  override_function=SuperLearner_override_1se, label="0- min")
res <- run_ltmle_glmnet(d_wide, varmethod = "ic", resdf=res, Qint=F, N_time = 2,  override_function=SuperLearner_override_AUC, label="0- min")
res <- run_ltmle_glmnet(d_wide, varmethod = "ic", resdf=res, Qint=F, N_time = 2,  override_function=SuperLearner_override_EN_AUC, label="0- min")

res

SuperLearner_override_AUC

res <- run_ltmle_glmnet(d_wide, varmethod = "tmle", resdf=res, Qint=F, N_time = 2, useMin=TRUE, label="0- min")
res <- run_ltmle_glmnet(d_wide, varmethod = "ic", resdf=res, Qint=F, N_time = 2, useMin=FALSE, label="0- 1se")
res <- run_ltmle_glmnet(d_wide, varmethod = "tmle", resdf=res, Qint=F, N_time = 2, useMin=FALSE, label="0- 1se")
res <- run_ltmle_glmnet(d_wide, varmethod = "ic", resdf=res, Qint=F, elastic.net=T, N_time = 2, useMin=T, label="0- EN-min")
res <- run_ltmle_glmnet(d_wide, varmethod = "tmle", resdf=res, Qint=F, elastic.net=T, N_time = 2, useMin=T, label="0- EN-min")
res <- run_ltmle_glmnet(d_wide, varmethod = "ic", resdf=res, Qint=F, elastic.net=T, N_time = 2, useMin=FALSE, label="0- EN-1se")
res <- run_ltmle_glmnet(d_wide, varmethod = "tmle", resdf=res, Qint=F, elastic.net=T, N_time = 2, useMin=FALSE, label="0- EN-1se")
res

res <- run_ltmle_glmnet(d_wide, varmethod = "ic", resdf=res, Qint=T, elastic.net=T, N_time = 2, useMin=TRUE, label="0- Qint EN")
res <- run_ltmle_glmnet(d_wide, varmethod = "tmle", resdf=res, Qint=T, elastic.net=F, N_time = 2, useMin=TRUE, label="0-")
res <- run_ltmle_glmnet(d_wide, varmethod = "tmle", resdf=res, Qint=F, N_time = 2, useMin=TRUE, label="0- min-tmle")
res


res <- run_ltmle_glmnet(d_wide, varmethod = "ic", resdf=res, Qint=T, N_time = 2, label="1- Qint, ic, lasso")
res <- run_ltmle_glmnet(d_wide, varmethod = "tmle", resdf=res, Qint=T, N_time = 2, label="2- Qint, tmle, lasso")
res <- run_ltmle_glmnet(d_wide, varmethod = "ic", resdf=res, Qint=FALSE, N_time = 2, label="3- tmle")
res <- run_ltmle_glmnet(d_wide, varmethod = "tmle", resdf=res, Qint=FALSE, N_time = 2, label="4- ")
res <- run_ltmle_glmnet(d_wide, varmethod = "ic", resdf=res, Qint=T, elastic.net=T, N_time = 2, label="5- ")
res <- run_ltmle_glmnet(d_wide, varmethod = "tmle", resdf=res, Qint=T, elastic.net=T, N_time = 2, label="6- ")
res <- run_ltmle_glmnet(d_wide, varmethod = "ic", resdf=res, Qint=FALSE, elastic.net=T, N_time = 2, label="7- ")
res <- run_ltmle_glmnet(d_wide, varmethod = "tmle", resdf=res, Qint=FALSE, elastic.net=T, N_time = 2, label="8- ")
res <- run_ltmle_glmnet(d_wide, varmethod = "ic", resdf=res, Qint=T, gcomp=T, N_time = 2, label="9- gcomp")
res <- run_ltmle_glmnet(d_wide, varmethod = "tmle", resdf=res, Qint=T, gcomp=T, N_time = 2, label="10- ")
res <- run_ltmle_glmnet(d_wide, varmethod = "ic", resdf=res, Qint=FALSE, gcomp=T, N_time = 2, label="11- ")
res <- run_ltmle_glmnet(d_wide, varmethod = "tmle", resdf=res, Qint=FALSE, gcomp=T, N_time = 2, label="12- ")
res <- run_ltmle_glmnet(d_wide, varmethod = "ic", resdf=res, Qint=T, gcomp=T, elastic.net=T, N_time = 2, label="13- ")
res <- run_ltmle_glmnet(d_wide, varmethod = "tmle", resdf=res, Qint=T, gcomp=T, elastic.net=T, N_time = 2, label="14- ")
res <- run_ltmle_glmnet(d_wide, varmethod = "ic", resdf=res, Qint=FALSE, gcomp=T, elastic.net=T, N_time = 2, label="16- ")
res <- run_ltmle_glmnet(d_wide, varmethod = "tmle", resdf=res, Qint=FALSE,  N_time = 2, label="17- ")
res <- run_ltmle_glmnet(d_wide, SL.library="glm", varmethod = "ic", resdf=res, Qint=FALSE,  N_time = 2, label="18-glm ")
res <- run_ltmle_glmnet(d_wide, SL.library="glm", varmethod = "ic", resdf=res, Qint=FALSE,  N_time = 2, label="19-glm ")
res <- run_ltmle(d_wide, SL.library="SL.mean", varmethod = "ic", resdf=res, Qint=FALSE,  N_time = 2, label="20-SL mean ")

res <- run_ltmle(d_wide, SL.library="SL.glm", varmethod = "ic", resdf=res, Qint=FALSE,  N_time = 2, label="21-SL glm ")
res <- run_ltmle(d_wide, SL.library="SL.glmnet", varmethod = "ic", resdf=res, Qint=FALSE,  N_time = 2, label="22-SL glmnet ")

SL.lib <- c("SL.mean", "SL.glm", "SL.EN.robust", "SL.glmnet.robust")
res <- run_ltmle(d_wide, varmethod = "ic", resdf=res, SL.library = SL.lib, Qint=FALSE)

res







d=d_wide
                             N_time = 2
                             SL.library = c("SL.glmnet")
                             resdf=NULL
                             Qint=F
                             gcomp=F
                             elastic.net=F
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


    qform = c(
      insulin_0="Q.kplus1 ~ 1",
      insulin_1="Q.kplus1 ~ 1",
      event_dementia_1="Q.kplus1 ~ 1")

    SuperLearner_override2 <- function(Y, X, newX = NULL, family = gaussian(), SL.library,
             method = "method.NNLS", id = NULL, verbose = FALSE, control = list(),
             cvControl = list(), obsWeights = NULL, env = parent.frame()) {
      stopifnot(identical(SL.library, "SL.glm"))

      res <- NULL
      try(res <- SL.glm(Y, X, newX, family, obsWeights, id))
      #if(is.null(res)){res <- SL.mean(Y, X, newX, family, obsWeights, id)}

      list(model=res$fit, SL.predict = res$pred)
    }


    res3<-NULL
      package_stub("SuperLearner", "SuperLearner", SuperLearner_override2, {
        #testthatsomemore::package_stub("ltmle", "Estimate", Estimate_override, {
          try(res3 <- ltmle(data=spec_ltmle$data,
                           Anodes = spec_ltmle$Anodes,
                           Cnodes = spec_ltmle$Cnodes,
                           Lnodes = spec_ltmle$Lnodes,
                           Ynodes = spec_ltmle$Ynodes,
                           survivalOutcome = T,
                           abar = c(1,1),
                           gcomp=gcomp,
                           Qform = NULL,
                           estimate.time=T,
                           deterministic.Q.function = det.Q.function,
                           SL.library = "SL.glm",
                           variance.method = varmethod #use tmle variance option for accuracy with positivity violations
          ))
        #})
      })
      summary(res3)


      res<-NULL
      package_stub("SuperLearner", "SuperLearner", SuperLearner_override, {
        #testthatsomemore::package_stub("ltmle", "Estimate", Estimate_override, {
        try(res <- ltmle(data=spec_ltmle$data,
                          Anodes = spec_ltmle$Anodes,
                          Cnodes = spec_ltmle$Cnodes,
                          Lnodes = spec_ltmle$Lnodes,
                          Ynodes = spec_ltmle$Ynodes,
                          survivalOutcome = T,
                          abar = c(1,1),
                          gcomp=gcomp,
                          Qform = NULL,
                          estimate.time=T,
                          deterministic.Q.function = det.Q.function,
                          SL.library = "SL.glmnet",
                          variance.method = varmethod #use tmle variance option for accuracy with positivity violations
        ))
        #})
      })

      summary(res)


    res2 <- ltmle(data=spec_ltmle$data,
                 Anodes = spec_ltmle$Anodes,
                 Cnodes = spec_ltmle$Cnodes,
                 Lnodes = spec_ltmle$Lnodes,
                 Ynodes = spec_ltmle$Ynodes,
                 survivalOutcome = T,
                 abar = c(1,1),
                 gcomp=gcomp,
                 Qform = NULL,
                 estimate.time=T,
                 deterministic.Q.function = det.Q.function,
                 SL.library = "glm",
                 variance.method = varmethod #use tmle variance option for accuracy with positivity violations
    )
    summary(res)
    summary(res2)
    summary(res3)




















    SuperLearner_override3 <- function(Y, X, newX = NULL, family = gaussian(), SL.library,
                                       method = "method.NNLS", id = NULL, verbose = FALSE, control = list(),
                                       cvControl = list(), obsWeights = NULL, env = parent.frame()) {
      stopifnot(identical(SL.library, "SL.glmnet"))

      res <- NULL
      #try(res <- SL.glm(Y, X, newX, family, obsWeights, id))
      if(is.null(res)){res <- SL.mean(Y, X, newX, family, obsWeights, id)}

      list(model=res$fit, SL.predict = res$pred)
    }




    res<-NULL
    package_stub("SuperLearner", "SuperLearner", SuperLearner_override3, {
      testthatsomemore::package_stub("ltmle", "Estimate", Estimate_override, {
      try(res <- ltmle(data=spec_ltmle$data,
                       Anodes = spec_ltmle$Anodes,
                       Cnodes = spec_ltmle$Cnodes,
                       Lnodes = spec_ltmle$Lnodes,
                       Ynodes = spec_ltmle$Ynodes,
                       survivalOutcome = T,
                       abar = abar_spec,
                       gcomp=gcomp,
                       Qform = NULL,
                       estimate.time=T,
                       deterministic.Q.function = det.Q.function,
                       SL.library = "SL.glmnet",
                       variance.method = varmethod #use tmle variance option for accuracy with positivity violations
      ))
      })
    })

    summary(res)


    res2 <- ltmle(data=spec_ltmle$data,
                  Anodes = spec_ltmle$Anodes,
                  Cnodes = spec_ltmle$Cnodes,
                  Lnodes = spec_ltmle$Lnodes,
                  Ynodes = spec_ltmle$Ynodes,
                  survivalOutcome = T,
                  abar = abar_spec,
                  gcomp=gcomp,
                  Qform = NULL,
                  estimate.time=T,
                  deterministic.Q.function = det.Q.function,
                  SL.library = "SL.mean",
                  variance.method = varmethod #use tmle variance option for accuracy with positivity violations
    )


    res3<-NULL
    package_stub("SuperLearner", "SuperLearner", SuperLearner_override, {
      testthatsomemore::package_stub("ltmle", "Estimate", Estimate_override, {
        try(res3 <- ltmle(data=spec_ltmle$data,
                         Anodes = spec_ltmle$Anodes,
                         Cnodes = spec_ltmle$Cnodes,
                         Lnodes = spec_ltmle$Lnodes,
                         Ynodes = spec_ltmle$Ynodes,
                         survivalOutcome = T,
                         abar = abar_spec,
                         gcomp=gcomp,
                         Qform = NULL,
                         estimate.time=T,
                         deterministic.Q.function = det.Q.function,
                         SL.library = "SL.glmnet",
                         variance.method = varmethod #use tmle variance option for accuracy with positivity violations
        ))
      })
    })


    summary(res)
    summary(res2)
    summary(res3)


    as.data.frame(summary(res)$effect.measures$RR)
    as.data.frame(summary(res2)$effect.measures$RR)
    as.data.frame(summary(res3)$effect.measures$RR)

