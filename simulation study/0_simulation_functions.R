
library(lava)
library(data.table)
library(tidyverse)

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

calc_sim_truth <- function(coefficient_tab){
  cc <- coefficient_tab

  nsamp<-1000000
  seed <- 3457347

  set.seed(seed)
  u.always <- synthesizeDD.always(cc)
  d.always <- sim(u.always, nsamp)

  set.seed(seed)
  u.never <- synthesizeDD.never(cc)
  d.never <- sim(u.never, nsamp)


  (prop.always3 <-
      1*(d.always$event_dementia_1 +
           d.always$event_dementia_2 +
           d.always$event_dementia_3 >0)  %>% table %>% prop.table)


  (prop.never3 <-
      1*(d.never$event_dementia_1 +
           d.never$event_dementia_2 +
           d.never$event_dementia_3  >0) %>% table %>% prop.table)


  (cRD3 <-  prop.always3[2] - prop.never3[2])
  (cRR3 <- (prop.always3[2])/prop.never3[2])

  (prop.always <-
      1*(d.always$event_dementia_1 +
           d.always$event_dementia_2 +
           d.always$event_dementia_3 +
           d.always$event_dementia_4 +
           d.always$event_dementia_5 +
           d.always$event_dementia_6 +
           d.always$event_dementia_7 +
           d.always$event_dementia_8 +
           d.always$event_dementia_9 +
           d.always$event_dementia_10 >0)  %>% table %>% prop.table)
  (prop.always10 <- d.always$event_dementia_10 %>% table %>% prop.table)


  (prop.never <-
      1*(d.never$event_dementia_1 +
           d.never$event_dementia_2 +
           d.never$event_dementia_3 +
           d.never$event_dementia_4 +
           d.never$event_dementia_5 +
           d.never$event_dementia_6 +
           d.never$event_dementia_7 +
           d.never$event_dementia_8 +
           d.never$event_dementia_9 +
           d.never$event_dementia_10 >0) %>% table %>% prop.table)
  (prop.never10 <- d.never$event_dementia_10 %>% table %>% prop.table)


  (cRD <-  prop.always[2] - prop.never[2])
  (cRR <- (prop.always[2])/prop.never[2])

  cat("Risk ratio T3: ",cRR3,"\n")
  cat("Risk difference T3: ",cRD3,"\n")

  cat("Risk ratio T10: ",cRR,"\n")
  cat("Risk difference T10: ",cRD,"\n")

  return(list(cRR3=cRR3, cRD3=cRD3, cRR=cRR, cRD=cRD))
}


#Calculate truth
synthesizeDD <- function(coefficients){
  requireNamespace("lava")
  coefficients <- data.table(coefficients)
  XNAMES <- names(coefficients)[-(1:3)]
  BETA <- coefficients[,-(1:3),with=0L]
  INTERCEPT <- coefficients[["(Intercept)"]]
  # empty lava model for simulation
  m <- lvm()
  distribution(m,"age_base") <- normal.lvm(mean=70,sd=10)
  distribution(m,"sex") <- binomial.lvm(p=0.4)
  m <- addvar(m,"ie_type")
  m <- addvar(m,"code5txt")
  m <- addvar(m,"quartile_income")
  # loop across time and variables
  for (j in 1:NROW(coefficients)){
    V <- coefficients$var[j]
    beta <- unlist(BETA[j,])
    X <- XNAMES[!is.na(beta)]
    beta <- beta[!is.na(beta)]
    # add V ~ Intercept + beta X
    distribution(m,V) <- binomial.lvm()
    intercept(m,V) <- INTERCEPT[j]
    regression(m,from=X,to=V) <- beta
  }
  class(m) <- c("synthesizeDD",class(m))
  m
}
sim.synthesizeDD <- function(x,...){
  class(x) <- class(x)[-1]
  d <- data.table(sim(x,...))
  v <- names(d)
  ttt <- sapply(strsplit(grep("death",v,value=TRUE),"event_death_"),"[",2)
  ## HERE
  ## for (t in ttt) set(x=d,j=paste0("fup_",t),value=d[[paste("event_death_",t)]]+d[[paste("event_dementia_",t)]]+d[[paste("censor_",t)]])
  #d <- apply(d,1,function(i){    })
  #fup <- data.table()
  #d[,fup:={browser();apply(.SD,1,function(x){browser()})},.SDcols=grep("event|censor|death",names(d))]
  d
}


synthesizeDD.always <- function(coefficients, A_name = "glp1"){
  requireNamespace("lava")
  coefficients <- data.table(coefficients)
  XNAMES <- names(coefficients)[-(1:3)]
  BETA <- coefficients[,-(1:3),with=0L]
  # collect At and Ct nodes; intervene At=1 and Ct=0 later
  loc_A <- grep(paste0("^", A_name, "_"), XNAMES)
  beta_A <- BETA[, loc_A, with = F]
  loc_C <- grep("^censor_", XNAMES)
  beta_C <- BETA[, loc_C, with = F]


  INTERCEPT <- coefficients[["(Intercept)"]]
  # empty lava model for simulation
  m <- lvm()
  distribution(m,"age_base") <- normal.lvm(mean=70,sd=10)
  distribution(m,"sex") <- binomial.lvm(p=0.4)
  m <- addvar(m,"ie_type")
  m <- addvar(m,"code5txt")
  m <- addvar(m,"quartile_income")
  # loop across time and variables
  for (j in 1:NROW(coefficients)){
    # At constant 1 -> intercept becomes intercept + At coefficient
    # also remove At from fitted betas
    # Ct constant 0 -> intercept no change, remove Ct from fitted betas
    temp_intercept <- INTERCEPT
    temp_sum_A_coef <- rowSums(beta_A, na.rm = T)  # intercept + At coefficients
    temp_intercept <- temp_intercept + temp_sum_A_coef

    V <- coefficients$var[j]
    beta <- unlist(BETA[j,])
    beta[loc_A] <- NA  # absorb A coefficient into intercept for always-on group; not depending on observed A values any more
    beta[loc_C] <- NA  # C is also intervened so will be ignored in conditional logistic models

    X <- XNAMES[!is.na(beta)]
    beta <- beta[!is.na(beta)]
    # add V ~ Intercept + beta X
    distribution(m,V) <- binomial.lvm()
    # intercept(m,V) <- INTERCEPT[j]
    intercept(m,V) <- temp_intercept[j]
    regression(m,from=X,to=V) <- beta
  }
  class(m) <- c("synthesizeDD",class(m))
  m
}



synthesizeDD.never <- function(coefficients, A_name = "glp1"){
  requireNamespace("lava")
  coefficients <- data.table(coefficients)
  XNAMES <- names(coefficients)[-(1:3)]
  BETA <- coefficients[,-(1:3),with=0L]
  # collect At and Ct nodes; intervene At=1 and Ct=0 later
  loc_A <- grep(paste0("^", A_name, "_"), XNAMES)
  beta_A <- BETA[, loc_A, with = F]
  loc_C <- grep("^censor_", XNAMES)
  beta_C <- BETA[, loc_C, with = F]


  INTERCEPT <- coefficients[["(Intercept)"]]
  # empty lava model for simulation
  m <- lvm()
  distribution(m,"age_base") <- normal.lvm(mean=70,sd=10)
  distribution(m,"sex") <- binomial.lvm(p=0.4)
  m <- addvar(m,"ie_type")
  m <- addvar(m,"code5txt")
  m <- addvar(m,"quartile_income")
  # loop across time and variables
  for (j in 1:NROW(coefficients)){
    # At constant 1 -> intercept becomes intercept + At coefficient
    # also remove At from fitted betas
    # Ct constant 0 -> intercept no change, remove Ct from fitted betas
    temp_intercept <- INTERCEPT
    temp_sum_A_coef <- rowSums(beta_A, na.rm = T)  # intercept + At coefficients
    temp_intercept <- temp_intercept + temp_sum_A_coef

    V <- coefficients$var[j]
    beta <- unlist(BETA[j,])
    beta[loc_A] <- NA  # absorb A coefficient into intercept for always-on group; not depending on observed A values any more
    beta[loc_C] <- NA  # C is also intervened so will be ignored in conditional logistic models

    X <- XNAMES[!is.na(beta)]
    beta <- beta[!is.na(beta)]
    # add V ~ Intercept + beta X
    distribution(m,V) <- binomial.lvm()
    intercept(m,V) <- INTERCEPT[j]#keep only intercept for "never on"
    # intercept(m,V) <- temp_intercept[j]
    regression(m,from=X,to=V) <- beta
  }
  class(m) <- c("synthesizeDD",class(m))
  m
}



clean_sim_data <- function(d, N_time){

  #d <- as.data.frame(sapply(d, as.numeric))
  #d[is.na(d)] <- 0 #Missingness due to censoring should be coded 0 as long as censoring variable is equal to 1.
  d<- data.table(d)

  for(i in 1:(N_time+1)){
    j=i+1
    d[get(paste0("event_dementia_",i))==1, (paste0("event_dementia_",j)):=1]
    d[get(paste0("event_death_",i))==1, (paste0("event_death_",j)):=1]
  }

  dementia.nodes<- grep("event_dementia_",names(d))
  death.nodes<- grep("event_death_",names(d))
  d[, sum_death :=rowSums(.SD,na.rm=T), .SDcols = death.nodes]
  d[, sum_dementia :=rowSums(.SD,na.rm=T), .SDcols = dementia.nodes]
  d[sum_death > sum_dementia, (dementia.nodes) := replace(.SD, .SD == 1, 0), .SDcols = dementia.nodes]
  d[sum_death < sum_dementia, (death.nodes) := replace(.SD, .SD == 1, 0), .SDcols = death.nodes]
  d[sum_death== sum_dementia, (death.nodes) := replace(.SD, .SD == 1, 0), .SDcols = death.nodes]
 return(d)
}



run_ltmle_glmnet <- function(d,
                             N_time = 11, #number of time points you want to look at
                             SL.library = c("SL.glmnet"),
                             resdf=NULL,
                             Qint=F,
                             gcomp=F,
                             det.Q=T,
                             gbound = c(0.01, 1),
                             override_function=SuperLearner_override,
                             varmethod = "tmle", #variance method
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


  spec_ltmle <- spec_analysis(data=d, c(long_covariates,"event_death_"),
                              baseline_vars, N_time,
                              Avars=c("glp1_"),
                              Yvars=c("event_dementia_"),
                              Cvars=c("censor_"))
  abar_spec = list(rep(1,N_time-1),rep(0,N_time-1))

  # #Drop the baseline events

  set.seed(12345)
  fit = NULL


  if(Qint){

    if(N_time==11){
      # qform = c(
      #   insulin_0="Q.kplus1 ~ 1",
      #   insulin_1="Q.kplus1 ~ 1",
      #   event_dementia_1="Q.kplus1 ~ 1",
      #   insulin_2="Q.kplus1 ~ 1",
      #   event_dementia_2="Q.kplus1 ~ 1",
      #   insulin_3="Q.kplus1 ~ 1",
      #   event_dementia_3="Q.kplus1 ~ 1",
      #   insulin_4="Q.kplus1 ~ 1",
      #   event_dementia_4="Q.kplus1 ~ 1",
      #   insulin_5="Q.kplus1 ~ 1",
      #   event_dementia_5="Q.kplus1 ~ 1",
      #   insulin_6="Q.kplus1 ~ 1",
      #   event_dementia_6="Q.kplus1 ~ 1",
      #   insulin_7="Q.kplus1 ~ 1",
      #   event_dementia_7="Q.kplus1 ~ 1",
      #   insulin_8="Q.kplus1 ~ 1",
      #   event_dementia_8="Q.kplus1 ~ 1",
      #   insulin_9="Q.kplus1 ~ 1",
      #   event_dementia_9="Q.kplus1 ~ 1",
      #   event_dementia_10="Q.kplus1 ~ 1"
      # )
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

if(glm){

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
                       SL.library = "glm",
                       variance.method = varmethod,
                       id=id
      ))

  }else{
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

}


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





run_ltmle_glmnet_no_cens <- function(d,
                             N_time = 11, #number of time points you want to look at
                             SL.library = c("SL.glmnet"),
                             resdf=NULL,
                             Qint=F,
                             gcomp=F,
                             det.Q=T,
                             gbound = c(0.01, 1),
                             override_function=SuperLearner_override,
                             varmethod = "tmle", #variance method
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


  spec_ltmle <- spec_analysis_no_cens(data=d, c(long_covariates),
                              baseline_vars, N_time,
                              Avars=c("glp1_"),
                              Yvars=c("event_dementia_"))
  abar_spec = list(rep(1,N_time-1),rep(0,N_time-1))

  # #Drop the baseline events
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
    det.q.fun = function(data, current.node, nodes, called.from.estimate.g){
      death.index <- grep("death_",names(data))
      if(length(death.index)==0)stop("no node found")
      hist.death.index <- death.index[death.index < current.node]
      if(length(hist.death.index)==0)return(NULL)
      if(length(hist.death.index)==1){
        is.deterministic <- data[,hist.death.index]==1
      } else {
        is.deterministic <- apply(data[,hist.death.index]==1,1,any)
      }#if death before, remove from fitting
      is.deterministic[is.na(is.deterministic)] <- F
      return(list(is.deterministic=is.deterministic, Q.value=0))
    }
  }else{
    det.q.fun = NULL
  }

  if(!is.null(id)){
    id <- spec_ltmle$data[["id"]]
  }

  if(glm){
    try(fit <- ltmle(data=spec_ltmle$data,
                     Anodes = spec_ltmle$Anodes,
                     Cnodes = NULL,
                     Lnodes = spec_ltmle$Lnodes,
                     Ynodes = spec_ltmle$Ynodes,
                     gbound=gbound,
                     survivalOutcome = T,
                     abar = abar_spec,
                     gcomp=gcomp,
                     Qform = qform,
                     estimate.time=F,
                     deterministic.Q.function = det.q.fun,
                     SL.library = "glm",
                     variance.method = varmethod,
                     id=id
    ))
  }else{
    package_stub("SuperLearner", "SuperLearner", override_function, {
      testthatsomemore::package_stub("ltmle", "Estimate", Estimate_override, {
        try(fit <- ltmle(data=spec_ltmle$data,
                         Anodes = spec_ltmle$Anodes,
                         Cnodes = NULL,
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

  }



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

spec_analysis_no_cens <- function(data, long_covariates, baseline_vars, N_time, Avars, Yvars, glm=F){

  node_names <- spec_nodes(baseline_vars=(baseline_vars),
                           longitudinal_vars=c(Avars,Yvars,long_covariates),
                           num_time=0:(N_time-1))
  node_names <- node_names[!(node_names %in% c(paste0(Yvars,0),paste0(Avars,0)))]
  #Drop final timepoint
  for(i in long_covariates){
    node_names <- node_names[!(grepl(paste0(i,(N_time-1)),node_names))]
  }

  Lnode_names <- c(baseline_vars, expand.grid(long_covariates,0:(N_time-1)) %>% apply(1, function(row) paste0(row, collapse = "")))
  Lnode_names <- gsub(" ","", Lnode_names)
  #Drop final timepoint
  Lnode_names <- Lnode_names[!(grepl(paste0("_",(N_time-1)),Lnode_names))]


  #subset to analysis columns and arrange
  d_ltmle <- data %>% dplyr::select(!!(node_names))
  #colnames(d_ltmle)


  return(list(
    data=d_ltmle,
    node_names=node_names,
    Anodes = node_names[sort(grep(paste("^",Avars, collapse="|", sep=""), node_names))],
    Cnodes = NULL,
    Lnodes = Lnode_names,
    Ynodes = node_names[sort(grep(paste("^",Yvars, collapse="|", sep=""), node_names))]
  ))
}


run_ltmle_glmnet_unadj <- function(d,
                             N_time = 11, #number of time points you want to look at
                             SL.library = c("SL.glmnet"),
                             resdf=NULL,
                             Qint=F,
                             gcomp=F,
                             det.Q=T,
                             override_function=SuperLearner_override,
                             varmethod = "tmle", #variance method
                             label=""){

  warn = getOption("warn")
  options(warn=-1)

  #clean competing events
  d <-clean_sim_data(d, N_time=N_time)

  #Use only first N time points
  d <- d %>%
    dplyr::select(!!(baseline_vars),matches(paste0("_(",paste0(0:(N_time-1),collapse="|"),")$")))


  spec_ltmle <- spec_analysis(data=d, c("event_death_"),
                              baseline_vars=NULL, N_time,
                              Avars=c("glp1_"),
                              Yvars=c("event_dementia_"))
  abar_spec = list(rep(1,N_time),rep(0,N_time))

  #Drop the baseline events

  spec_ltmle$Cnodes = spec_ltmle$Cnodes[spec_ltmle$Cnodes!="censor_0"]
  spec_ltmle$Lnodes = spec_ltmle$Lnodes[spec_ltmle$Lnodes!="event_death_0"]
  spec_ltmle$Ynodes = spec_ltmle$Ynodes[spec_ltmle$Ynodes!="event_dementia_0"]

  set.seed(12345)
  fit = NULL


           if(Qint){

             if(N_time==11){
               qform = c(
                 event_death_1="Q.kplus1 ~ 1",
                 event_dementia_1="Q.kplus1 ~ 1",
                 event_death_2="Q.kplus1 ~ 1",
                 event_dementia_2="Q.kplus1 ~ 1",
                 event_death_3="Q.kplus1 ~ 1",
                 event_dementia_3="Q.kplus1 ~ 1",
                 event_death_4="Q.kplus1 ~ 1",
                 event_dementia_4="Q.kplus1 ~ 1",
                 event_death_5="Q.kplus1 ~ 1",
                 event_dementia_5="Q.kplus1 ~ 1",
                 event_death_6="Q.kplus1 ~ 1",
                 event_dementia_6="Q.kplus1 ~ 1",
                 event_death_7="Q.kplus1 ~ 1",
                 event_dementia_7="Q.kplus1 ~ 1",
                 event_death_8="Q.kplus1 ~ 1",
                 event_dementia_8="Q.kplus1 ~ 1",
                 event_death_9="Q.kplus1 ~ 1",
                 event_dementia_9="Q.kplus1 ~ 1",
                 event_death_10="Q.kplus1 ~ 1",
                 event_dementia_10="Q.kplus1 ~ 1"
               )
             }

             if(N_time==2){
               qform = c(
                 event_death_1="Q.kplus1 ~ 1",
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
      try(fit <- ltmle(data=spec_ltmle$data,
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



  if(!is.null(fit)){
    res <- summary(fit)
    res.RR <- as.data.frame(res$effect.measures$RR)
    res.ate <- as.data.frame(res$effect.measures$ATE) %>% rename(ate.long.name=long.name,ate=estimate, ate.sd=std.dev , ate.pval=pvalue, ate.ci.lb=CI.2.5., ate.ci.ub=  CI.97.5., ate.log.std.err=log.std.err)
    res <- cbind(res.RR, res.ate)
    res$label <- label
  }
  if(!is.null(resdf)){
    res <- bind_rows(resdf, res)
  }

  options(warn=warn)
  return(res)
}



run_ltmle_glmnet_interaction <- function(d,
                             N_time = 11, #number of time points you want to look at
                             SL.library = c("SL.glmnet"),
                             resdf=NULL,
                             Qint=F,
                             gcomp=F,
                             det.Q=F,
                             override_function=SuperLearner_override,
                             varmethod = "tmle", #variance method
                             label=""){

  warn = getOption("warn")
  options(warn=-1)

  #clean competing events
  d <-clean_sim_data(d, N_time=N_time)

  #make interactions between A and L
  d <- data.frame(d)

  int_vars <- NULL
  for(i in 0:(N_time-1)){
    for(j in long_covariates){
      int_varname<-paste0("glp1X",j)
      new_varname<-paste0("glp1X",j, i)
      var1 <- paste0("glp1_", i)
      var2 <- paste0(j, i)

      newvar <- d[[var1]] * d[[var2]]
      d[[new_varname]] <- newvar
      int_vars = c(int_vars, int_varname)
    }
  }
  int_vars = unique(int_vars)


  #Use only first N time points
  #Use only first N time points
  d <- d %>%
    dplyr::select(!!(baseline_vars),matches(paste0("_(",paste0(0:(N_time-1),collapse="|"),")$")))


  spec_ltmle <- spec_analysis(data=d, c(long_covariates, int_vars, "event_death_"),
                              baseline_vars, N_time,
                              Avars=c("glp1_"),
                              Yvars=c("event_dementia_"))
  abar_spec = list(rep(1,N_time),rep(0,N_time))


  #Drop the baseline events

  spec_ltmle$Cnodes = spec_ltmle$Cnodes[spec_ltmle$Cnodes!="censor_0"]
  spec_ltmle$Lnodes = spec_ltmle$Lnodes[spec_ltmle$Lnodes!="event_death_0"]
  spec_ltmle$Ynodes = spec_ltmle$Ynodes[spec_ltmle$Ynodes!="event_dementia_0"]





  set.seed(12345)
  res = NULL


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
}




run_ltmle <- function(d,
                      N_time = 11, #number of time points you want to look at
                      SL.library = c("SL.glmnet"),
                      resdf=NULL,
                      Qint=F,
                      det.Q=T,
                      varmethod = "tmle", #variance method
                      label=""
){

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
        event_dementia_1="Q.kplus1 ~ 1"
      )
    }


    if(det.Q){
      det.q.fun = det.Q.function
    }else{
      det.q.fun = NULL
    }


        try(res <- ltmle(data=spec_ltmle$data,
                         Anodes = spec_ltmle$Anodes,
                         Cnodes = spec_ltmle$Cnodes,
                         Lnodes = spec_ltmle$Lnodes,
                         Ynodes = spec_ltmle$Ynodes,
                         survivalOutcome = T,
                         abar = abar_spec,
                         Qform = qform,
                         deterministic.Q.function = det.q.fun,
                         SL.library = SL.library,
                         variance.method = varmethod #use tmle variance option for accuracy with positivity violations
        ))

  }else{
        try(res <- ltmle(data=spec_ltmle$data,
                         Anodes = spec_ltmle$Anodes,
                         Cnodes = spec_ltmle$Cnodes,
                         Lnodes = spec_ltmle$Lnodes,
                         Ynodes = spec_ltmle$Ynodes,
                         survivalOutcome = T,
                         abar = abar_spec,
                         deterministic.Q.function = det.Q.function,
                         SL.library = SL.library,
                         variance.method = varmethod #use tmle variance option for accuracy with positivity violations
        ))
  }


  if(!is.null(res)){
    res <- summary(res)
    res <- as.data.frame(res$effect.measures$RR)
    res$label <- label
  }
  if(!is.null(resdf)){
    res <- bind_rows(resdf, res)
  }

  options(warn=warn)
  return(res)
}



SL.glmnet.robust <- function (Y, X, newX, family, obsWeights, id, alpha = 1, nfolds = 10,
                              nlambda = 100, useMin = TRUE, loss = "deviance", ...){
  #.SL.require("glmnet")
  if (!is.matrix(X)) {
    X <- model.matrix(~-1 + ., X)
    newX <- model.matrix(~-1 + ., newX)
  }
  fitCV <- NULL
  try(fitCV <- glmnet::cv.glmnet(x = X, y = Y, weights = obsWeights,
                                 lambda = NULL, type.measure = loss, nfolds = nfolds,
                                 family = family$family, alpha = alpha, nlambda = nlambda,
                                 ...))
  if(is.null(fitCV)){
    meanY <- weighted.mean(Y, w = obsWeights)
    pred <- rep.int(meanY, times = nrow(newX))
    fit <- list(object = meanY)
    out <- list(pred = pred, fit = fit)
    class(out$fit) <- c("SL.mean")
    return(out)
  }else{
    pred <- predict(fitCV, newx = newX, type = "response", s = ifelse(useMin, "lambda.min", "lambda.1se"))
    fit <- list(object = fitCV, useMin = useMin)
    class(fit) <- "SL.glmnet"
    out <- list(pred = pred, fit = fit)
    return(out)
  }

}


SL.EN.robust <- function(..., alpha = 0.5){
  SL.glmnet.robust(...,alpha = 0.5)
}







