
#Parallelize:
try(parallel::detectCores())
try(n.cores <- parallel::detectCores() -4)

#create the cluster
try(my.cluster <- parallel::makeCluster( n.cores,type = "FORK"))

#register it to be used by %dopar%
try(doParallel::registerDoParallel(cl = my.cluster))

#registerDoSNOW(my.cluster)
# pb <- txtProgressBar(style = 3)
# progress <- function(n) setTxtProgressBar(pb, n)
# opts <- list(progress = progress)


clean_sim_data <- function(d, N_time){

  d <- as.data.frame(sapply(d, as.numeric))
  d[is.na(d)] <- 0 #Missingness due to censoring should be coded 0 as long as censoring variable is equal to 1.
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
                             override_function=SuperLearner_override,
                             varmethod = "tmle", #variance method
                             alt=FALSE,
                             label=""){

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

  #Drop the baseline events
  spec_ltmle$data <- spec_ltmle$data %>% subset(., select = -c(event_death_0, censor_0, event_dementia_0))
  spec_ltmle$Cnodes = spec_ltmle$Cnodes[spec_ltmle$Cnodes!="censor_0"]
  spec_ltmle$Lnodes = spec_ltmle$Lnodes[spec_ltmle$Lnodes!="event_death_0"]
  spec_ltmle$Ynodes = spec_ltmle$Ynodes[spec_ltmle$Ynodes!="event_dementia_0"]

  set.seed(12345)
  fit = NULL


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



run_ltmle_glmnet_unadj <- function(d,
                             N_time = 11, #number of time points you want to look at
                             SL.library = c("SL.glmnet"),
                             resdf=NULL,
                             Qint=F,
                             gcomp=F,
                             det.Q=T,
                             override_function=SuperLearner_override,
                             varmethod = "tmle", #variance method
                             alt=FALSE,
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
                              Yvars=c("event_dementia_"),
                              alt=alt)
  abar_spec = list(rep(1,N_time),rep(0,N_time))

  #Drop the baseline events
  spec_ltmle$data <- spec_ltmle$data %>% subset(., select = -c(event_death_0, censor_0, event_dementia_0))
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
                             det.Q=T,
                             override_function=SuperLearner_override,
                             varmethod = "tmle", #variance method
                             alt=FALSE,
                             label=""){

  warn = getOption("warn")
  options(warn=-1)

  #clean competing events
  d <-clean_sim_data(d, N_time=N_time)

  #make interactions between A and L
  long_covariates

  i=1
  j=long_covariates[1]

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

  # Yvars=c("event_dementia_")
  #
  # long_covariates = c(long_covariates, int_vars, "event_death_")
  # node_names <- spec_nodes(baseline_vars=baseline_vars,
  #                          longitudinal_vars=c(Avars,"censor_",long_covariates, Yvars),
  #                          num_time=0:(N_time-1))

  #Use only first N time points
  d <- d %>%
    dplyr::select(!!(baseline_vars),matches(paste0("_(",paste0(0:(N_time-1),collapse="|"),")$")))

  colnames(d)

  spec_ltmle <- spec_analysis(data=d, c(long_covariates, int_vars, "event_death_"),
                              baseline_vars, N_time,
                              Avars=c("glp1_"),
                              Yvars=c("event_dementia_"),
                              alt=alt)
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







