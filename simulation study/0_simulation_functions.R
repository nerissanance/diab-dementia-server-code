
#Parallelize:
parallel::detectCores()
n.cores <- parallel::detectCores() -4

#create the cluster
my.cluster <- parallel::makeCluster(
  n.cores,
  type = "FORK"
)

#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)

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
                      elastic.net=F,
                      varmethod = "tmle" #variance method
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


  set.seed(12345)
  res = NULL


  if(Qint){

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


    if(elastic.net){
      package_stub("SuperLearner", "SuperLearner", SuperLearner_override_EN, {
        testthatsomemore::package_stub("ltmle", "Estimate", Estimate_override, {
          try(res <- ltmle(data=spec_ltmle$data,
                           Anodes = spec_ltmle$Anodes,
                           Cnodes = spec_ltmle$Cnodes[spec_ltmle$Cnodes!="censor_0"],
                           Lnodes = spec_ltmle$Lnodes[spec_ltmle$Lnodes!="event_death_0"],
                           Ynodes = spec_ltmle$Ynodes[spec_ltmle$Ynodes!="event_dementia_0"],
                           survivalOutcome = T,
                           abar = abar_spec,
                           gcomp=gcomp,
                           Qform = qform,
                           estimate.time=T,
                           deterministic.Q.function = det.Q.function,
                           SL.library = SL.library,
                           variance.method = varmethod #use tmle variance option for accuracy with positivity violations
          ))
        })})
    }else{
      package_stub("SuperLearner", "SuperLearner", SuperLearner_override, {
        testthatsomemore::package_stub("ltmle", "Estimate", Estimate_override, {
          try(res <- ltmle(data=spec_ltmle$data,
                           Anodes = spec_ltmle$Anodes,
                           Cnodes = spec_ltmle$Cnodes[spec_ltmle$Cnodes!="censor_0"],
                           Lnodes = spec_ltmle$Lnodes[spec_ltmle$Lnodes!="event_death_0"],
                           Ynodes = spec_ltmle$Ynodes[spec_ltmle$Ynodes!="event_dementia_0"],
                           survivalOutcome = T,
                           abar = abar_spec,
                           gcomp=gcomp,
                           Qform = qform,
                           estimate.time=T,
                           deterministic.Q.function = det.Q.function,
                           SL.library = SL.library,
                           variance.method = varmethod #use tmle variance option for accuracy with positivity violations
          ))
        })})
    }




  }else{



    if(elastic.net){
      SuperLearner_override_EN
        package_stub("SuperLearner", "SuperLearner", SuperLearner_override, {
          testthatsomemore::package_stub("ltmle", "Estimate", Estimate_override, {
            try(res <- ltmle(data=spec_ltmle$data,
                             Anodes = spec_ltmle$Anodes,
                             Cnodes = spec_ltmle$Cnodes[spec_ltmle$Cnodes!="censor_0"],
                             Lnodes = spec_ltmle$Lnodes[spec_ltmle$Lnodes!="event_death_0"],
                             Ynodes = spec_ltmle$Ynodes[spec_ltmle$Ynodes!="event_dementia_0"],
                             survivalOutcome = T,
                             abar = abar_spec,
                             deterministic.Q.function = det.Q.function,
                             SL.library = SL.library,
                             variance.method = varmethod #use tmle variance option for accuracy with positivity violations
            ))
          })})
    }else{

    package_stub("SuperLearner", "SuperLearner", SuperLearner_override, {
      testthatsomemore::package_stub("ltmle", "Estimate", Estimate_override, {
        try(res <- ltmle(data=spec_ltmle$data,
                         Anodes = spec_ltmle$Anodes,
                         Cnodes = spec_ltmle$Cnodes[spec_ltmle$Cnodes!="censor_0"],
                         Lnodes = spec_ltmle$Lnodes[spec_ltmle$Lnodes!="event_death_0"],
                         Ynodes = spec_ltmle$Ynodes[spec_ltmle$Ynodes!="event_dementia_0"],
                         survivalOutcome = T,
                         abar = abar_spec,
                         deterministic.Q.function = det.Q.function,
                         SL.library = SL.library,
                         variance.method = varmethod #use tmle variance option for accuracy with positivity violations
        ))
      })})
    }
  }


  if(!is.null(res)){
    res <- summary(res)
    res <- as.data.frame(res$effect.measures$RR)
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
                      varmethod = "tmle" #variance method
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


  set.seed(12345)
  res = NULL


  if(Qint){

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


        try(res <- ltmle(data=spec_ltmle$data,
                         Anodes = spec_ltmle$Anodes,
                         Cnodes = spec_ltmle$Cnodes[spec_ltmle$Cnodes!="censor_0"],
                         Lnodes = spec_ltmle$Lnodes[spec_ltmle$Lnodes!="event_death_0"],
                         Ynodes = spec_ltmle$Ynodes[spec_ltmle$Ynodes!="event_dementia_0"],
                         survivalOutcome = T,
                         abar = abar_spec,
                         Qform = qform,
                         deterministic.Q.function = det.Q.function,
                         SL.library = SL.library,
                         variance.method = varmethod #use tmle variance option for accuracy with positivity violations
        ))

  }else{
        try(res <- ltmle(data=spec_ltmle$data,
                         Anodes = spec_ltmle$Anodes,
                         Cnodes = spec_ltmle$Cnodes[spec_ltmle$Cnodes!="censor_0"],
                         Lnodes = spec_ltmle$Lnodes[spec_ltmle$Lnodes!="event_death_0"],
                         Ynodes = spec_ltmle$Ynodes[spec_ltmle$Ynodes!="event_dementia_0"],
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
  }
  if(!is.null(resdf)){
    res <- bind_rows(resdf, res)
  }

  options(warn=warn)
  return(res)
}
