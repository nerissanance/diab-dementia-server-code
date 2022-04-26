







#------------------------------------------
# glmnet-only function for LTMLE that avoids
# SuperLearner cross-validation
#------------------------------------------



# SuperLearner_override <- function(Y, X, newX = NULL, family = gaussian(), SL.library,
#                                   method = "method.NNLS", id = NULL, verbose = FALSE, control = list(),
#                                   cvControl = list(), obsWeights = NULL, env = parent.frame()) {
#   stopifnot(identical(SL.library, "SL.glmnet"))
#
#   res <- SL.glmnet(Y, X, newX, family, obsWeights, id)
#   list(model=res, SL.predict = res$pred)
# }

SuperLearner_override <- function(Y, X, newX = NULL, family = gaussian(), SL.library="SL.glmnet",
                                      method = "method.NNLS", id = NULL, verbose = FALSE, control = list(),
                                      cvControl = list(), obsWeights = NULL, env = parent.frame(), alpha=1, loss  = "auc") {
  stopifnot(identical(SL.library, "SL.glmnet"))

  res <- NULL
  try(res <- SL.glmnet(Y, X, newX, family, obsWeights, id, alpha=alpha, useMin =TRUE,  nfolds = 5))
  if(is.null(res)){res <- SL.mean(Y, X, newX, family, obsWeights, id)}

  list(model=res$fit, SL.predict = res$pred)
}

SuperLearner_override_RF <- function(Y, X, newX = NULL, family = gaussian(), SL.library="SL.glmnet",
                                  method = "method.NNLS", id = NULL, verbose = FALSE, control = list(),
                                  cvControl = list(), obsWeights = NULL, env = parent.frame(), alpha=1, loss  = "auc") {
  stopifnot(identical(SL.library, "SL.randomForest"))

  res <- NULL
  try(res <- SL.randomForest(Y, X, newX, family, obsWeights, id, mtry = ifelse(family=="gaussian", max(floor(ncol(X)/3), 1), floor(sqrt(ncol(X)))),
                             ntree = 100, nodesize = ifelse(family == "gaussian", 5, 1), maxnodes = NULL, importance = FALSE))
  if(is.null(res)){res <- SL.mean(Y, X, newX, family, obsWeights, id)}

  list(model=res$fit, SL.predict = res$pred)
}

SuperLearner_override_1se <- function(Y, X, newX = NULL, family = gaussian(), SL.library,
                                  method = "method.NNLS", id = NULL, verbose = FALSE, control = list(),
                                  cvControl = list(), obsWeights = NULL, env = parent.frame()) {
  stopifnot(identical(SL.library, "SL.glmnet"))

  res <- NULL
  try(res <- SL.glmnet(Y, X, newX, family, obsWeights, id, nfolds = 5, useMin = FALSE))
  if(is.null(res)){res <- SL.mean(Y, X, newX, family, obsWeights, id)}

  list(model=res$fit, SL.predict = res$pred)
}

SuperLearner_override_EN <- function(Y, X, newX = NULL, family = gaussian(), SL.library,
                                         method = "method.NNLS", id = NULL, verbose = FALSE, control = list(),
                                         cvControl = list(), obsWeights = NULL, env = parent.frame()) {
  stopifnot(identical(SL.library, "SL.glmnet"))

  res <- NULL
  try(res <- SL.glmnet(Y, X, newX, family, obsWeights, id, alpha = 0.5, nfolds = 5))
  if(is.null(res)){res <- SL.mean(Y, X, newX, family, obsWeights, id)}

  list(model=res$fit, SL.predict = res$pred)
}

SuperLearner_override_EN_1se <- function(Y, X, newX = NULL, family = gaussian(), SL.library,
                                     method = "method.NNLS", id = NULL, verbose = FALSE, control = list(),
                                     cvControl = list(), obsWeights = NULL, env = parent.frame()) {
  stopifnot(identical(SL.library, "SL.glmnet"))

  res <- NULL
  try(res <- SL.glmnet(Y, X, newX, family, obsWeights, id, alpha = 0.5, nfolds = 5, useMin = FALSE))
  if(is.null(res)){res <- SL.mean(Y, X, newX, family, obsWeights, id)}

  list(model=res$fit, SL.predict = res$pred)
}


SuperLearner_override_AUC <- function(Y, X, newX = NULL, family = gaussian(), SL.library,
                                  method = "method.NNLS", id = NULL, verbose = FALSE, control = list(),
                                  cvControl = list(), obsWeights = NULL, env = parent.frame()) {
  stopifnot(identical(SL.library, "SL.glmnet"))

  res <- NULL
  try(res <- SL.glmnet(Y, X, newX, family, obsWeights, id, loss  = "auc", nfolds = 5))
  if(is.null(res)){res <- SL.mean(Y, X, newX, family, obsWeights, id)}

  list(model=res$fit, SL.predict = res$pred)
}

SuperLearner_override_EN_AUC <- function(Y, X, newX = NULL, family = gaussian(), SL.library,
                                  method = "method.NNLS", id = NULL, verbose = FALSE, control = list(),
                                  cvControl = list(), obsWeights = NULL, env = parent.frame()) {
  stopifnot(identical(SL.library, "SL.glmnet"))

  res <- NULL
  try(res <- SL.glmnet(Y, X, newX, family, obsWeights, id, alpha = 0.5, loss  = "auc", nfolds = 5))
  if(is.null(res)){res <- SL.mean(Y, X, newX, family, obsWeights, id)}

  list(model=res$fit, SL.predict = res$pred)
}


SuperLearner_override_AUC_1se <- function(Y, X, newX = NULL, family = gaussian(), SL.library,
                                         method = "method.NNLS", id = NULL, verbose = FALSE, control = list(),
                                         cvControl = list(), obsWeights = NULL, env = parent.frame()) {
  stopifnot(identical(SL.library, "SL.glmnet"))

  res <- NULL
  try(res <- SL.glmnet(Y, X, newX, family, obsWeights, id, alpha = 1, loss  = "auc", useMin = FALSE, nfolds = 5))
  if(is.null(res)){res <- SL.mean(Y, X, newX, family, obsWeights, id)}

  list(model=res$fit, SL.predict = res$pred)
}




package_stub<-function (package_name, function_name, stubbed_value, expr){
  if (!is.element(package_name, utils::installed.packages()[,
                                                            1]) && !is.element(package_name, loadedNamespaces())) {
    stop(gettextf("Could not find package %s for stubbing %s",
                  sQuote(package_name), dQuote(function_name)))
  }
  stopifnot(is.character(function_name))
  if (!is.function(stubbed_value))
    warning(gettextf("Stubbing %s::%s with a %s instead of a function",
                     package_name, function_name, sQuote(class(stubbed_value)[1])))
  namespaces <- list(as.environment(paste0("package:",
                                           package_name)), getNamespace(package_name))
  if (!exists(function_name, envir = namespaces[[1]], inherits = FALSE))
    namespaces <- namespaces[-1]
  if (!exists(function_name, envir = utils::tail(namespaces,
                                                 1)[[1]], inherits = FALSE))
    stop(gettextf("Cannot stub %s::%s because it must exist in the package",
                  package_name, function_name))
  lapply(namespaces, unlockBinding, sym = function_name)
  previous_object <- get(function_name, envir = utils::tail(namespaces,
                                                            1)[[1]])
  on.exit({
    lapply(namespaces, function(ns) {
      tryCatch(error = function(.) NULL, assign(function_name,
                                                previous_object, envir = ns))
      lockBinding(function_name, ns)
    })
  })
  lapply(namespaces, function(ns) assign(function_name, stubbed_value,
                                         envir = ns))
  eval.parent(substitute(expr))
}









spec_nodes <- function(baseline_vars, longitudinal_vars, num_time){
  node_names <- c(baseline_vars, expand.grid(longitudinal_vars, num_time) %>%
                    apply(1, function(row) paste0(row, collapse = "")))
  node_names <- gsub(" ","",node_names)
  return(node_names)
}



spec_analysis <- function(data, long_covariates, baseline_vars, N_time, Avars=c("glp1_","sglt2_inhib_"), Yvars=c("event_dementia_")){

  node_names <- spec_nodes(baseline_vars=baseline_vars,
                           longitudinal_vars=c(Avars,long_covariates, "censor_",Yvars),
                           num_time=0:(N_time-1))
  # node_names <- spec_nodes(baseline_vars=baseline_vars,
  #                          longitudinal_vars=c(long_covariates, "censor_",Avars,Yvars),
  #                          num_time=0:(N_time-1))

  Lnode_names <- c(baseline_vars, expand.grid(long_covariates,0:(N_time-1)) %>% apply(1, function(row) paste0(row, collapse = "")))
  Lnode_names <- gsub(" ","", Lnode_names)


  #subset to analysis columns and arrange
  d_ltmle <- data %>% dplyr::select(!!(node_names))
  colnames(d_ltmle)

  #clean censoring nodes to right format
  Cnode_names = node_names[grep("^censor", node_names)]
  for(i in Cnode_names){
    d_ltmle[[i]] <- BinaryToCensoring(is.censored=d_ltmle[[i]])
  }



  return(list(
    data=d_ltmle,
    node_names=node_names,
    Anodes = node_names[sort(grep(paste("^",Avars, collapse="|", sep=""), node_names))],
    Cnodes = Cnode_names,
    Lnodes = Lnode_names,
    Ynodes = node_names[sort(grep(paste("^",Yvars, collapse="|", sep=""), node_names))]
  ))
}

SL.hal9001.flexible <- function(Y,
                                X,
                                newX = NULL,
                                family=stats::binomial(),
                                obsWeights = rep(1, length(Y)),
                                id = NULL,
                                max_degree = ifelse(ncol(X) >= 20, 2, 3),
                                smoothness_orders = 1,
                                num_knots = if(max_degree==2) c(50,25) else c(50, 25, 15),
                                reduce_basis = 1 / sqrt(length(Y)),
                                lambda = NULL,
                                ...){

  require("hal")

  if(!is.matrix(X)) {X <- as.matrix(X)}
  if(!is.null(newX) & !is.matrix(newX)) {newX <- as.matrix(newX)}


    hal_fit <- hal::fit_hal(Y = Y, X = X, max_degree = max_degree,  id=id,
                            family="binomial",
                            fit_control = list(weights=obsWeights),
                            smoothness_orders = smoothness_orders,
                            num_knots = num_knots,
                            reduce_basis =reduce_basis,
                            lambda = lambda)


  if(!is.null(newX)){
    pred <- stats::predict(hal_fit, new_data = newX)
  }else{
    pred <- stats::predict(hal_fit, new_data = X)
  }
  fit <- list(object = hal_fit)
  class(fit) <- "SL.hal9001"
  out <- list(pred = pred, fit = fit)
  return(out)
}


# stochastic intervention function for dual treatment nodes
SI_function <- function(data, current.node, nodes){
  Anodes <- nodes$A
  if(!(current.node %in% Anodes)) return(NULL)
  if(sum(Anodes < current.node) < 2) return(NULL)

  prev.a.node <- max(Anodes[Anodes < current.node])
  prev.prev.a.node <- max(Anodes[Anodes < prev.a.node])
  is.deterministic <- ifelse(!is.na(data[,current.node]), data[,prev.prev.a.node]==1,F)
  prob1 <- data[, current.node][is.deterministic]

  return(list(is.deterministic=is.deterministic, prob1=prob1))
}




det.Q.function <- function(data, current.node, nodes, called.from.estimate.g){
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







#------------------------------------------
# Tabulate regime sparsity
#------------------------------------------
tab_regimes <- function(d, N_time){

  res_list <- NULL
  res <- data.frame(id=d$pnr, time=-1, censored=0, event=0, intensification=0, static_glp1=1, static_sglt2=1, stochastic_glp1=1, stochastic_sglt2=1)
  res_tab <- data.frame(time=0:N_time, N_uncensored=NA, N_censored=NA, intensification=0,
                        N_continuous_glp1=NA, N_stochastic_glp1=NA, N_continuous_sglt2=NA, N_stochastic_sglt2=NA,
                        N_events_continuous_glp1=NA, N_events_stochastic_glp1=NA, N_events_continuous_sglt2=NA, N_events_stochastic_sglt2=NA)

  for(i in 0:N_time){
    censored <- ifelse(d[[paste0("censor_",i)]]==0,F,T)
    res$censored <- 1*censored
    res$time <- res$time + 1*!censored
    new_event <- d[[paste0("event_dementia_",i)]]
    if(i!=0){
      new_event <- ifelse(d[[paste0("event_dementia_",i-1)]]==1, 0, new_event)
    }
    res$event <- new_event

    intensification <- d[[paste0("insulin_",i)]]
    if(i!=0){
      intensification <- ifelse(d[[paste0("insulin_",i-1)]]==1, 0, intensification)
    }
    res$intensification <- intensification

    on_glp1 <- d[[paste0("glp1_",i)]]
    on_sglt2 <- d[[paste0("sglt2_inhib_",i)]]
    if(i>0){
      prev_glp1 <- d[[paste0("glp1_",i-1)]]
      prev_sglt2 <- d[[paste0("sglt2_inhib_",i-1)]]
    }else{
      prev_glp1 <- prev_sglt2 <-  rep(0, nrow(d))
    }
    if(i<N_time){
      next_glp1 <- d[[paste0("glp1_",i+1)]]
      next_sglt2 <- d[[paste0("sglt2_inhib_",i+1)]]
    }else{
      next_glp1 <- next_sglt2 <-  rep(1, nrow(d))
    }

    res$static_glp1[!censored] <- ifelse(on_glp1[!censored]==0,0,res$static_glp1[!censored])
    res$static_sglt2[!censored] <- ifelse(on_sglt2[!censored]==0,0,res$static_sglt2[!censored])
    res$stochastic_glp1[!censored] <- ifelse(on_glp1[!censored]==0 & prev_sglt2[!censored]==0,0,res$stochastic_glp1[!censored])
    res$stochastic_sglt2[!censored] <- ifelse(on_sglt2[!censored]==0 & prev_sglt2[!censored]==0,0,res$stochastic_sglt2[!censored])
    res_list[[as.name(i)]] <- res

    j<-i+1
    res_tab$N_uncensored[j] <- sum(res$censored==0)
    res_tab$N_censored[j] <- sum(res$censored)
    res_tab$N_continuous_glp1[j] <- sum(res$static_glp1[!censored])
    res_tab$N_stochastic_glp1[j] <- sum(res$stochastic_glp1[!censored])
    res_tab$N_continuous_sglt2[j] <- sum(res$static_sglt2[!censored])
    res_tab$N_stochastic_sglt2[j] <- sum(res$stochastic_sglt2[!censored])
    res_tab$N_events_continuous_glp1[j] <- sum(res$static_glp1[res$event==1 & !censored])
    res_tab$N_events_stochastic_glp1[j] <- sum(res$stochastic_glp1[res$event==1 & !censored])
    res_tab$N_events_continuous_sglt2[j] <- sum(res$static_sglt2[res$event==1 & !censored])
    res_tab$N_events_stochastic_sglt2[j] <- sum(res$stochastic_sglt2[res$event==1 & !censored])
    res_tab$N_intensification_and_stay_glp1[j] <- sum(res$static_glp1[res$intensification==1 & on_glp1==1 & next_glp1==1 ])
    res_tab$N_intensification_and_drop_glp1[j] <- sum(res$static_glp1[res$intensification==1 & on_glp1==1 & next_glp1==0 ])
    res_tab$N_intensification_and_stay_sglt2[j] <- sum(res$static_sglt2[res$intensification==1 & on_sglt2==1 & next_sglt2==1 ])
    res_tab$N_intensification_and_drop_sglt2[j] <- sum(res$static_sglt2[res$intensification==1 & on_sglt2==1 & next_sglt2==0 ])
  }

  #replace all 1's and 2's with 3's
  res_tab[res_tab==1] <- 3
  res_tab[res_tab==2] <- 3

  res_perc <- res_tab
  for(i in 1:12){
    res_perc[i+4] <- round(res_perc[i+4]/res_perc$N_uncensored * 100, 2)
  }
  res_perc <- res_perc[,-c(1:4)]
  colnames(res_perc) <- gsub("N_","perc_",colnames(res_perc))

  #combine datasets
  res_df <- cbind(res_tab, res_perc)
  return(res_df)
}
