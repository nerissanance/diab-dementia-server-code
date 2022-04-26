
rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))




gc()
d_wide_list <- readRDS(file=here("data/simulated_data_list.RDS"))
d_wide_list <- d_wide_list[1:50]
gc()



# resdf_Qint_1se <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
#   res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, override_function=SuperLearner_override_1se))
  return(res)
#}
saveRDS(resdf_Qint_1se, paste0(here::here(),"/data/sim_res_Qint_1se.RDS"))



spec_analysis
function(data, long_covariates, baseline_vars, N_time, Avars=c("glp1_","sglt2_inhib_"), Yvars=c("event_dementia_")){

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
