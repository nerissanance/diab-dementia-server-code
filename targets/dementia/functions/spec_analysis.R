if(FALSE){
  data=tar_read(wide_df)
  Avars=c("glp1_")
  Yvars=c("MACE_")
  Cvars="censor_dem_"
  baseline_vars=c("age_base","sex", "code5txt", "quartile_income","metformin_dur", "year_2nd_line_start","code5txt_miss","quartile_income_miss" )
  long_covariates= c("insulin_", "chronic.pulmonary.disease_","hypertension_",
                                "ischemic.heart.disease_","heart.failure_", "renal.disease_")
  
}


spec_analysis <- function(data, long_covariates, baseline_vars, N_time, Avars, Yvars, Cvars){
  
  node_names <- spec_nodes(baseline_vars=(baseline_vars),
                           longitudinal_vars=c(Avars, Cvars,Yvars,long_covariates),
                           num_time=0:(N_time-1))
  node_names <- node_names[!(node_names %in% c(paste0(Yvars,0),paste0(Cvars,0),paste0(Avars,0),"event_death_0"))]
  #Drop final timepoint
  for(i in long_covariates){
    node_names <- node_names[!(grepl(paste0(i,(N_time-1)),node_names))]
  }
  
  Lnode_names <- c(baseline_vars, expand.grid(long_covariates,0:(N_time-1)) %>% apply(1, function(row) paste0(row, collapse = "")))
  Lnode_names <- gsub(" ","", Lnode_names)
  #Drop final timepoint
  Lnode_names <- Lnode_names[!(grepl(paste0("_",(N_time-1)),Lnode_names))]
  Lnode_names <- Lnode_names[!(Lnode_names %in% c("event_death_0"))]
  
  
  #subset to analysis columns and arrange
  d_ltmle <- data %>% dplyr::select(!!(node_names))
  #colnames(d_ltmle)
  
  #clean censoring nodes to right format
  Cnode_names = node_names[grep(paste0("^",Cvars), node_names)]
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

