if(FALSE){
  
  data=tar_read(wide_df)
  Avars="glp1_"
  Yvars="MACE_"
  Cvars="censor_new_"
  yr=2011
  baseline_vars=tar_read(baseline_vars)
  long_covariates=tar_read(long_covariates)
  N_time=5
  cvd_subset="hba1c365_0"
  
}
subset_and_specify_analysis <- function(data, yr, 
                                    baseline_vars,
                                    long_covariates,
                                    Avars,
                                    Yvars,
                                    Cvars,
                                     N_time ,cvd_subset=NULL){

 
  #for dementia we want: 
  #  - no dementia at baseline
  if (Yvars=="event_dementia_") {
  
    #drop those with prior dementia
    data <- data[prior_dementia==0,]
      
    
  ## FOR RUNNING MANUAL COMPETING RISK FIX
  ## edit--when one occurs first, set other to zero so there's no competing event:
  dementia.nodes<- grep("event_dementia_",names(data))
  death.nodes<- grep("event_death_",names(data))
  data[, sum_death :=rowSums(.SD,na.rm=T), .SDcols = death.nodes]
  data[, sum_dementia :=rowSums(.SD,na.rm=T), .SDcols = dementia.nodes]
  
  data[sum_death > sum_dementia, (dementia.nodes) := replace(.SD, .SD == 1, 0), .SDcols = dementia.nodes]
  data[sum_death < sum_dementia, (death.nodes) := replace(.SD, .SD == 1, 0), .SDcols = death.nodes]
  # NOTE: decided to prioritize dementia in the event that both death and dementia occur in the same time bin
  data[sum_death== sum_dementia, (death.nodes) := replace(.SD, .SD == 1, 0), .SDcols = death.nodes]
  } else if(Yvars%in%c("MACE_","MACE2_")) {
    
    #for MACE we want people who had at least one baseline HBA1C value:
    #"cvd_subset" string is for the baseline value we want to subset on for A1C measurements 
    data <- data[!is.na(get(cvd_subset)),]
    #we also want to create a new censoring node which incorporates 
    #hba1c missingness, as per our emails and updates in the analytic plan
    #to do this, we take the max of our censoring node censor_MACE_
    #and our new indicator for censoring by hba1c, censor_hba1c_
    #(because of the wide format, this is clunky)
    for(i in 0:(N_time-1)){
      data[,paste0("censor_new_",i):=pmax(get(paste0("censor_MACE_",i)),get(paste0("censor_hba1c_",i)),na.rm=T)]
    }
    #uncomment for some tables that show this is working:
    # table(data$censor_MACE2_3,data$censor_hba1c_3,useNA="always")
    # #people move from NA and 0 categories to 1, as expected
    # table(data$censor_MACE2_3,data$censor_new_3,useNA="always")
    # table(data$censor_hba1c_3,data$censor_new_3,useNA="always")
    # table(data$censor_new_3,useNA="always")
    
  } else {stop("Outcome not accounted for in subsetting, revise subset_ file")}
  
  

     # #subset to after yr
  d_wide <- data[year(secdate)>=yr,]
  
  
  d <- d_wide %>%
    dplyr::select(!!(baseline_vars), matches(paste0(baseline_vars,"_miss")),
                  matches(paste0("_(",paste0(0:(N_time-1),collapse="|"),")$")),
                  matches(paste0("_(",paste0(0:(N_time-1), "_miss",collapse="|"),")$")))
  ##colnames(d)
  
  #specify LTMLE analysis
  spec_ltmle <- spec_analysis(data=d, long_covariates,
                              baseline_vars,
                              N_time=N_time,
                              Avars=Avars,
                              Yvars=Yvars,
                              Cvars=Cvars)
  
  
  return(spec_ltmle)
  
}