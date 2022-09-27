

# Created: 2021-10-12
# Author(s): 
# Output files: A longitudinal data.table with covariates and datasets 
#                prepared  for LTMLE and covariate file, all with full data (no missings)

#NOTE: currently doing something SUPER simple --this is just a placeholder
#for something fancier later on.

impute_longitudinal_missingness <- function(time_dem=time_dem){
  

  #save non-imputed dementia and death for tabulation
  time_dem$first_dementia <- time_dem$event_dementia
  time_dem$first_death <- time_dem$event_death

  # Fill event with 1 and censor with 1
  setkeyv(time_dem,c("pnr","splittime"))
  #table(time_dem$event_dementia,useNA="always")
  #table(time_dem$splittime, time_dem$event_dementia)
  time_dem[,event_dementia:=nafill(event_dementia,type="locf")]
  # table(time_dem$event_dementia)
  # table(time_dem$splittime, time_dem$event_dementia)
  time_dem[,MACE:=nafill(MACE,type="locf")]
  time_dem[,MACE2:=nafill(MACE2,type="locf")]

    time_dem[,event_death:=nafill(event_death,type="locf")]
    time_dem[,censor_dem:=nafill(censor_dem,type="locf")]
    time_dem[,censor_MACE:=nafill(censor_MACE,type="locf")]
    time_dem[,censor_MACE2:=nafill(censor_MACE2,type="locf")]
    
  

  
  #Create missingness indicator variables
  for(var in c("chronic.pulmonary.disease","heart.failure","hypertension","ischemic.heart.disease","myocardial.infarction","renal.disease","any.malignancy",
               "metformin","sglt2_inhib","glp1","dpp4_inhib","insulin","sulfonylurea","repaglinid","alfa_glusidase_inhib","thiazolodinidion"))
    time_dem[,paste0(var,"_miss")]<- ifelse(is.na(time_dem[[var]]),1,0)
  
  
  #Check assumptions and cleaning here!
  #Missing values should be from censoring
  setkeyv(time_dem,c("pnr","splittime"))
  
  for(var in c("chronic.pulmonary.disease","heart.failure","hypertension","ischemic.heart.disease","myocardial.infarction","renal.disease","any.malignancy")){
    time_dem[,(var):=nafill(get(var),type="locf"),by=pnr]
  }
 #NOTE: I removed these because we don't want to LVCF for treatment, need to check this later
  # c("metformin","sglt2_inhib","glp1","dpp4_inhib","insulin","sulfonylurea","repaglinid","alfa_glusidase_inhib","thiazolodinidion")
   for(var in c("metformin","sglt2_inhib","glp1","dpp4_inhib","insulin","sulfonylurea","repaglinid","alfa_glusidase_inhib","thiazolodinidion")){
   #  #time_dem[,(var):=nafill(get(var),type="locf"),by=pnr]
     time_dem[,(var):=as.numeric(get(var))]
   #   time_dem[,(var):=nafill(get(var),type="locf"),by=pnr]
   #   
   }
  
  return(time_dem=time_dem)
  
}



