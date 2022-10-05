
#Make a dataset with longform time-dependent variables but with all baseline variables merged in for easier tabulation
save_full_longform_data <- function(time_dem, base_dem){
  
  base_dem$pnr <- as.numeric(base_dem$pnr)
  time_dem$pnr <- as.numeric(time_dem$pnr)
  
  vars =c("inn","age","calender","visitfreq","metformin",
          "sglt2_inhib", "glp1","sulfonylurea","insulin","dpp4_inhib","repaglinid","thiazolodinidion",
          "alfa_glusidase_inhib","chronic.pulmonary.disease","heart.failure","hypertension","ischemic.heart.disease",
          "myocardial.infarction","renal.disease","any.malignancy",
          "event_dementia","event_mi","event_death","censor","censor_mi") 
  all_vars = colnames(time_dem)[colnames(time_dem) %in% c(vars,paste0(vars,"_miss"))]
  
  
  time_dem_wide <- time_dem[,.SD[1],by=c("pnr","splittime"),.SDcols=all_vars]
  
  
  #Code any 2nd-line treatment usage
  time_dem_wide <- data.table(sapply(time_dem_wide, as.numeric))
  time_dem_wide$any_second_line_no_sglt2_ <- 1*(rowSums(time_dem_wide[,c("sulfonylurea","dpp4_inhib","repaglinid","thiazolodinidion","alfa_glusidase_inhib")], na.rm=T) >0)
  time_dem_wide$any_second_line <- 1*(rowSums(time_dem_wide[,c("sglt2_inhib","sulfonylurea","dpp4_inhib","repaglinid","thiazolodinidion","alfa_glusidase_inhib")], na.rm=T) >0)
  

  
  #Save long-format dataset for tabulation
  time_dem_full_long <- merge(time_dem_wide,
                              baseline<- base_dem[,.(pnr,#first_other,
                                                     secdate,birth_date, year_2nd_line_start,
                                                     #first_ind,last_ind,first_ud,  last_ud,
                                                     metformin_dur,sex,CREA_base,HBA1C_base,HDL_chol_base,LDL_chol_base,T_chol_base,
                                                     age_base,egfr_base, quartile_income, code5txt,
                                                     # origin,  origin_miss,
                                                     quartile_income_miss,code5txt_miss,             CREA_base_miss,      HBA1C_base_miss,    
                                                     HDL_chol_base_miss,  LDL_chol_base_miss,  T_chol_base_miss,    egfr_base_miss )],
                              by="pnr",all.x=TRUE)
  return(time_dem_full_long) 
}
  