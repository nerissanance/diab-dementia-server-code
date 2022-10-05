if (FALSE){
  long_df=tar_read(longitudinal_imputed)
  base_dem=tar_read(baseline_imputed)
  labdata=tar_read(hba1c_wide)
  N_time=11
  long_covariates=tar_read(long_covariates)
}

transform_long_to_wide <- function(base_dem,
                                   long_df,
                                   N_time,
                                   long_covariates,
                                   labdata){

  
    long_df <- long_df[splittime<N_time,]

    #cast to wide format
    vars =c("metformin",
            "sglt2_inhib", "glp1","sulfonylurea","insulin","dpp4_inhib","repaglinid","thiazolodinidion",
            "alfa_glusidase_inhib",
            gsub("_","",long_covariates),
            "event_death","event_dementia","censor_dem","MACE","MACE2","censor_MACE","censor_MACE2") 
    all_vars = colnames(long_df)[colnames(long_df) %in% c(vars,paste0(vars,"_miss"))]
    
    
    #subset to single observation per timepoint here
    time_dem_wide <- long_df[,.SD[1],by=c("pnr","splittime"),.SDcols=c(all_vars)]
    
    
    #Code any 2nd-line treatment usage
    ## time_dem_wide <- data.table(sapply(time_dem_wide, as.numeric))

    time_dem_wide$any_second_line_no_sglt2 <- 1*(rowSums(time_dem_wide[,c("sulfonylurea","dpp4_inhib","repaglinid","thiazolodinidion","alfa_glusidase_inhib")], na.rm=T) >0)
    time_dem_wide$any_second_line_no_dpp4 <- 1*(rowSums(time_dem_wide[,c("sulfonylurea","sglt2_inhib","repaglinid","thiazolodinidion","alfa_glusidase_inhib")], na.rm=T) >0)
    time_dem_wide$any_second_line <- 1*(rowSums(time_dem_wide[,c("sglt2_inhib","sulfonylurea","dpp4_inhib","repaglinid","thiazolodinidion","alfa_glusidase_inhib")], na.rm=T) >0)
    time_dem_wide$glp1_or_sglt2 <- 1*(rowSums(time_dem_wide[,c("sglt2_inhib","glp1")], na.rm=T) >0)
    
    time_dem_wide2 <- data.table::dcast(time_dem_wide,pnr~splittime,value.var= c(all_vars,"any_second_line","any_second_line_no_sglt2","glp1_or_sglt2"))
    
    baseline<- base_dem[,.(pnr,#first_other,
                           secdate,birth_date, year_2nd_line_start,prior_dementia,
                           #first_ind,last_ind,first_ud,  last_ud,
                           metformin_dur,sex,CREA_base,HBA1C_base,HDL_chol_base,LDL_chol_base,T_chol_base,
                           age_base,egfr_base, quartile_income, code5txt,edu_4cat,
                          origin,  origin_miss, immigrant,
                           quartile_income_miss,code5txt_miss,             CREA_base_miss,      HBA1C_base_miss,    
                           HDL_chol_base_miss,  LDL_chol_base_miss,  T_chol_base_miss,    egfr_base_miss )]
    
    

    d <- merge(baseline,time_dem_wide2,by="pnr",all.x=FALSE)
    # d <- merge(baseline,time_dem_wide2,by="pnr",all.x=TRUE)
    
    #also merge in hba1c data
    setkey(labdata,pnr)
    setkey(d,pnr)
    d <- labdata[d]

    return(d)
}




