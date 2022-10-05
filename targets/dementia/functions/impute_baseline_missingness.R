

# Created: 2021-10-12
# Author(s): 
# Input files: long data from file 2a and base_dem.rds from file 1
# Output files: A longitudinal data.table with covariates and datasets 
#                prepared  for LTMLE and covariate file, all with full data (no missings)

#NOTE: currently doing something SUPER simple --this is just a placeholder
#for something fancier later on.

impute_baseline_missingness <- function(base_dem=base_dem){
  

  #drop unneeded variables
  base_dem <- base_dem %>% subset(., select = -c(last_ind, first_ud, last_ud))

  
  #look at percent missing
  #apply(base_dem,2,function(x) prop.table(table(is.na(x))))
  #about 40% missing in the biomarkers, but we are not surprised by that


  #impute the rest of them that have missingnesss to the median/mode
  
  # *  opr_land                 : Country of origin
  # * quartile_income 

  
  get_mode <- function(x){
    uniqv <- unique(na.omit(x))
    uniqv[which.max(tabulate(match(x,uniqv)))]
    
  }

  base_dem$quartile_income_miss <- ifelse(is.na(base_dem$quartile_income), 1, 0)
  base_dem$quartile_income<- as.character(base_dem$quartile_income)
  base_dem$quartile_income[is.na(base_dem$quartile_income)] <- get_mode(base_dem$quartile_income)
  base_dem$quartile_income<- factor(base_dem$quartile_income)
  #table(is.na(base_dem$quartile_income))
  
  
  base_dem$code5txt_miss <- ifelse(is.na(base_dem$code5txt), 1, 0)
  base_dem$code5txt<- as.character(base_dem$code5txt)
  base_dem$code5txt[is.na(base_dem$code5txt)] <- get_mode(base_dem$code5txt)
  base_dem$code5txt<- factor(base_dem$code5txt)
  #same with 4 cat
  base_dem$edu_4cat[is.na(base_dem$edu_4cat)] <- get_mode(base_dem$edu_4cat)
  
  #table(base_dem$code5txt)
  #table(is.na(base_dem$code5txt))
  base_dem$immigrant <- ifelse(base_dem$origin%in%c("Immigrant"),1,
                               ifelse(base_dem$origin%in%c("Dane","Descendent"),0,NA))
  base_dem$origin_miss <- ifelse(is.na(base_dem$origin), 1, 0)
  base_dem$origin[base_dem$origin=="."|is.na(base_dem$origin)] <- get_mode(base_dem$origin)
  base_dem$immigrant[is.na(base_dem$immigrant)] <- get_mode(base_dem$immigrant)
  
  #table(is.na(base_dem$origin))
  
  #Median impute continuous
  
  
  # Variables not imputed:
  #first_ind, last_ind, first_ud, last_ud, dementia, year_2nd_line_start, doddato, dod_ami, dod_stroke, dod_cv
  #table(base_dem$year_2nd_line_start)
  #table(is.na(base_dem$year_2nd_line_start))
  
  #ADD MISSINGNESS INDICATORS TO ALL
  
  base_dem$CREA_base_miss <- ifelse(is.na(base_dem$CREA_base), 1, 0)
  base_dem$CREA_base[is.na(base_dem$CREA_base)] <- median(base_dem$CREA_base, na.rm=T)
  
  base_dem$HBA1C_base_miss <- ifelse(is.na(base_dem$HBA1C_base), 1, 0)
  base_dem$HBA1C_base[is.na(base_dem$HBA1C_base)] <- median(base_dem$HBA1C_base, na.rm=T)
  
  base_dem$HDL_chol_base_miss <- ifelse(is.na(base_dem$HDL_chol_base), 1, 0)
  base_dem$HDL_chol_base[is.na(base_dem$HDL_chol_base)] <- median(base_dem$HDL_chol_base, na.rm=T)
  
  base_dem$LDL_chol_base_miss <- ifelse(is.na(base_dem$LDL_chol_base), 1, 0)
  base_dem$LDL_chol_base[is.na(base_dem$LDL_chol_base)] <- median(base_dem$LDL_chol_base, na.rm=T)
  
  base_dem$T_chol_base_miss <- ifelse(is.na(base_dem$T_chol_base), 1, 0)
  base_dem$T_chol_base[is.na(base_dem$T_chol_base)] <- median(base_dem$T_chol_base, na.rm=T)
  
  base_dem$egfr_base_miss <- ifelse(is.na(base_dem$egfr_base), 1, 0)
  base_dem$egfr_base[is.na(base_dem$egfr_base)] <- median(base_dem$egfr_base, na.rm=T)
  
  #cat(apply(base_dem,2,function(x) prop.table(table(is.na(x)))))
  
  
  return(base_dem)
  
}



