
rm(list=ls())
sink(file="./1_data_cleaning/3_transform_long_to_wide.Rout",append=F)
Sys.Date()
source(here::here("0_config.R"))

base_dem <- readRDS(file=here::here("data/base_dem_imputed.rds"))
time_dem <- readRDS(file=here::here("data/time_dem_long.rds"))


time_dem_wide <- time_dem[,.SD[1],by=c("pnr","splittime"),.SDcols=c("inn","age","calender","metformin",
                                                                    "sglt2_inhib", "glp1","sulfonylurea","insulin","dpp4_inhib","repaglinid","thiazolodinidion",
                                                                    "alfa_glusidase_inhib","chronic.pulmonary.disease","heart.failure","hypertension","ischemic.heart.disease",
                                                                    "myocardial.infarction","renal.disease","any.malignancy",
                                                                    "event_dementia","event_death","censor")]


#Code any 2nd-line treatment usage
time_dem_wide <- data.table(sapply(time_dem_wide, as.numeric))
time_dem_wide$any_second_line_no_sglt2 <- 1*(rowSums(time_dem_wide[,c("sulfonylurea","dpp4_inhib","repaglinid","thiazolodinidion","alfa_glusidase_inhib")], na.rm=T) >0)
time_dem_wide$any_second_line <- 1*(rowSums(time_dem_wide[,c("sglt2_inhib","sulfonylurea","dpp4_inhib","repaglinid","thiazolodinidion","alfa_glusidase_inhib")], na.rm=T) >0)

#Save long-format dataset for tabulation
time_dem_full_long <- merge(time_dem_wide,
                        base_dem[,.(pnr,first_other,secdate,fdato,first_ind,last_ind,first_ud,      
                                    last_ud,ie_type,opr_land,sex,CREA_base,HBA1C_base,HDL_chol_base,LDL_chol_base,T_chol_base,
                                    age_base,egfr_base, quartile_income, code5txt)],
                        by="pnr",all.x=TRUE)
saveRDS(time_dem_full_long, file=here::here("data/full_data_long.rds"))

#cast to wide format
time_dem_wide2 <- dcast(time_dem_wide,pnr~splittime,value.var=
                          c("pnr","calender","metformin",
                            "sglt2_inhib", "glp1","sulfonylurea","insulin","dpp4_inhib","repaglinid","thiazolodinidion",
                            "alfa_glusidase_inhib","any_second_line","any_second_line_no_sglt2","chronic.pulmonary.disease","heart.failure","hypertension","ischemic.heart.disease",
                            "myocardial.infarction","renal.disease","any.malignancy",
                            "event_dementia","event_death","censor"))

time_dem_wide2 <- merge(time_dem_wide2,
                        base_dem[,.(pnr,first_other,secdate,fdato,first_ind,last_ind,first_ud,      
                                    last_ud,ie_type,opr_land,sex,CREA_base,HBA1C_base,HDL_chol_base,LDL_chol_base,T_chol_base,
                                    age_base,egfr_base, quartile_income, code5txt)],
                        by="pnr",all.x=TRUE)


saveRDS(time_dem_wide2,file=here::here("data/time_dem_wide.rds"))  
table(is.na(time_dem_wide2$quartile_income))#double checking imputation
table(is.na(time_dem_wide2$opr_land))#double checking imputation
table((time_dem_wide2$opr_land))#double checking imputation
table(is.na(time_dem_wide2$code5txt))#double checking imputation
table((time_dem_wide2$ie_type))#double checking imputation





