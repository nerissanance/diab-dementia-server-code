# 
# tabulate_data <- function(long_df, wide_df, baseline_vars, long_covariates){
# 
# #calculate follow up time
#   
#   
#   long_df <- long_df %>% filter(splittime < 11)
#   table(long_df$event_dementia)
#   prop.table(table(long_df$event_dementia)) * 100
# 
#   table(long_df$glp1)
#   max(long_df$secdate)
#   
#   #tabulate drug usage
#   df_drug_long <- long_df %>% filter(censor==0) %>% group_by(pnr) %>%
#     summarize(dementia = max(event_dementia, na.rm=T),
#               num_glp_use = sum(glp1, na.rm=T),
#               full_glp_use = 1*(sum(glp1, na.rm=T)==11))
#   table(df_drug_long$num_glp_use)
#   table(df_drug_long$full_glp_use, df_drug_long$dementia)
#   
#   #Check variable presence
#   baseline_vars = c("origin","age_base","sex", "code5txt", "quartile_income","metformin_dur", "year_2nd_line_start")
#   long_covariates = c("insulin_","any.malignancy_", "chronic.pulmonary.disease_","hypertension_",
#                       "myocardial.infarction_", "ischemic.heart.disease_","heart.failure_", "renal.disease_")
#   
# 
#   colnames(long_df)
#   baseline_vars[!(baseline_vars %in% colnames(long_df))]
#   long_covariates[!(gsub("_","",long_covariates) %in% colnames(long_df))]
#   
#   #temp <- wide_df %>% filter(glp1_0, glp1_1, glp1_2, glp1_3, glp1_4, glp1_5, glp1_6)
#   time_dem_wide2<-wide_df
#   table(1*(time_dem_wide2$glp1_0+time_dem_wide2$glp1_1+time_dem_wide2$glp1_2+time_dem_wide2$glp1_3+time_dem_wide2$glp1_4+time_dem_wide2$glp1_5+time_dem_wide2$glp1_6+time_dem_wide2$glp1_7+time_dem_wide2$glp1_8+time_dem_wide2$glp1_9+time_dem_wide2$glp1_10)>10,
#         (time_dem_wide2$event_dementia_10))
#   
#   
#   head(wide_df)
#   time_dem_wide2<-wide_df
#   # table(time_dem_wide$event_dementia)
#   # table(time_dem_wide$splittime, time_dem_wide$event_dementia)
#   # d3 <- time_dem_wide %>% filter(splittime==0)
#   # table(d3$glp1, (d3$event_dementia))
#   
#   table(1*(time_dem_wide2$glp1_0==1),
#         1*(time_dem_wide2$event_dementia_1+time_dem_wide2$event_dementia_2+time_dem_wide2$event_dementia_3+time_dem_wide2$event_dementia_4+time_dem_wide2$event_dementia_5+time_dem_wide2$event_dementia_6+time_dem_wide2$event_dementia_7+time_dem_wide2$event_dementia_8+time_dem_wide2$event_dementia_9+time_dem_wide2$event_dementia_10)>0)
#   table(1*(time_dem_wide2$glp1_0+time_dem_wide2$glp1_1+time_dem_wide2$glp1_2+time_dem_wide2$glp1_3+time_dem_wide2$glp1_4+time_dem_wide2$glp1_5+time_dem_wide2$glp1_6+time_dem_wide2$glp1_7+time_dem_wide2$glp1_8+time_dem_wide2$glp1_9+time_dem_wide2$glp1_10)>10,
#         1*(time_dem_wide2$event_dementia_1+time_dem_wide2$event_dementia_2+time_dem_wide2$event_dementia_3+time_dem_wide2$event_dementia_4+time_dem_wide2$event_dementia_5+time_dem_wide2$event_dementia_6+time_dem_wide2$event_dementia_7+time_dem_wide2$event_dementia_8+time_dem_wide2$event_dementia_9+time_dem_wide2$event_dementia_10)>0)
#   table(time_dem_wide$event_dementia[time_dem_wide$splittime==0])
#   table(is.na(time_dem_wide$event_dementia[time_dem_wide$splittime==0]))
#   
# #count visit frequency within each splittime
# head(time_dem)
# 
# time_dem <- time_dem %>% group_by(pnr, splittime) %>% mutate(visitfreq=n())
# time_dem <- data.table(time_dem)
# }
# 
# 
# tar_load(wide_df)
# names(wide_df)
# 
# library(Publish)
# #lazyFactorCoding(wide_df)
# wide_df[,quartile_income_miss:=factor(quartile_income_miss,levels=c("0","1"),labels=c("No","Yes"))]
# wide_df[,code5txt_miss:=factor(code5txt_miss,levels=c("0","1"),labels=c("No","Yes"))]
# wide_df[,CREA_base_miss:=factor(CREA_base_miss,levels=c("0","1"),labels=c("No","Yes"))]
# wide_df[,HBA1C_base_miss:=factor(HBA1C_base_miss,levels=c("0","1"),labels=c("No","Yes"))]
# wide_df[,HDL_chol_base_miss:=factor(HDL_chol_base_miss,levels=c("0","1"),labels=c("No","Yes"))]
# wide_df[,LDL_chol_base_miss:=factor(LDL_chol_base_miss,levels=c("0","1"),labels=c("No","Yes"))]
# wide_df[,T_chol_base_miss:=factor(T_chol_base_miss,levels=c("0","1"),labels=c("No","Yes"))]
# wide_df[,egfr_base_miss:=factor(egfr_base_miss,levels=c("0","1"),labels=c("No","Yes"))]
# wide_df[,sum_dementia:=factor(sum_dementia,levels=c("0","4","7","9","10","11"),labels=c("0","4","7","9","10","11"))]
# test <- summary(utable(sex~age_base+Q(egfr_base)+quartile_income+CREA_base_miss ,data=wide_df),
#                 age_base="Age",
#                 drop.reference="binary")
# test
