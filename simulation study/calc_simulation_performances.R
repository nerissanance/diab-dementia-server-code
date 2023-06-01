


rm(list=ls())
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))
source(paste0(here::here(),"/simulation study/0_simulation_cleaning_functions.R"))





#---------------------------------------------------------
# manuscript sim results: Null sim
#---------------------------------------------------------
files <- dir(path=paste0(here::here(),"/sim_res/"), pattern = "*.RDS")
#files <- files[grepl("old_null_sim_res_",files)]
files <- files[grepl("_null_",files)]
files <- files[grepl("_T11",files)]

#old_null_sim_res_ic_glm_T11
#temp <- readRDS(paste0(here::here(),"/sim_res/sim_res_RF_ic_v3.RDS"))
# head(temp)

#NOTE: check which files are missing from the old pipeline:
old_files <- dir(path=paste0(here::here(),"/sim_res_old/"), pattern = "*.RDS")
old_files <- old_files[grepl("old_null_sim_res_",old_files)]
old_files <- old_files[grepl("_T11",old_files)]


files_temp <- files[!(files %in% c( "" ))]
old_files <- old_files[!(old_files %in% c( "" ))]
files_temp <- gsub("_T11","", files_temp)
old_files <- gsub("_T11","", old_files)
files_temp <- gsub("old_null_sim_res_","", files_temp)
old_files <- gsub("old_null_sim_res_","", old_files)

files_temp[!(files_temp %in% old_files)]
old_files[!(old_files %in% files_temp)]


#load bootstrap
boot_iter_files <- dir(path=paste0(here::here(),"/data/bootstrap/"), pattern = "*.RDS")
boot_iter_files <- boot_iter_files[grepl("sim_res_boot_old_sim_null_T11_",boot_iter_files)]
length(boot_iter_files)


trueRR=1
trueRD=0
iptw=T
original_sim_res_null <- calc_sim_performance_old(files=files, boot_iter_files=boot_iter_files, trueRR=1, trueRD=0, iptw=T)
original_sim_res_null$perf_tab_RR
original_sim_res_null$perf_tab_diff
original_sim_res_null$perf_tab_diff$filenames


res_null_diff <- original_sim_res_null$perf_tab_diff
res_null_RR <- original_sim_res_null$perf_tab_RR

save(res_null_diff, res_null_RR,  file=paste0(here::here(),"/results/sim_performance_results_original_null.Rdata"))


#---------------------------------------------------------
# manuscript sim results: protective sim
#---------------------------------------------------------
files <- dir(path=paste0(here::here(),"/sim_res/"), pattern = "*.RDS")
#files <- files[grepl("_v3",files)]
files <- files[!grepl("_null_",files)]

#NOTE: check which files are missing from the old pipeline:
old_files <- dir(path=paste0(here::here(),"/sim_res_old/"), pattern = "*.RDS")
old_files <- old_files[grepl("_v3",old_files)]
old_files <- gsub("_v3","",old_files)


files_temp <- files[!(files %in% c( "sim_res_1se.RDS","sim_res_AUC.RDS" ))]
old_files <- old_files[!(old_files %in% c( "sim_res_1se_ic.RDS","sim_res_AUC_ic.RDS" ))]


files_temp <- gsub(".RDS","", files_temp)
old_files <- gsub("_T11","", old_files)

files_temp[!(files_temp %in% old_files)]
old_files[!(old_files %in% files_temp)]


boot_iter_files <- dir(path=paste0(here::here(),"/data/bootstrap/"), pattern = "*.RDS")
unique(gsub("\\d","",boot_iter_files))

boot_iter_files_500iter <- boot_iter_files[grepl("sim_res_boot_old_sim_cens_competing_risks_500_iter_T11",boot_iter_files)]
boot_iter_files <- boot_iter_files[grepl("_v3",boot_iter_files)|grepl("sim_res_boot_original_sim_T11_",boot_iter_files)|grepl("sim_res_boot_old_sim_detQ_T11",boot_iter_files)]
#boot_iter_files <- boot_iter_files[!grepl("ridge",boot_iter_files)]
boot_iter_files <- c(boot_iter_files, boot_iter_files_500iter)
boot_iter_files <- data.frame(boot_file = boot_iter_files, analysis=NA)
boot_iter_files$analysis[grepl("500_iter",boot_iter_files$boot_file)] <- "DetQ, 500 iter"
boot_iter_files$analysis[grepl("sim_res_boot_old_sim_detQ_T11",boot_iter_files$boot_file)] <- "DetQ"
boot_iter_files$analysis[grepl("ridge",boot_iter_files$boot_file)] <- "Ridge"
boot_iter_files$analysis[grepl("cens_competing_risks_v3",boot_iter_files$boot_file) | grepl("sim_res_boot_original_sim_T11_",boot_iter_files$boot_file)] <- "no DetQ"
table(boot_iter_files$analysis)




trueRR=0.5148661
trueRD= -0.009683665
iptw=T



res_v3 <- calc_sim_performance_old(files=files, boot_iter_files=boot_iter_files, trueRR=trueRR, trueRD= trueRD, iptw=T )
res_v3$perf_tab_diff

res_v3_diff <- res_v3$perf_tab_diff
res_v3_RR <- res_v3$perf_tab_RR


save(res_v3_diff, res_v3_RR,  file=paste0(here::here(),"/results/sim_performance_results_original.Rdata"))



# #---------------------------------------------------------
# # Outcome blind
# #---------------------------------------------------------
#
# #T3
# # RR: 0.5462801
# # RD: -0.008447575
# #T10
# # RR: 0.5133281
# # RD: -0.02607657
#
#
# files_ob_t4 <- dir(path=paste0(here::here(),"/sim_res/"), pattern = "*.RDS")
# files_ob_t4 <- files_ob_t4[grepl("outcome_blind_",files_ob_t4)]
# files_ob_t4 <- files_ob_t4[grepl("_T4",files_ob_t4)]
# files_ob_t4 <- files_ob_t4[!grepl("sim_res_boot_",files_ob_t4)]
#
# files_ob_t11 <- dir(path=paste0(here::here(),"/sim_res/"), pattern = "*.RDS")
# files_ob_t11 <- files_ob_t11[grepl("outcome_blind_",files_ob_t11)]
# files_ob_t11 <- files_ob_t11[grepl("_T11",files_ob_t11)]
# files_ob_t11 <- files_ob_t11[!grepl("sim_res_boot_",files_ob_t11)]
#
#
# #load bootstrap
# boot_iter_files_ob_t4 <- dir(path=paste0(here::here(),"/data/bootstrap/"), pattern = "*.RDS")
# boot_iter_files_ob_t4 <- boot_iter_files_ob_t4[grepl("blind",boot_iter_files_ob_t4)]
# boot_iter_files_ob_t4 <- boot_iter_files_ob_t4[grepl("_T4",boot_iter_files_ob_t4)]
# boot_iter_files_ob_t4 <- boot_iter_files_ob_t4[!grepl("_null",boot_iter_files_ob_t4)]
# boot_iter_files_ob_t4 <- boot_iter_files_ob_t4[!grepl("_subsampled",boot_iter_files_ob_t4)]
# boot_iter_files_ob_t4 <- boot_iter_files_ob_t4[!grepl("_old",boot_iter_files_ob_t4)]
# length(boot_iter_files_ob_t4)
#
# boot_iter_files_ob_t4 <- data.frame(boot_file =boot_iter_files_ob_t4, analysis=NA)
# boot_iter_files_ob_t4$analysis[grepl("outcome_blind_cens_competing_risks_noDetQ",boot_iter_files_ob_t4$boot_file)] <- "no DetQ"
# boot_iter_files_ob_t4$analysis[grepl("sim_res_boot_outcome_blind_T",boot_iter_files_ob_t4$boot_file)] <- "DetQ-old"
# boot_iter_files_ob_t4$analysis[grepl("sim_res_boot_outcome_blind_cens_competing_risks_T",boot_iter_files_ob_t4$boot_file)] <- "DetQ"
# boot_iter_files_ob_t4$analysis[grepl("_no_ties_cens_competing_risks",boot_iter_files_ob_t4$boot_file)] <- "no ties"
# boot_iter_files_ob_t4$analysis[grepl("_n500",boot_iter_files_ob_t4$boot_file)] <- "DetQ, 500 iter"
# boot_iter_files_ob_t4$analysis[grepl("sim_res_boot_no_ties_outcome_blind_cens_competing_risks_T",boot_iter_files_ob_t4$boot_file)] <- "no ties"
#
#
# boot_iter_files_ob_t4 <- boot_iter_files_ob_t4 %>% filter(analysis!="DetQ-old")
# sim_res_ob_t4 <- calc_sim_performance(files=files_ob_t4, boot_iter_files=boot_iter_files_ob_t4, trueRR=0.5450597, trueRD=-0.008528233, iptw=F)
# sim_res_ob_t10 <- calc_sim_performance(files=files_ob_t11, boot_iter_files=NULL, trueRR=0.5232744, trueRD=-0.02527015, iptw=F)
#
#
# #---------------------------------------------------------
# # Null, outcome blind
# #---------------------------------------------------------
#
#
# files_ob_null_t4 <- dir(path=paste0(here::here(),"/sim_res/"), pattern = "*.RDS")
# files_ob_null_t4 <- files_ob_null_t4[grepl("null",files_ob_null_t4)]
# files_ob_null_t4 <- files_ob_null_t4[grepl("T4",files_ob_null_t4)]
# files_ob_null_t4 <- files_ob_null_t4[!grepl("old",files_ob_null_t4)]
#
# files_ob_null_t11 <- dir(path=paste0(here::here(),"/sim_res/"), pattern = "*.RDS")
# files_ob_null_t11 <- files_ob_null_t11[grepl("null",files_ob_null_t11)]
# files_ob_null_t11 <- files_ob_null_t11[grepl("T11",files_ob_null_t11)]
# files_ob_null_t11 <- files_ob_null_t11[!grepl("old",files_ob_null_t11)]
#
#
# #load bootstrap
# boot_iter_files_ob_null_t4 <- dir(path=paste0(here::here(),"/data/bootstrap/"), pattern = "*.RDS")
# boot_iter_files_ob_null_t4 <- boot_iter_files_ob_null_t4[grepl("sim_res_boot_null_4",boot_iter_files_ob_null_t4)]
# length(boot_iter_files_ob_null_t4)
#
# # boot_iter_files_ob_null_t11 <- dir(path=paste0(here::here(),"/data/bootstrap/"), pattern = "*.RDS")
# # boot_iter_files_ob_null_t11 <- boot_iter_files_ob_null_t11[!grepl("_old",boot_iter_files_ob_null_t11)]
# # boot_iter_files_ob_null_t11 <- boot_iter_files_ob_null_t11[grepl("_null",boot_iter_files_ob_null_t11)]
#
#
# sim_res_ob_null_t4 <- calc_sim_performance(files=files_ob_null_t4, boot_iter_files=boot_iter_files_ob_null_t4, trueRR=1, trueRD=0, iptw=F)
# sim_res_ob_null_t10 <- calc_sim_performance(files=files_ob_null_t11, boot_iter_files=NULL, trueRR=1, trueRD=0, iptw=F)
#
