


rm(list=ls())
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))
source(paste0(here::here(),"/simulation study/0_simulation_cleaning_functions.R"))


#---------------------------------------------------------
# v3
#---------------------------------------------------------
files <- dir(path=paste0(here::here(),"/sim_res/"), pattern = "*.RDS")
files <- files[grepl("_v3",files)]

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
old_sim_res_v3 <- calc_sim_performance(files=files, boot_iter_files=boot_iter_files, trueRR=0.5148661, trueRD= -0.009683665, iptw=T )

# view(old_sim_res_v3$perf_tab_diff)
# view(old_sim_res_v3$perf_tab_RR)

old_sim_res_v3_diff <- old_sim_res_v3$perf_tab_diff
old_sim_res_v3_RR <- old_sim_res_v3$perf_tab_RR
save(old_sim_res_v3_diff, old_sim_res_v3_RR,  file=paste0(here::here(),"/results/sim_performance_results_original.Rdata"))



#---------------------------------------------------------
# Null, old sim
#---------------------------------------------------------
files <- dir(path=paste0(here::here(),"/sim_res/"), pattern = "*.RDS")
files <- files[grepl("old_null_sim_res_",files)]
files <- files[grepl("_T11",files)]

#load bootstrap
boot_iter_files <- dir(path=paste0(here::here(),"/data/bootstrap/"), pattern = "*.RDS")
boot_iter_files <- boot_iter_files[grepl("sim_res_boot_old_sim_null_T11_",boot_iter_files)]
length(boot_iter_files)


trueRR=1
trueRD=0
iptw=T
original_sim_res_null <- calc_sim_performance(files=files, boot_iter_files=boot_iter_files, trueRR=1, trueRD=0, iptw=T)
original_sim_res_null$perf_tab_RR
original_sim_res_null$perf_tab_RD


old_sim_res_null_diff <- original_sim_res_null$perf_tab_diff
old_sim_res_null_RR <- original_sim_res_null$perf_tab_RR

save(old_sim_res_null_diff, old_sim_res_null_RR,  file=paste0(here::here(),"/results/sim_performance_results_original_null.Rdata"))

#---------------------------------------------------------
# Outcome blind
#---------------------------------------------------------

#T3
# RR: 0.5462801
# RD: -0.008447575
#T10
# RR: 0.5133281
# RD: -0.02607657


files_ob_t4 <- dir(path=paste0(here::here(),"/sim_res/"), pattern = "*.RDS")
files_ob_t4 <- files_ob_t4[grepl("outcome_blind_",files_ob_t4)]
files_ob_t4 <- files_ob_t4[grepl("_T4",files_ob_t4)]
files_ob_t4 <- files_ob_t4[!grepl("sim_res_boot_",files_ob_t4)]

files_ob_t11 <- dir(path=paste0(here::here(),"/sim_res/"), pattern = "*.RDS")
files_ob_t11 <- files_ob_t11[grepl("outcome_blind_",files_ob_t11)]
files_ob_t11 <- files_ob_t11[grepl("_T11",files_ob_t11)]
files_ob_t11 <- files_ob_t11[!grepl("sim_res_boot_",files_ob_t11)]


#load bootstrap
boot_iter_files_ob_t4 <- dir(path=paste0(here::here(),"/data/bootstrap/"), pattern = "*.RDS")
boot_iter_files_ob_t4 <- boot_iter_files_ob_t4[grepl("blind",boot_iter_files_ob_t4)]
boot_iter_files_ob_t4 <- boot_iter_files_ob_t4[grepl("_T4",boot_iter_files_ob_t4)]
boot_iter_files_ob_t4 <- boot_iter_files_ob_t4[!grepl("_null",boot_iter_files_ob_t4)]
boot_iter_files_ob_t4 <- boot_iter_files_ob_t4[!grepl("_subsampled",boot_iter_files_ob_t4)]
boot_iter_files_ob_t4 <- boot_iter_files_ob_t4[!grepl("_old",boot_iter_files_ob_t4)]
length(boot_iter_files_ob_t4)

boot_iter_files_ob_t4 <- data.frame(boot_file =boot_iter_files_ob_t4, analysis=NA)
boot_iter_files_ob_t4$analysis[grepl("outcome_blind_cens_competing_risks_noDetQ",boot_iter_files_ob_t4$boot_file)] <- "no DetQ"
boot_iter_files_ob_t4$analysis[grepl("sim_res_boot_outcome_blind_T",boot_iter_files_ob_t4$boot_file)] <- "DetQ-old"
boot_iter_files_ob_t4$analysis[grepl("sim_res_boot_outcome_blind_cens_competing_risks_T",boot_iter_files_ob_t4$boot_file)] <- "DetQ"
boot_iter_files_ob_t4$analysis[grepl("_no_ties_cens_competing_risks",boot_iter_files_ob_t4$boot_file)] <- "no ties"
boot_iter_files_ob_t4$analysis[grepl("_n500",boot_iter_files_ob_t4$boot_file)] <- "DetQ, 500 iter"
boot_iter_files_ob_t4$analysis[grepl("sim_res_boot_no_ties_outcome_blind_cens_competing_risks_T",boot_iter_files_ob_t4$boot_file)] <- "no ties"


boot_iter_files_ob_t4 <- boot_iter_files_ob_t4 %>% filter(analysis!="DetQ-old")
sim_res_ob_t4 <- calc_sim_performance(files=files_ob_t4, boot_iter_files=boot_iter_files_ob_t4, trueRR=0.5450597, trueRD=-0.008528233, iptw=F)
sim_res_ob_t10 <- calc_sim_performance(files=files_ob_t11, boot_iter_files=NULL, trueRR=0.5232744, trueRD=-0.02527015, iptw=F)

view(sim_res_ob_t10$perf_tab_diff)

#Check the truth without death/cens

files=files_ob_t4
boot_iter_files=boot_iter_files_ob_t4
trueRR=0.5133281
trueRD=-0.008447575
iptw=F


#---------------------------------------------------------
# Null, outcome blind
#---------------------------------------------------------


files_ob_null_t4 <- dir(path=paste0(here::here(),"/sim_res/"), pattern = "*.RDS")
files_ob_null_t4 <- files_ob_null_t4[grepl("null",files_ob_null_t4)]
files_ob_null_t4 <- files_ob_null_t4[grepl("T4",files_ob_null_t4)]
files_ob_null_t4 <- files_ob_null_t4[!grepl("old",files_ob_null_t4)]

files_ob_null_t11 <- dir(path=paste0(here::here(),"/sim_res/"), pattern = "*.RDS")
files_ob_null_t11 <- files_ob_null_t11[grepl("null",files_ob_null_t11)]
files_ob_null_t11 <- files_ob_null_t11[grepl("T11",files_ob_null_t11)]
files_ob_null_t11 <- files_ob_null_t11[!grepl("old",files_ob_null_t11)]


#load bootstrap
boot_iter_files_ob_null_t4 <- dir(path=paste0(here::here(),"/data/bootstrap/"), pattern = "*.RDS")
boot_iter_files_ob_null_t4 <- boot_iter_files_ob_null_t4[grepl("sim_res_boot_null_4",boot_iter_files_ob_null_t4)]
length(boot_iter_files_ob_null_t4)

# boot_iter_files_ob_null_t11 <- dir(path=paste0(here::here(),"/data/bootstrap/"), pattern = "*.RDS")
# boot_iter_files_ob_null_t11 <- boot_iter_files_ob_null_t11[!grepl("_old",boot_iter_files_ob_null_t11)]
# boot_iter_files_ob_null_t11 <- boot_iter_files_ob_null_t11[grepl("_null",boot_iter_files_ob_null_t11)]


sim_res_ob_null_t4 <- calc_sim_performance(files=files_ob_null_t4, boot_iter_files=boot_iter_files_ob_null_t4, trueRR=1, trueRD=0, iptw=F)
sim_res_ob_null_t10 <- calc_sim_performance(files=files_ob_null_t11, boot_iter_files=NULL, trueRR=1, trueRD=0, iptw=F)

