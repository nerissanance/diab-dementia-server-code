


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
boot_iter_files <- boot_iter_files[grepl("_v3",boot_iter_files)]
length(boot_iter_files)


old_sim_res_v3 <- calc_sim_performance(files, boot_iter_files=boot_iter_files, trueRR=0.5148661, trueRD= -0.009683665, iptw=F )


view(old_sim_res_v3$perf_tab_diff)
view(old_sim_res_v3$perf_tab_RR)





#---------------------------------------------------------
# old sim
#---------------------------------------------------------
 files_all <- dir(path=paste0(here::here(),"/sim_res/"), pattern = "*.RDS")
# files <- files[grepl("old_null_sim_res_",files)]
# files <- files[grepl("_T11",files)]
files <- c("sim_res_Qint_noDetQ_lasso_prescreen.RDS",
           "sim_res_Qint_1se_int.RDS",
           "sim_res_1se_int.RDS",
           "sim_res_glm_ic.RDS",
           "sim_res_gcomp.RDS",
           "sim_res_noDetQ_ic.RDS",
           "sim_res_noDetQ_tmle.RDS",
           "sim_res_noDetQ_Qint_ic.RDS",
           "sim_res_noDetQ_Qint_tmle.RDS",
            "sim_res_noDetQ_Qint_ic_V2.RDS",
            "sim_res_noDetQ_Qint_tmle_V2.RDS",
           "sim_res_noDetQ_Qint_ic_no_cens.RDS",
           "sim_res_noDetQ_Qint_tmle_no_cens.RDS",
           "sim_res_ic.RDS",
           "/data/sim_res_Qint_ic.RDS",
           "/data/sim_res_EN.RDS",
           "/data/sim_res_Qint_EN.RDS",
           "/data/sim_res_AUC.RDS",
           "/data/sim_res_Qint_AUC.RDS",
           "/data/sim_res_1se.RDS",
           "/data/sim_res_Qint_1se.RDS",
           "/data/sim_res_AUC_1se.RDS",
           "/data/sim_res_Qint_AUC_1se.RDS",
           "/data/sim_res_rf.RDS"
           )

files <- files[files %in% files_all]

setwd(paste0(here::here(),"/data/"))
load(file=paste0(here::here(),"/results/truth_rare.Rdata"))

# d1<-readRDS("sim_res_noDetQ_ic.RDS")
# d2<-readRDS("sim_res_noDetQ_ic_v2.RDS")
# d2<-readRDS("sim_res_noDetQ_ic_no_cens.RDS")
# res1 <- calc_sim_performance(files=c("sim_res_noDetQ_ic.RDS",
#                                      "sim_res_noDetQ_tmle.RDS","sim_res_noDetQ_Qint_tmle_est_update.RDS"), boot_iter_files=NULL, 0.6924793 , -0.005929)
# res1$perf_tab_RR
# res1$perf_tab_diff

# d <- files %>% map(readRDS) %>% map_dfr(~bind_rows(.) , .id="analysis")
# d <- d %>% mutate(analysis = factor(analysis))
# levels(d$analysis) = files[as.numeric(levels(d$analysis))]
# d$analysis <- gsub(".RDS","",d$analysis)

#load bootstrap
boot_iter_files <- dir(path=paste0(here::here(),"/data/bootstrap/"), pattern = "*.RDS")
boot_iter_files <- boot_iter_files[grepl("sim_res_boot_old_sim_cens_competing_risks_500_iter_",boot_iter_files)]
length(boot_iter_files)

boot_iter_files <- dir(path=paste0(here::here(),"/data/bootstrap/"), pattern = "*.RDS")
boot_iter_files2 <- boot_iter_files[grepl("old_sim_",boot_iter_files)]
length(boot_iter_files2)

#old_sim_res <- calc_sim_performance(files, boot_iter_files=boot_iter_files, 0.6924793 , -0.005929 )
old_sim_res <- calc_sim_performance(files, boot_iter_files=boot_iter_files, trueRR=0.3430989, trueRD= -0.01292504, iptw=F )
old_sim_res_v3 <- calc_sim_performance(files, boot_iter_files=boot_iter_files, trueRR=0.5148661, trueRD= -0.009683665, iptw=F )


tab<-old_sim_res$perf_tab_RR
tab<-tab %>% select(variance_estimator, Qint,  DetQ, o.coverage, bias, variance,mse, bias_se_ratio, coverage, mean_ci_width, filenames)

knitr::kable(tab, digits = 3)


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

original_sim_res_null <- calc_sim_performance(files, boot_iter_files=boot_iter_files, 1, 0)
original_sim_res_null$perf_tab_RR
original_sim_res_null$perf_tab_RD
tab<-original_sim_res_null$perf_tab_RR
tab<-tab %>% select(variance_estimator, Qint,  DetQ, o.coverage, bias, variance,mse, bias_se_ratio, coverage, mean_ci_width)

knitr::kable(tab, digits = 3)

save(original_sim_res_null,  file=paste0(here::here(),"/results/sim_performance_results_original_null.Rdata"))

