



rm(list=ls())
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))
source(paste0(here::here(),"/simulation study/0_simulation_cleaning_functions.R"))
library(knitr)
library(xtable)
library(kableExtra)




#---------------------------------------------------------
# manuscript sim results: Null sim
#---------------------------------------------------------
files_null <- dir(path=paste0(here::here(),"/sim_res/null/"), pattern = "*.RDS")

# #load bootstrap
# boot_iter_files <- dir(path=paste0(here::here(),"/data/bootstrap/"), pattern = "*.RDS")
# boot_iter_files <- boot_iter_files[grepl("sim_res_boot_old_sim_null_T11_",boot_iter_files)]
# length(boot_iter_files)


trueRR=1
trueRD=0
iptw=T
original_sim_res_null <- calc_sim_performance(WD=paste0(here::here(),"/sim_res/null/"), files=files_null, boot_iter_files=NULL, trueRR=1, trueRD=0, iptw=T)




res_null_diff <- original_sim_res_null$perf_tab_diff
res_null_diff$filenames

#Set Qint to NA if IPTW
res_null_diff$Qint[res_null_diff$iptw=="IPTW"] <- "NA"

#Set DetQ to NA if IPTW and drop duplicates
res_null_diff$DetQ[res_null_diff$iptw=="IPTW"] <- "NA"


#Mark estimators
res_null_diff$estimator <- "LASSO"
res_null_diff$estimator[grepl("rf",res_null_diff$filenames)] <- "Random Forest"
res_null_diff$estimator[grepl("glm",res_null_diff$filenames)] <- "GLM"
res_null_diff$estimator[grepl("ridge",res_null_diff$filenames)] <- "Ridge"
res_null_diff$estimator[grepl("EN",res_null_diff$filenames)] <- "Elastic Net"
res_null_diff$estimator[grepl("AUC",res_null_diff$filenames)] <- "LASSO, AUC fit"
res_null_diff$estimator[grepl("_1se",res_null_diff$filenames)] <- "LASSO, 1se fit"
res_null_diff$estimator[grepl("lasso_prescreen",res_null_diff$filenames)] <- "GLM, LASSO prescreen"

res_null_diff <- res_null_diff %>% distinct(estimator, iptw, DetQ, Qint, .keep_all = TRUE) %>% arrange(iptw, estimator,  Qint, DetQ, bias, variance)

res_null_diff %>% select(estimator, iptw, DetQ, Qint, o.coverage, bias, filenames) %>% as.data.frame()

# To change in organization
# cgnage method to "Estimator" and put as first column
# change estimator to "algorithm" and put as second column


res_null_diff_coverage <- res_null_diff %>%
  distinct(estimator, iptw, Qint, DetQ,bias,variance,mse,bias_se_ratio,o.coverage, .keep_all = TRUE) %>%
  #mutate(filenames=gsub("old_null_sim_res_","",filenames)) %>%
  #filter(variance_estimator=="ic") %>%
  #filter(!(filenames %in% c("noDetQ_Qint_tmle_T11")))  %>%
  # filter(filenames %in% c("old_null_sim_res_noDetQ_ic_T11","old_null_sim_res_noDetQ_Qint_ic_T11","old_null_sim_res_ic_glm_T11","old_null_sim_res_noDetQ_ic_glm_T11","old_null_sim_res_Qint_noDetQ_glm_tmle_T11")) %>%
  select(filenames, estimator, iptw, Qint, DetQ,bias,variance,mse,bias_se_ratio,o.coverage, ) %>% rename(oracle.coverage=o.coverage)



res_null_table <- res_null_diff_coverage %>% select(filenames,iptw, estimator, Qint, DetQ,bias,variance,mse,bias_se_ratio,oracle.coverage) %>%
  rename(Estimator=estimator,  `Q-int`=Qint, `Det. Q`=DetQ, Method=iptw, `Bias/SE`=bias_se_ratio,
         Bias=bias, Variance=variance, `Oracle coverage`=oracle.coverage) %>% arrange(Method, Estimator, `Oracle coverage`)

# identify index of rows to highlight
row.i.1 <- which(res_null_table$filenames=="old_null_sim_res_ic_T11")
res_null_table <- res_null_table %>% subset(., select = -c(filenames))

print(as.data.frame(res_null_table))

res_null_xtable <- res_null_table %>%
  knitr::kable(
    format = "latex",
    align = "l",
    booktabs = TRUE,
    longtable = TRUE,
    linesep = "",
    digits =5) %>%
  kableExtra::kable_styling(
    position = "left") %>%
  kable_styling()%>%
  row_spec(row.i.1-1, hline_after = T) %>%
  row_spec(row.i.1, bold=T,hline_after = T)

res_null_xtable

save_kable(res_null_xtable, file="C:/Users/andre/Documents/jici/diab-dementia-server-code/tables/null_sim_results_table.tex",float = FALSE)

