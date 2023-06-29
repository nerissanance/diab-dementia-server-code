



rm(list=ls())
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))
source(paste0(here::here(),"/simulation study/0_simulation_cleaning_functions.R"))
library(knitr)
library(xtable)
library(kableExtra)


files <- dir(path=paste0(here::here(),"/sim_res/protective/"), pattern = "*.RDS")


res_protective <- calc_sim_performance(WD=paste0(here::here(),"/sim_res/protective/"), files=files, boot_iter_files=NULL, trueRR=0.5148661, trueRD= -0.009683665, iptw=T )
res_diff <- res_protective$perf_tab_diff

res_protective <- calc_sim_performance(WD=paste0(here::here(),"/sim_res/protective/"), files=files, boot_iter_files=NULL, trueRR=0.5148661, trueRD= -0.0354950000, iptw=T )
res_diff <- res_protective$perf_tab_diff




#Remove no Det Q
res_diff <- res_diff %>% filter(DetQ!="No")


#Mark estimators
res_diff$estimator <- "LASSO"
res_diff$estimator[grepl("rf",res_diff$filenames)] <- "Random Forest"
res_diff$estimator[grepl("glm",res_diff$filenames)] <- "GLM"
res_diff$estimator[grepl("ridge",res_diff$filenames)] <- "Ridge"
res_diff$estimator[grepl("gbound",res_diff$filenames)] <- "G bound:"
res_diff$estimator[grepl("EN",res_diff$filenames)] <- "Elastic Net"
res_diff$estimator[grepl("AUC",res_diff$filenames)] <- "LASSO, AUC fit"
res_diff$estimator[grepl("_1se",res_diff$filenames)] <- "LASSO, Lambda: 1se"
res_diff$estimator[grepl("lasso_prescreen",res_diff$filenames)] <- "GLM, LASSO prescreen"

res_diff %>% filter(estimator=="GLM", !grepl("iptw", filenames)) %>% select(filenames)

# #remove duplicates /iptw repeats
res_diff <- res_diff %>% filter(!filenames %in% c("sim_res_ridge_ic_v3_iptw","sim_res_ridge",
                                                  "sim_res_Qint_ic",
                                                  "sim_res_noDetQ_Qint_ic", "sim_res_noDetQ_Qint_tmle",
                                                  "sim_res_DetQ_ic","sim_res_DetQ_ic_v2", "sim_res_DetQ_ic_v5",
                                                  "sim_res_DetQ__ridge_ic_v3_iptw","sim_res_ridge_ic_v3",
                                                  "sim_res_ic"))


# filter(!(filenames %in% c("sim_res_noDetQ_Qint_tmle",
#                           "sim_res_DetQ_ic_v2",
#                           "sim_res_DetQ_ic_v2_iptw"
# )),


#Set Qint to NA if IPTW
res_diff$Qint[res_diff$iptw=="IPTW"] <- "NA"

#Set DetQ to NA if IPTW and drop duplicates
res_diff$DetQ[res_diff$iptw=="IPTW"] <- "NA"

res_diff_raw <- res_diff

res_diff <- res_diff %>% distinct(estimator, iptw, DetQ, Qint, .keep_all = TRUE) %>% arrange(iptw, estimator,  Qint, DetQ, bias, variance)


#-------------------------------------
#To fix/RERUN!
#res_diff <- res_diff %>% filter(!(filenames %in% c("sim_res_Qint_EN","sim_res_Qint_AUC_1se"))) %>% as.data.frame()
#-------------------------------------

res_diff %>% select(estimator, iptw, DetQ, Qint, o.coverage, bias, filenames) %>% as.data.frame()








# Scenario 2: Realistic simulation, protective effect of GLP1 on dementia


# True Risk Difference: -0.009683665
#
#
# ## Comparison of different estimators' performance
#
# **Notes:**
#
#   * Based on these results, we chose the LASSO estimator with Q-prediction and no deterministic Q function
# * Several of the estimators have comparable performance, but the chosen estimator performs best in both RR and RD estimation
# * Ridge regressions have lower MSE but not perfect 95% oracle coverage
# * Including the deterministic Q function marginally decreases bias/variance, so we should use in the bootstrap estimator


### Risk difference

#colnames(res_v3_diff)
#unique(res_v3_diff$filenames)
res_diff_coverage <- res_diff %>% subset(., select = -c(bias_se_ratio_emp, power, censoring_in_data, mean_ci_width, simulated_data))
res_diff_coverage <- res_diff_coverage %>% filter(variance_estimator=="ic" | is.na(variance_estimator), !is.na(bias))

res_diff_coverage <- res_diff_coverage %>%
  select(filenames, estimator, Qint, DetQ, iptw, bias, variance, mse, bias_se_ratio, o.coverage, filenames) %>% arrange(o.coverage)  %>% rename(oracle.coverage=o.coverage) %>% distinct()

#knitr::kable(res_diff_coverage, digits =6)


res_table <- res_diff_coverage  %>%
  # filter(!(filenames %in% c("sim_res_noDetQ_Qint_tmle",
  #                           "sim_res_DetQ_ic_v2",
  #                           "sim_res_DetQ_ic_v5",
  #                           "sim_res_DetQ_ic_v2_iptw"#, "sim_res_Qint_ic"
  # )),
  # estimator!="LASSO, Lambda: 1se, AUC fit", estimator!="Random Forest") %>%
  filter(!is.na(variance)) %>% arrange(iptw,estimator,  Qint, DetQ,  bias, variance)


# identify index of rows to highlight
row.i.1 <- which(res_table$filenames=="sim_res_DetQ_ic_v3")

res_table <- res_table %>% select(filenames,  iptw,
                                  estimator, Qint,
  bias,variance,bias_se_ratio,oracle.coverage) %>%
  rename(Algorithm=estimator,  `Q-int`=Qint,  Estimator=iptw, `Bias/SE`=bias_se_ratio,
         Bias=bias, Variance=variance, `Oracle coverage`=oracle.coverage)

row.i.1 <- which(res_table$filenames=="sim_res_DetQ_ic_v3")
#res_table <- res_table %>% subset(., select = -c(filenames))


#save for html file
res_table_protective <- res_table
res_table_protective_raw <- res_diff_raw
save(res_table_protective, res_table_protective_raw, file=paste0(here::here(),"/results/sim_performance_results_protective.Rdata"))

print(as.data.frame(res_table))

res_xtable <- res_table %>%
  knitr::kable(
    format = "latex",
    align = "l",
    booktabs = TRUE,
    longtable = TRUE,
    linesep = "",
    digits =6) %>%
  kableExtra::kable_styling(
    position = "left"#,
    # latex_options = c("striped", "repeat_header"),
    # stripe_color = "gray!15"
  ) %>%
  kable_styling()%>%
  row_spec(row.i.1-1, hline_after = T) %>%
  row_spec(row.i.1, bold=T,hline_after = T)


save_kable(res_xtable, file="C:/Users/andre/Documents/jici/diab-dementia-server-code/tables/sim_results_table.tex",float = FALSE)



