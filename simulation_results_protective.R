



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
#new
#res_protective <- calc_sim_performance(WD=paste0(here::here(),"/sim_res/protective/"), files=files, boot_iter_files=NULL, trueRR=0.5148661, trueRD= -0.009395516, iptw=T )
#res_protective



res_diff <- res_protective$perf_tab_diff

#Set Qint to NA if IPTW
res_diff$Qint[res_diff$iptw=="IPTW"] <- "NA"

#Set DetQ to NA if IPTW and drop duplicates
res_diff$DetQ[res_diff$iptw=="IPTW"] <- "NA"



#Mark estimators
res_diff$estimator <- "LASSO"
res_diff$estimator[grepl("rf",res_diff$filenames)] <- "Random Forest"
res_diff$estimator[grepl("glm",res_diff$filenames)] <- "GLM"
res_diff$estimator[grepl("ridge",res_diff$filenames)] <- "Ridge"
res_diff$estimator[grepl("EN",res_diff$filenames)] <- "Elastic Net"
res_diff$estimator[grepl("AUC",res_diff$filenames)] <- "LASSO, AUC fit"
res_diff$estimator[grepl("_1se",res_diff$filenames)] <- "LASSO, 1se fit"
res_diff$estimator[grepl("lasso_prescreen",res_diff$filenames)] <- "GLM, LASSO prescreen"


# #remove duplicates /iptw repeats
res_diff <- res_diff %>% filter(!filenames %in% c("sim_res_ridge_ic_v3_iptw","sim_res_ridge",
                                                  "sim_res_Qint_ic",
                                                  "sim_res_noDetQ_Qint_ic", "sim_res_noDetQ_Qint_tmle",
                                                  "sim_res_DetQ_ic","sim_res_DetQ_ic_v2",
                                                  #"sim_res_Qint_ic_v3_iptw",
                                                  "sim_res_DetQ__ridge_ic_v3_iptw","sim_res_ridge_ic_v3",
                                                  #"sim_res_DetQ_ic_v3_iptw",
                                                  "sim_res_ic"))

res_diff <- res_diff %>% distinct(estimator, iptw, DetQ, Qint, .keep_all = TRUE) %>% arrange(iptw, estimator,  Qint, DetQ, bias, variance)

res_diff[res_diff$filenames=="sim_res_DetQ_ic_v3",]

#-------------------------------------
#To fix/RERUN!
#res_diff <- res_diff %>% filter(!(filenames %in% c("sim_res_Qint_EN","sim_res_Qint_AUC_1se"))) %>% as.data.frame()
#-------------------------------------

res_diff %>% select(estimator, iptw, DetQ, Qint, o.coverage, bias, filenames) %>% as.data.frame()



# To change in organization
# cgnage method to "Estimator" and put as first column
# change estimator to "algorithm" and put as second column





## Performance of difference variance estimators on null data

# **Notes:**
#
#   * Only showing LASSO estimator results-all estimator performances assessed in the realistic simulated data below.
# * Sanity-check on estimation performance on data with a known null association between GLP1 and dementia.
# * The IC variance estimator is anti-conservative and the TMLE variance estimator is conservative.
# * The bootstrap is anti-conservative but less so than the IC variance estimator.
# * The TMLE estimator is very conservative, with CI widths 8-10X that of the bootstrap.
# * The IPTW estimator is uniformly biased with overly-wide confidence intervals in all simulations (not shown).







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
res_v3_diff_coverage <- res_diff %>% subset(., select = -c(bias_se_ratio_emp, power, censoring_in_data, mean_ci_width, simulated_data))
res_v3_diff_coverage <- res_v3_diff_coverage %>% filter(variance_estimator=="ic" | is.na(variance_estimator), !is.na(bias))

#unique(res_v3_diff_coverage$filenames)
# res_v3_diff_coverage$estimator <- "LASSO"
# res_v3_diff_coverage$estimator[grepl("rf",res_v3_diff_coverage$filenames)] <- "Random Forest"
# res_v3_diff_coverage$estimator[grepl("glm",res_v3_diff_coverage$filenames)] <- "GLM"
# res_v3_diff_coverage$estimator[grepl("ridge",res_v3_diff_coverage$filenames)] <- "Ridge"
# res_v3_diff_coverage$estimator[grepl("EN",res_v3_diff_coverage$filenames)] <- "Elastic Net"
# res_v3_diff_coverage$estimator[grepl("lasso_prescreen",res_v3_diff_coverage$filenames)] <- "GLM, LASSO prescreen"
# res_v3_diff_coverage$estimator[grepl("_DetQ",res_v3_diff_coverage$filenames)] <- paste0(res_v3_diff_coverage$estimator[grepl("_DetQ",res_v3_diff_coverage$filenames)],", Det-Q")
# res_v3_diff_coverage$estimator[grepl("_detQ",res_v3_diff_coverage$filenames)] <- paste0(res_v3_diff_coverage$estimator[grepl("_detQ",res_v3_diff_coverage$filenames)],", Det-Q")
#res_v3_diff_coverage$estimator[grepl("_iptw",res_v3_diff_coverage$filenames)] <- paste0(res_v3_diff_coverage$estimator[grepl("_iptw",res_v3_diff_coverage$filenames)],", IPTW")


#res_v3_diff_coverage$estimator[res_v3_diff_coverage$Qint=="Yes"] <- paste0(res_v3_diff_coverage$estimator[res_v3_diff_coverage$Qint=="Yes"],", Q-intercept")
res_v3_diff_coverage$estimator[grepl("1se",res_v3_diff_coverage$filenames)] <- paste0(res_v3_diff_coverage$estimator[grepl("1se",res_v3_diff_coverage$filenames)],", Lambda: 1se")
res_v3_diff_coverage$estimator[grepl("AUC",res_v3_diff_coverage$filenames)] <- paste0(res_v3_diff_coverage$estimator[grepl("AUC",res_v3_diff_coverage$filenames)],", AUC fit")

res_v3_diff_coverage <- res_v3_diff_coverage %>%
  select(filenames, estimator, Qint, DetQ, iptw, bias, variance, mse, bias_se_ratio, o.coverage, filenames) %>% arrange(o.coverage)  %>% rename(oracle.coverage=o.coverage) %>% distinct()

#knitr::kable(res_v3_diff_coverage, digits =6)


res_table <- res_v3_diff_coverage  %>%
  filter(!(filenames %in% c("sim_res_noDetQ_Qint_tmle",
                            "sim_res_DetQ_ic_v2",
                            "sim_res_DetQ_ic_v2_iptw"#, "sim_res_Qint_ic"
  )),
  estimator!="LASSO, Lambda: 1se, AUC fit", estimator!="Random Forest") %>%
  filter(!is.na(variance)) %>% arrange(iptw,estimator,  Qint, DetQ,  bias, variance)


# identify index of rows to highlight
row.i.1 <- which(res_table$filenames=="sim_res_DetQ_ic_v3")

res_table <- res_table %>% select(filenames, estimator,
  iptw, Qint, DetQ,
  bias,variance,mse,bias_se_ratio,oracle.coverage) %>%
  rename(Estimator=estimator,  `Q-int`=Qint, `Det. Q`=DetQ, Method=iptw, `Bias/SE`=bias_se_ratio,
         Bias=bias, Variance=variance, `Oracle coverage`=oracle.coverage)

row.i.1 <- which(res_table$filenames=="sim_res_DetQ_ic_v3")
res_table <- res_table %>% subset(., select = -c(filenames))



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

res_xtable

save_kable(res_xtable, file="C:/Users/andre/Documents/jici/diab-dementia-server-code/tables/sim_results_table.tex",float = FALSE)



