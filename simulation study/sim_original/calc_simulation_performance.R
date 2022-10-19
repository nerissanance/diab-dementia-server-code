


rm(list=ls())
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))

#--------------------------------
# Load simulation results
#--------------------------------





#Unadjusted
sim_df <- bind_rows(
  resdf_noDetQ_Qint_tmle_unadj<- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_Qint_tmle_unadj.RDS")) %>% mutate(analysis="unadj, Q-intercept, no detQ, tmle"),
  resdf_noDetQ_tmle_unadj<- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_tmle_unadj.RDS"))  %>% mutate(analysis="unadj, no detQ, tmle"),
  resdf_noDetQ_Qint_ic_tmle_unadj<- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_Qint_ic_unadj.RDS")) %>% mutate(analysis="unadj, Q-intercept, no detQ, ic"),
  resdf_noDetQ_ic_tmle_unadj<- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_ic_unadj.RDS"))  %>% mutate(analysis="unadj, no detQ, ic"),

  #common outcomes
  resdf_tmle_common <- readRDS(paste0(here::here(),"/data/sim_res_resdf_tmle_common_tmle.RDS")) %>% mutate(analysis="LASSO tmle - common outcome"),
  resdf_noDetQ_Qint_tmle_common <- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_Qint_tmle_common.RDS")) %>% mutate(analysis="LASSO no DetQ Qint tmle - common outcome"),
  resdf_ic_common <- readRDS(paste0(here::here(),"/data/sim_res_resdf_tmle_common.RDS"))  %>% mutate(analysis="LASSO IC - common outcome"),

  #adjusted -other
  resdf_noDetQ_Qint_AUC<- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_Qint_tmle_AUC.RDS")) %>% mutate(analysis="LASSO no DetQ Qint tmle AUC"),
  resdf_noDetQ_tmle<- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_tmle.RDS")) %>% mutate(analysis="LASSO no DetQ tmle"),
  #resdf_noDetQ_tmle2<- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_tmle_new.RDS")) %>% mutate(analysis="LASSO no DetQ tmle new"),
  resdf_noDetQ_tmle3<- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_tmle_est_update.RDS")) %>% mutate(analysis="LASSO no DetQ tmle updated estimator"),
  resdf_Qint_noDetQ_lasso_prescreen<- readRDS(paste0(here::here(),"/data/sim_res_Qint_noDetQ_lasso_prescreen.RDS")) %>% mutate(analysis="GLM Q-intercept LASSO prescreen -TMLE var"),
  #resdf_Qint_noDetQ_lasso_prescreen2<- readRDS(paste0(here::here(),"/data/sim_res_Qint_noDetQ_lasso_prescreen_100_200_reps.RDS")) %>% mutate(analysis="GLM Q-intercept LASSO prescreen -TMLE var, alt"),
  resdf_noDetQ_ic<- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_ic_est_update.RDS")) %>% mutate(analysis="LASSO no DetQ IC updated estimator"),
  resdf_noDetQ_Qint_tmle<- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_Qint_tmle_est_update.RDS")) %>% mutate(analysis="LASSO no DetQ Qint tmle updated estimator"),


  resdf_noDetQ_Qint_tmle<- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_Qint_tmle.RDS"))  %>% mutate(analysis="LASSO no DetQ Qint tmle"),
  resdf_noDetQ_Qint_tmle_interaction <- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_Qint_tmle_interaction.RDS"))  %>% mutate(analysis="LASSO no DetQ Qint tmle- interactions"),

  #adjusted -ridge

  resdf_noDetQ_tmle_ridge<- readRDS(paste0(here::here(),"/data/sim_res_ridge_noDetQ_tmle_est_update.RDS")) %>% mutate(analysis="Ridge no DetQ tmle - est update"),
  resdf_noDetQ_ic_ridge<- readRDS(paste0(here::here(),"/data/sim_res_ridge_noDetQ_ic_est_update.RDS")) %>% mutate(analysis="Ridge no DetQ ic - est update"),
  resdf_noDetQ_Qint_tmle_ridge<- readRDS(paste0(here::here(),"/data/sim_res_ridge_noDetQ_Qint_tmle3.RDS")) %>% mutate(analysis="Ridge no DetQ Qint tmle"),
  # resdf_noDetQ_Qint_tmle_ridge_alt_data<- readRDS(paste0(here::here(),"/data/sim_res_ridge_noDetQ_Qint_tmle2.RDS"))  %>% mutate(analysis="Ridge no DetQ Qint tmle, alt"),
  # resdf_noDetQ_Qint_tmle_ridge_1se<- readRDS(paste0(here::here(),"/data/sim_res_ridge_1se_noDetQ_Qint_tmle.RDS")) %>% mutate(analysis="Ridge no DetQ Qint tmle 1se"),
  resdf_noDetQ_Qint_tmle_ridge_iptw<- readRDS(paste0(here::here(),"/data/sim_res_ridge_noDetQ_Qint_tmle2.RDS")) %>%
    select(ate.log.std.err,iptw.long.name,
             iptw.estimate,iptw.sd,iptw.pval,iptw.ci.lb,iptw.ci.ub,
             iptw.log.std.err,iptw.ate.long.name,iptw.ate,iptw.ate.sd,iptw.ate.pval,
             iptw.ate.ci.lb,iptw.ate.ci.ub,iptw.ate.log.std.err,label ) %>%
    rename(estimate   =iptw.estimate, std.dev  =iptw.sd, pvalue=iptw.pval,CI.2.5.   =iptw.ci.lb,
           CI.97.5. =iptw.ci.ub,
           log.std.err=iptw.log.std.err,ate.long.name=iptw.ate.long.name,
           ate=iptw.ate,ate.sd=iptw.ate.sd,ate.pval=iptw.ate.pval,
           ate.ci.lb=iptw.ate.ci.lb,ate.ci.ub=iptw.ate.ci.ub) %>% mutate(analysis="Ridge no DetQ Qint iptw"),
  resdf_noDetQ_Qint_tmle_ridge_1se_iptw<- readRDS(paste0(here::here(),"/data/sim_res_ridge_1se_noDetQ_Qint_tmle.RDS")) %>%
    select(ate.log.std.err,iptw.long.name,
           iptw.estimate,iptw.sd,iptw.pval,iptw.ci.lb,iptw.ci.ub,
           iptw.log.std.err,iptw.ate.long.name,iptw.ate,iptw.ate.sd,iptw.ate.pval,
           iptw.ate.ci.lb,iptw.ate.ci.ub,iptw.ate.log.std.err,label ) %>%
    rename(estimate   =iptw.estimate, std.dev  =iptw.sd, pvalue=iptw.pval,CI.2.5.   =iptw.ci.lb,
           CI.97.5. =iptw.ci.ub,
           log.std.err=iptw.log.std.err,ate.long.name=iptw.ate.long.name,
           ate=iptw.ate,ate.sd=iptw.ate.sd,ate.pval=iptw.ate.pval,
           ate.ci.lb=iptw.ate.ci.lb,ate.ci.ub=iptw.ate.ci.ub) %>%
        mutate(analysis="Ridge no DetQ Qint iptw 1se"),

  #  resdf_noDetQ_Qint_tmle_ridge_gbound1_9<- readRDS(paste0(here::here(),"/data/sim_res_ridge_noDetQ_Qint_tmle_gbound1_9.RDS")) %>% mutate(analysis="Ridge no DetQ Qint tmle gbound 0.1-0.9"),
  # resdf_noDetQ_Qint_tmle_ridge_gbound05<- readRDS(paste0(here::here(),"/data/sim_res_ridge_noDetQ_Qint_tmle_gbound05.RDS")) %>% mutate(analysis="Ridge no DetQ Qint tmle gbound 0.05-1"),
  # resdf_noDetQ_Qint_tmle_ridge_gbound005_9<- readRDS(paste0(here::here(),"/data/sim_res_ridge_noDetQ_Qint_tmle_gbound005_9.RDS")) %>% mutate(analysis="Ridge no DetQ Qint tmle gbound 0.005-0.9"),
  # resdf_noDetQ_Qint_tmle_ridge_gbound001<- readRDS(paste0(here::here(),"/data/sim_res_ridge_noDetQ_Qint_tmle_gbound001.RDS")) %>% mutate(analysis="Ridge no DetQ Qint tmle gbound 0.001"),

  # resdf_noDetQ_tmle_EN<- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_EN_tmle.RDS")) %>% mutate(analysis="Elastic Net no DetQ tmle"),
  # resdf_noDetQ_Qint_tmle_EN<- readRDS(paste0(here::here(),"/data/sim_res_EN_noDetQ_Qint_tmle.RDS")) %>% mutate(analysis="Elastic Net no DetQ Qint tmle"),
   resdf_AUC <- readRDS(paste0(here::here(),"/data/sim_res_AUC_Qint_tmle.RDS")) %>% mutate(analysis="LASSO Qint tmle AUC")#,
  # resdf_ic <- readRDS(paste0(here::here(),"/data/sim_res_ic.RDS"))%>% mutate(analysis="LASSO IC"),
  # resdf_noDetQ_ic <- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_ic.RDS")) %>% mutate(analysis="LASSO no DetQ IC"),
  # resdf_noDetQ_Qint_ic <- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_Qint_ic.RDS")) %>% mutate(analysis="LASSO no DetQ Qint IC")
)

unique(sim_df$analysis)

#--------------------------------
# load bootstrap
#--------------------------------

boot_iter_files <- dir(path=paste0(here::here(),"/data/bootstrap/"), pattern = "*.RDS")
boot_iter_files <- boot_iter_files[grepl("_T11",boot_iter_files)]
boot_iter_files <- boot_iter_files[!grepl("_null",boot_iter_files)]
boot_iter_files <- boot_iter_files[!grepl("_subsampled",boot_iter_files)]
boot_iter_files <- boot_iter_files[!grepl("_outcome_blind",boot_iter_files)]
length(boot_iter_files)

setwd(paste0(here::here(),"/data/bootstrap/"))
boot_res <- boot_iter_files %>% map(readRDS) %>% map_dfr(~bind_rows(.) , .id="boot_iter")

#calc bootstrap CI's
boot_CIs <- boot_res %>% group_by(boot_iter) %>%
  summarise(
    CI1=quantile(estimate,.025),
    CI2=quantile(estimate,.975),
    ate.CI1=quantile(ate,.025),
    ate.CI2=quantile(ate,.975)
  )
boot_CIs


#--------------------------------
# Set truth
#--------------------------------

load(paste0(here::here(),"/results/truth_rare.Rdata"))
load(paste0(here::here(),"/results/truth_common.Rdata"))


sim_df$true.RR <- cRR
sim_df$true.RD <- cRD

sim_df$true.RR[grepl("common outcome",sim_df$analysis)] <- cRR_common
sim_df$true.RD[grepl("common outcome",sim_df$analysis)] <- cRD_common



#--------------------------------
#calculate bootstrap performance
#--------------------------------

perf_tab_RR_boot <- data.frame(
  analysis = "Bootstrap - clustered ID",
  coverage = mean(boot_CIs$CI1 <= sim_df$true.RR[1] &  boot_CIs$CI2 >= sim_df$true.RR[1])*100,
  mean_ci_width=mean(log(boot_CIs$CI2)-log(boot_CIs$CI1)),
  power=mean((boot_CIs$CI1 > 1 & boot_CIs$CI2>1)|(boot_CIs$CI1 < 1 & boot_CIs$CI2<1))*100
)

perf_tab_diff_boot <- data.frame(
  analysis = "Bootstrap - clustered ID",
  coverage = mean(boot_CIs$ate.CI1 <= sim_df$true.RD[1] &  boot_CIs$ate.CI2 >= sim_df$true.RD[1])*100,
  mean_ci_width=mean((boot_CIs$ate.CI2)-(boot_CIs$ate.CI1)),
  power=mean((boot_CIs$ate.CI1 > 0 & boot_CIs$ate.CI2>0)|(boot_CIs$ate.CI1 < 0 & boot_CIs$ate.CI2<0))*100
)
perf_tab_RR <- bind_rows(perf_tab_RR, perf_tab_RR_boot)
perf_tab_diff <- bind_rows(perf_tab_diff, perf_tab_diff_boot)



#temp drop extreme outliers
sim_df <- sim_df%>% filter(estimate > 1/128)

perf_tab_RR <- sim_df %>% group_by(analysis) %>%
  mutate(variance=mean((estimate-mean(estimate))^2), #check if the brackets in this formula are right
         variance_log=mean((log(estimate)-mean(log(estimate)))^2),
         RD.variance=mean((mean(ate)-ate)^2),
         o.ci.lb = estimate - 1.96 * sqrt(variance),
         o.ci.ub = estimate + 1.96 * sqrt(variance),
         o.ci.lb2 = log(estimate) - 1.96 * sqrt(variance_log),
         o.ci.ub2 = log(estimate) + 1.96 * sqrt(variance_log)) %>%
  summarize(
    bias_logRR=mean(log(estimate))-log(true.RR),
    variance_logRR=mean((mean(log(estimate))-log(estimate))^2),
    mse = bias_logRR^2 + variance_logRR,
    bias_se_ratio= bias_logRR/sqrt(variance_logRR),
    coverage=mean(CI.2.5.<=true.RR & true.RR<=CI.97.5.)*100,
    #oracle coverage
    o.coverage=mean(o.ci.lb<=true.RR & true.RR<= o.ci.ub)*100,
    o.coverage_logRR=mean(o.ci.lb2<=log(true.RR) & log(true.RR)<= o.ci.ub2)*100,
    mean_ci_width_logRR=mean(log(CI.97.5.)-log(CI.2.5.)),
    power=mean((CI.2.5. > 1 & CI.97.5.>1)|(CI.2.5. < 1 & CI.97.5.<1))*100,
    o.power=mean((o.ci.lb > 0 & o.ci.ub>0)|(o.ci.lb < 0 & o.ci.ub<0))*100
  ) %>%
  distinct()

perf_tab_RR_old <- bind_rows(perf_tab_RR, perf_tab_RR_boot)

res <- perf_tab_RR %>% arrange(mse)

res %>% filter(analysis %in% c("LASSO no DetQ tmle updated estimator","LASSO no DetQ IC updated estimator","Bootstrap - clustered ID"))
"LASSO no DetQ IC updated estimator"
#--------------------------------
# Save data
#--------------------------------
saveRDS(sim_df, file=paste0(here::here(),"/results/compiled_simulatation_results.rds"))

save(perf_tab_RR_old, file=paste0(here::here(),"/results/simulation_results_old.Rdata"))


