


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
# Save data
#--------------------------------
saveRDS(sim_df, file=paste0(here::here(),"/results/compiled_simulatation_results.rds"))
