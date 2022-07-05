


rm(list=ls())
library(here)
source(here::here("0_config.R"))

#--------------------------------
# Load simulation results
#--------------------------------

resdf_noDetQ_Qint_tmle_unadj<- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_Qint_tmle_unadj.RDS"))
resdf_noDetQ_tmle_unadj<- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_tmle_unadj.RDS"))
resdf_noDetQ_ic_tmle_unadj<- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_Qint_ic_unadj.RDS"))
resdf_noDetQ_Qint_AUC<- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_Qint_tmle_AUC.RDS"))
resdf_noDetQ_tmle<- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_tmle.RDS"))
resdf_Qint_noDetQ_lasso_prescreen<- readRDS(paste0(here::here(),"/data/sim_res_Qint_noDetQ_lasso_prescreen.RDS"))
resdf_noDetQ_Qint_tmle<- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_Qint_tmle.RDS"))
resdf_noDetQ_Qint_tmle_ridge<- readRDS(paste0(here::here(),"/data/sim_res_ridge_noDetQ_Qint_tmle.RDS"))
resdf_noDetQ_Qint_tmle_EN<- readRDS(paste0(here::here(),"/data/sim_res_EN_noDetQ_Qint_tmle.RDS"))
resdf_AUC <- readRDS(paste0(here::here(),"/data/sim_res_AUC_Qint_tmle.RDS"))
resdf_noDetQ_Qint_tmle_common <- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_Qint_tmle_common.RDS"))


# resdf_unadj <- readRDS(paste0(here::here(),"/data/sim_res_unadj.RDS"))
# resdf_unadj_Qint <- readRDS(paste0(here::here(),"/data/sim_res_unadj_Qint.RDS"))
#
# resdf_glm <- readRDS(paste0(here::here(),"/data/sim_res_glm_ic.RDS"))
# resdf_Qint <- readRDS(paste0(here::here(),"/data/sim_res_Qint_ic.RDS"))
# resdf_ic <- readRDS(paste0(here::here(),"/data/sim_res_ic.RDS"))
# resdf_EN_ic <- readRDS(paste0(here::here(),"/data/sim_res_EN.RDS"))
# resdf_EN_Qint <- readRDS(paste0(here::here(),"/data/sim_res_Qint_EN.RDS"))
# resdf_gcomp <- readRDS(paste0(here::here(),"/data/sim_res_gcomp.RDS"))
#
#
# resdf_noDetQ_ic <- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_ic.RDS"))
# resdf_noDetQ_tmle <- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_tmle.RDS"))
# resdf_noDetQ_Qint_ic <- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_Qint_ic.RDS"))
# resdf_noDetQ_Qint_tmle <- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_Qint_tmle.RDS"))
#
# resdf_Qint_noDetQ_lasso_prescreen <- readRDS(paste0(here::here(),"/data/sim_res_Qint_noDetQ_lasso_prescreen.RDS"))

plotdf <- bind_rows(
  resdf_noDetQ_tmle_unadj  %>% mutate(analysis="unadj, tmle, no detQ"),
  resdf_noDetQ_Qint_tmle_unadj %>% mutate(analysis="unadj, Q-intercept, no detQ, tmle"),
  #resdf_noDetQ_ic_tmle_unadj %>% mutate(analysis="unadj, Q-intercept, no detQ, ic"),
  #resdf_noDetQ_ic %>% mutate(analysis="LASSO no DetQ IC"),
  resdf_noDetQ_tmle %>% mutate(analysis="LASSO no DetQ tmle"),
  #resdf_noDetQ_Qint_ic %>% mutate(analysis="LASSO no DetQ Qint IC"),
  resdf_noDetQ_Qint_tmle %>% mutate(analysis="LASSO no DetQ Qint tmle"),
  resdf_noDetQ_Qint_tmle_common %>% mutate(analysis="LASSO no DetQ Qint tmle - common outcome"),
  resdf_noDetQ_Qint_tmle_ridge %>% mutate(analysis="Ridge no DetQ Qint tmle"),
  resdf_noDetQ_Qint_tmle_EN %>% mutate(analysis="Elastic Net no DetQ Qint tmle"),
  resdf_AUC %>% mutate(analysis="LASSO Qint tmle AUC"),
  resdf_noDetQ_Qint_AUC %>% mutate(analysis="LASSO no DetQ Qint tmle AUC"),
  resdf_Qint_noDetQ_lasso_prescreen %>% mutate(analysis="GLM Q-intercept LASSO prescreen -TMLE var")
  )






#--------------------------------
# Set truth
#--------------------------------

plotdf$true.RR <- 0.392377

plotdf$true.RR[plotdf$analysis=="LASSO no DetQ Qint tmle - common outcome"] <- 0.6483788

#--------------------------------
# Calc bias, variance, and MSE
#--------------------------------

perf_tab <- plotdf %>% group_by(analysis) %>%
                       summarize(coverage=mean(CI.2.5.<true.RR & true.RR<CI.97.5.)*100,
                                 bias=mean(abs(log(estimate) - log(true.RR))),
                                 variance=mean(std.dev^2),
                                 bias_std_ratio=mean(abs(log(estimate) - log(true.RR))/std.dev),
                                 mse=variance + bias^2)
res <- perf_tab %>% arrange(mse)
res %>% select(analysis,  bias, variance,    mse, coverage)

plotdf <- left_join(plotdf,perf_tab, by="analysis") %>% arrange(mse) %>%
          mutate(analysis=factor(analysis, levels=unique(analysis)))

#Try out glmnet with interactions (all, or just with interactions with A)
# How many continious with spline terms


#--------------------------------
# Plots
#--------------------------------

set.seed(123)
ggplot(plotdf, aes(y=analysis, x=estimate)) +
  geom_jitter(width=0, height=0.05, alpha=0.75) +
  geom_vline(xintercept = 1) +
  geom_vline(aes(xintercept = true.RR), linetype="dashed") +
  scale_x_continuous(trans = "log10")


# ggplot(plotdf, aes(x=estimate, color=analysis, fill=analysis)) +
#   facet_wrap(~analysis) +
#   geom_density() +
#   geom_vline(xintercept = 1) +
#   geom_vline(aes(xintercept = true.RR), linetype="dashed") +
#   scale_x_continuous(trans = "log10")


ggplot(plotdf, aes(x=estimate)) +
  facet_wrap(~analysis) +
  geom_density() +
  geom_vline(xintercept = 1) +
  geom_vline(aes(xintercept = true.RR), linetype="dashed") +
  scale_x_continuous(trans = "log10") + theme_bw()

#
# ggplot(plotdf, aes(x=estimate)) +
#   facet_grid(analysis~.) +
#   geom_density(fill="grey70") +
#   geom_vline(xintercept = 1) +
#   geom_vline(aes(xintercept = true.RR), linetype="dashed") +
#   scale_x_continuous(trans = "log10") + theme_bw()




# To do:

# gcomp with and without Qint
# full ltmle with and without Qint



#Ideas for speed:
  # -change order of samples
 #  -downsample the non-glp1 use data?
  # savio
  #parallelization on linux for ltmle?

  #use dopar or mcapply instead of for loop
