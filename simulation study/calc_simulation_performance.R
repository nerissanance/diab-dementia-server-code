


rm(list=ls())
library(here)
source(here::here("0_config.R"))

#--------------------------------
# Load simulation results
#--------------------------------

#unadjusted

rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))




#Unadjusted
resdf_noDetQ_Qint_tmle_unadj<- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_Qint_tmle_unadj.RDS"))
resdf_noDetQ_tmle_unadj<- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_tmle_unadj.RDS"))
resdf_noDetQ_Qint_ic_tmle_unadj<- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_Qint_ic_unadj.RDS"))
resdf_noDetQ_ic_tmle_unadj<- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_ic_unadj.RDS"))


#adjusted
resdf_noDetQ_Qint_AUC<- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_Qint_tmle_AUC.RDS"))
resdf_noDetQ_tmle<- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_tmle.RDS"))
resdf_Qint_noDetQ_lasso_prescreen<- readRDS(paste0(here::here(),"/data/sim_res_Qint_noDetQ_lasso_prescreen.RDS"))
resdf_Qint_noDetQ_lasso_prescreen2<- readRDS(paste0(here::here(),"/data/sim_res_Qint_noDetQ_lasso_prescreen_100_200_reps.RDS"))

resdf_noDetQ_Qint_tmle<- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_Qint_tmle.RDS"))
resdf_noDetQ_Qint_tmle_interaction <- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_Qint_tmle_interaction.RDS"))
resdf_noDetQ_Qint_tmle_ridge<- readRDS(paste0(here::here(),"/data/sim_res_ridge_noDetQ_Qint_tmle.RDS"))
resdf_noDetQ_Qint_tmle_EN<- readRDS(paste0(here::here(),"/data/sim_res_EN_noDetQ_Qint_tmle.RDS"))
resdf_AUC <- readRDS(paste0(here::here(),"/data/sim_res_AUC_Qint_tmle.RDS"))

#common outcomes
resdf_tmle_common <- readRDS(paste0(here::here(),"/data/sim_res_resdf_tmle_common_tmle.RDS"))
resdf_noDetQ_Qint_tmle_common <- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_Qint_tmle_common.RDS"))
resdf_ic_common <- readRDS(paste0(here::here(),"/data/sim_res_resdf_tmle_common.RDS"))
summary(resdf_noDetQ_Qint_tmle_common$estimate)


# resdf_unadj <- readRDS(paste0(here::here(),"/data/sim_res_unadj.RDS"))
# resdf_unadj_Qint <- readRDS(paste0(here::here(),"/data/sim_res_unadj_Qint.RDS"))
#
# resdf_glm <- readRDS(paste0(here::here(),"/data/sim_res_glm_ic.RDS"))
# resdf_Qint <- readRDS(paste0(here::here(),"/data/sim_res_Qint_ic.RDS"))
 resdf_ic <- readRDS(paste0(here::here(),"/data/sim_res_ic.RDS"))
# resdf_EN_ic <- readRDS(paste0(here::here(),"/data/sim_res_EN.RDS"))
# resdf_EN_Qint <- readRDS(paste0(here::here(),"/data/sim_res_Qint_EN.RDS"))
# resdf_gcomp <- readRDS(paste0(here::here(),"/data/sim_res_gcomp.RDS"))
#
#
 resdf_noDetQ_ic <- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_ic.RDS"))
# resdf_noDetQ_tmle <- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_tmle.RDS"))
 resdf_noDetQ_Qint_ic <- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_Qint_ic.RDS"))
# resdf_noDetQ_Qint_tmle <- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_Qint_tmle.RDS"))
#
# resdf_Qint_noDetQ_lasso_prescreen <- readRDS(paste0(here::here(),"/data/sim_res_Qint_noDetQ_lasso_prescreen.RDS"))

plotdf <- bind_rows(
  resdf_noDetQ_tmle_unadj  %>% mutate(analysis="unadj, no detQ, tmle"),
  resdf_noDetQ_Qint_tmle_unadj %>% mutate(analysis="unadj, Q-intercept, no detQ, tmle"),
  resdf_noDetQ_Qint_ic_tmle_unadj  %>% mutate(analysis="unadj, Q-intercept, no detQ, ic"),
  resdf_noDetQ_ic_tmle_unadj %>% mutate(analysis="unadj, no detQ, ic"),

  resdf_ic_common %>% mutate(analysis="LASSO IC - common outcome"),
  resdf_tmle_common %>% mutate(analysis="LASSO tmle - common outcome"),
  resdf_noDetQ_Qint_tmle_common %>% mutate(analysis="LASSO no DetQ Qint tmle - common outcome"),


  resdf_ic %>% mutate(analysis="LASSO IC"),
  resdf_noDetQ_ic %>% mutate(analysis="LASSO no DetQ IC"),
  resdf_noDetQ_tmle %>% mutate(analysis="LASSO no DetQ tmle"),
  resdf_noDetQ_Qint_ic %>% mutate(analysis="LASSO no DetQ Qint IC"),
  resdf_noDetQ_Qint_tmle %>% mutate(analysis="LASSO no DetQ Qint tmle"),
  resdf_noDetQ_Qint_tmle_interaction %>% mutate(analysis="LASSO no DetQ Qint tmle- interactions"),
  resdf_noDetQ_Qint_tmle_ridge %>% mutate(analysis="Ridge no DetQ Qint tmle"),
  resdf_noDetQ_Qint_tmle_EN %>% mutate(analysis="Elastic Net no DetQ Qint tmle"),
  resdf_AUC %>% mutate(analysis="LASSO Qint tmle AUC"),
  resdf_noDetQ_Qint_AUC %>% mutate(analysis="LASSO no DetQ Qint tmle AUC"),
  resdf_Qint_noDetQ_lasso_prescreen2 %>% mutate(analysis="GLM Q-intercept LASSO prescreen -TMLE var")
  )






#--------------------------------
# Set truth
#--------------------------------

load(paste0(here::here(),"/results/truth_rare.Rdata"))
load(paste0(here::here(),"/results/truth_common.Rdata"))


plotdf$true.RR <- cRR
plotdf$true.RD <- cRD

plotdf$true.RR[grepl("common outcome",plotdf$analysis)] <- cRR_common
plotdf$true.RD[grepl("common outcome",plotdf$analysis)] <- cRD_common

#--------------------------------
# Calc bias, variance, and MSE
#--------------------------------


perf_tab <- plotdf %>% group_by(analysis) %>%
  mutate(variance=mean((estimate-mean(estimate))^2), #check if the brackets in this formula are right

         RD.variance=mean((mean(ate)-ate)^2),

         o.ci.lb = estimate - 1.96 * sqrt(variance),
         o.ci.ub = estimate + 1.96 * sqrt(variance)) %>%
  summarize(coverage=mean(CI.2.5.<=true.RR & true.RR<=CI.97.5.)*100,
            bias=mean((estimate))-(true.RR),
            variance=mean((estimate-mean(estimate))^2),

           #oracle coverage
           o.coverage=mean(o.ci.lb<=true.RR & true.RR<= o.ci.ub)*100,
           #log-trans bias
            bias2=mean(log(estimate))-log(true.RR),
            variance2=mean((mean(log(estimate))-log(estimate))^2),
            bias3=exp(mean(log(estimate))-log(true.RR)),
            variance3=exp(mean((mean(log(estimate))-log(estimate))^2)),
            bias_std_ratio=bias/mean(std.dev),
            mean_est_variance= mean(std.dev^2),
            mean_ci_width=mean(CI.97.5.-CI.2.5.),
            mse=variance + bias^2,
            power=mean((CI.2.5. > 1 & CI.97.5.>1)|(CI.2.5. < 1 & CI.97.5.<1))*100,
            RD.coverage=mean(ate.ci.lb<=true.RD & true.RD<=ate.ci.ub)*100,
            RD.bias=mean(ate)-true.RD,
            RD.variance=mean((mean(ate)-ate)^2),
            RD.bias_std_ratio=RD.bias/mean(ate.sd),
            RD.mse=RD.variance + RD.bias^2,
            RD.power=mean((ate.ci.lb > 0 & ate.ci.ub>0)|(ate.ci.lb < 0 & ate.ci.ub<0))*100) %>%
  distinct()

res <- perf_tab %>% arrange(-coverage, mse)
res

knitr::kable(res)

#res %>% select(analysis,  bias, variance,    mse, coverage, power)

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




ggplot(plotdf, aes(x=estimate)) +
  facet_wrap(~analysis) +
  geom_density() +
  geom_vline(xintercept = 1) +
  geom_vline(aes(xintercept = true.RR), linetype="dashed") +
  scale_x_continuous(trans = "log10") + theme_bw()


  #use dopar or mcapply instead of for loop
