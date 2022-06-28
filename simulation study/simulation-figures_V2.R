


rm(list=ls())
library(here)
source(here::here("0_config.R"))

#--------------------------------
# Load simulation results
#--------------------------------

# resdf_glm <- readRDS(paste0(here::here(),"/data/sim_res_glm_ic.RDS"))
# resdf_Qint <- readRDS(paste0(here::here(),"/data/sim_res_Qint_ic.RDS"))
# resdf_ic <- readRDS(paste0(here::here(),"/data/sim_res_ic.RDS"))
# res_tmle_var <- readRDS(paste0(here::here(),"/data/sim_res_tmle.RDS"))
# resdf_EN_ic <- readRDS(paste0(here::here(),"/data/sim_res_EN_ic.RDS"))
# resdf_EN_Qint <- readRDS(paste0(here::here(),"/data/sim_res_EN_Qint_ic.RDS"))
# resdf_gcomp <- readRDS(paste0(here::here(),"/data/sim_res_gcomp.RDS"))
#
# plotdf <- bind_rows(
#   resdf_glm %>% mutate(analysis="glm"),
#   resdf_Qint %>% mutate(analysis="LTMLE - LASSO, Q-intercept, IC variance"),
#   resdf_ic %>% mutate(analysis="LTMLE - LASSO, IC variance"),
#   resdf_gcomp %>% mutate(analysis="G-Comp - LASSO, IC variance"),
#   res_tmle_var %>% mutate(analysis="LTMLE - LASSO, TMLE variance"),
#   resdf_EN_ic %>% mutate(analysis="LTMLE - EN, IC variance"),
#   resdf_EN_Qint %>% mutate(analysis="LTMLE - EN, Q-intercept, IC variance")
# )
#
# head(plotdf)
resdf_unadj <- readRDS(paste0(here::here(),"/data/sim_res_unadj.RDS"))
resdf_unadj_Qint <- readRDS(paste0(here::here(),"/data/sim_res_unadj_Qint.RDS"))

resdf_glm <- readRDS(paste0(here::here(),"/data/sim_res_glm_ic.RDS"))
resdf_Qint <- readRDS(paste0(here::here(),"/data/sim_res_Qint_ic.RDS"))
resdf_ic <- readRDS(paste0(here::here(),"/data/sim_res_ic.RDS"))
resdf_EN_ic <- readRDS(paste0(here::here(),"/data/sim_res_EN.RDS"))
resdf_EN_Qint <- readRDS(paste0(here::here(),"/data/sim_res_Qint_EN.RDS"))
resdf_gcomp <- readRDS(paste0(here::here(),"/data/sim_res_gcomp.RDS"))


resdf_noDetQ_ic <- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_ic.RDS"))
resdf_noDetQ_tmle <- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_tmle.RDS"))
resdf_noDetQ_Qint_ic <- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_Qint_ic.RDS"))
resdf_noDetQ_Qint_tmle <- readRDS(paste0(here::here(),"/data/sim_res_noDetQ_Qint_tmle.RDS"))

resdf_Qint_noDetQ_lasso_prescreen <- readRDS(paste0(here::here(),"/data/sim_res_Qint_noDetQ_lasso_prescreen.RDS"))

plotdf <- bind_rows(
  resdf_unadj %>% mutate(analysis="unadj"),
  resdf_unadj_Qint %>% mutate(analysis="unadj, Q-intercept"),
  resdf_noDetQ_ic %>% mutate(analysis="LASSO no DetQ IC"),
  resdf_noDetQ_tmle %>% mutate(analysis="LASSO no DetQ tmle"),
  resdf_noDetQ_Qint_ic %>% mutate(analysis="LASSO no DetQ Qint IC"),
  resdf_noDetQ_Qint_tmle %>% mutate(analysis="LASSO no DetQ Qint tmle"),
  resdf_Qint_noDetQ_lasso_prescreen %>% mutate(analysis="GLM Q-intercept LASSO prescreen -TMLE var")
  )

head(plotdf)





#--------------------------------
# Set truth
#--------------------------------

plotdf$true.RR <- 0.336

plotdf$true.RR.common <- 0.4345383

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
