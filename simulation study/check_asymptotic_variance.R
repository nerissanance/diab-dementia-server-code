


rm(list=ls())
library(here)
source(here::here("0_config.R"))

#--------------------------------
# Load simulation results
#--------------------------------


resdf_Qint_noDetQ_lasso_prescreen<- readRDS(paste0(here::here(),"/data/sim_res_Qint_noDetQ_lasso_prescreen.RDS"))
resdf_Qint_noDetQ_lasso_prescreen2<- readRDS(paste0(here::here(),"/data/sim_res_Qint_noDetQ_lasso_prescreen_100_200_reps.RDS"))


plotdf <- bind_rows(
  resdf_Qint_noDetQ_lasso_prescreen[1:10,] %>% mutate(analysis="GLM Q-intercept LASSO prescreen -10 reps"),
  resdf_Qint_noDetQ_lasso_prescreen[1:25,] %>% mutate(analysis="GLM Q-intercept LASSO prescreen -25 reps"),
  resdf_Qint_noDetQ_lasso_prescreen[1:50,] %>% mutate(analysis="GLM Q-intercept LASSO prescreen -50 reps"),
  resdf_Qint_noDetQ_lasso_prescreen %>% mutate(analysis="GLM Q-intercept LASSO prescreen -100 reps"),
  bind_rows(resdf_Qint_noDetQ_lasso_prescreen,resdf_Qint_noDetQ_lasso_prescreen2[1:25,]) %>% mutate(analysis="GLM Q-intercept LASSO prescreen -125 reps"),
  bind_rows(resdf_Qint_noDetQ_lasso_prescreen,resdf_Qint_noDetQ_lasso_prescreen2[1:50,]) %>% mutate(analysis="GLM Q-intercept LASSO prescreen -150 reps"),
  bind_rows(resdf_Qint_noDetQ_lasso_prescreen,resdf_Qint_noDetQ_lasso_prescreen2[1:75,]) %>% mutate(analysis="GLM Q-intercept LASSO prescreen -175 reps"),
  bind_rows(resdf_Qint_noDetQ_lasso_prescreen,resdf_Qint_noDetQ_lasso_prescreen2) %>% mutate(analysis="GLM Q-intercept LASSO prescreen -200 reps")
  ) %>%
  mutate(analysis=factor(analysis, levels=unique(analysis)))






#--------------------------------
# Set truth
#--------------------------------

plotdf$true.RR <- 0.392377


#--------------------------------
# Calc bias, variance, and MSE
#--------------------------------

perf_tab <- plotdf %>% group_by(analysis) %>%
                       summarize(coverage=mean(CI.2.5.<true.RR & true.RR<CI.97.5.)*100,
                                 bias=mean(log(estimate))-log(true.RR),
                                 #calculate variance of the estimator
                                 #https://www.statlect.com/fundamentals-of-statistics/variance-estimation
                                 #http://www.dliebl.com/RM_ES_Script/estimation-theory.html
                                 #variance=mean(std.dev^2),
                                 variance=mean((mean(log(estimate))-log(estimate))^2),
                                 #variance2=mean((mean(estimate)-estimate)^2),
                                 #bias_std_ratio=mean(abs(log(estimate) - log(true.RR))/std.dev),
                                 mse=variance + bias^2#,
                                 #power=mean((CI.2.5. > 1 & CI.97.5.>1)|(CI.2.5. < 1 & CI.97.5.<1))
                                 ) %>%
  distinct()
perf_tab

knitr::kable(perf_tab)
