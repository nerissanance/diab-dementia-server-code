


rm(list=ls())
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))
source(paste0(here::here(),"/simulation study/0_simulation_cleaning_functions.R"))


#---------------------------------------------------------
# performance over time
#---------------------------------------------------------

truth_df <- readRDS(paste0(here::here(),"/data/sim_res_truth.RDS"))
sim_res_over_time <- readRDS(paste0(here::here(),"/data/sim_res_ic_t1-11.RDS")) %>% subset(., select = -c(true.RR,  true.RD))
sim_res_over_time_tmle <- readRDS(paste0(here::here(),"/data/sim_res_tmle_t1-11.RDS"))
sim_res_over_time$time <- as.numeric(gsub("Y","",sim_res_over_time$analysis))
sim_res_over_time_tmle$time <- as.numeric(gsub("Y","",sim_res_over_time_tmle$analysis))

sim_res_over_time <- left_join(sim_res_over_time, truth_df, by = c("time")) %>% rename(true.RR=RR,  true.RD=RD)
sim_res_over_time_tmle <- left_join(sim_res_over_time_tmle, truth_df, by = c("time")) %>% rename(true.RR=RR,  true.RD=RD)


perf_tab_over_time_RR <- sim_res_over_time %>% group_by(time) %>%
  mutate(variance=mean(((estimate)-mean((estimate)))^2),
         o.ci.lb = (estimate) - 1.96 * sqrt(variance),
         o.ci.ub = (estimate) + 1.96 * sqrt(variance)) %>%
  group_by(time) %>%
  summarize(
    bias=mean(log(estimate))-log(true.RR),
    variance=mean((mean(log(estimate))-log(estimate))^2),
    mse = bias^2 + variance,
    bias_se_ratio= bias/sqrt(variance),
    bias_se_ratio_emp= bias/mean(std.dev),
    coverage=mean(CI.2.5.<=true.RR & true.RR<=CI.97.5.)*100,
    #oracle coverage
    o.coverage=mean(o.ci.lb<=(true.RR) & (true.RR)<= o.ci.ub)*100,
    mean_ci_width=mean(log(CI.97.5.)-log(CI.2.5.))
  ) %>% filter(!is.na(variance)) %>%
  distinct()


perf_tab_over_time_diff <- sim_res_over_time %>% filter(!is.na(ate)) %>%
  subset(., select = -c(estimate, std.dev, CI.2.5., CI.97.5.)) %>%
  rename(estimate= ate, std.dev= ate.sd, CI.2.5.=ate.ci.lb, CI.97.5.=ate.ci.ub)%>%
  group_by(time) %>%
  mutate(variance=mean((estimate-mean(estimate))^2),
         o.ci.lb = estimate - 1.96 * sqrt(variance),
         o.ci.ub = estimate + 1.96 * sqrt(variance)) %>%
  group_by(time) %>%
  summarize(
    bias=mean((estimate))-(true.RD),
    variance=mean((estimate-mean(estimate))^2),
    mse = bias^2 + variance,
    bias_se_ratio= bias/sqrt(variance),
    bias_se_ratio_emp= bias/mean(std.dev),
    coverage=mean(CI.2.5.<=true.RD & true.RD<=CI.97.5.)*100,
    #oracle coverage
    o.coverage=mean(o.ci.lb<=true.RD & true.RD<= o.ci.ub)*100,
    mean_ci_width=mean((CI.97.5.)-(CI.2.5.))
  ) %>%
  distinct()



perf_tab_over_time_RR_tmle <- sim_res_over_time_tmle %>% group_by(time) %>%
  mutate(variance=mean(((estimate)-mean((estimate)))^2),
         o.ci.lb = (estimate) - 1.96 * sqrt(variance),
         o.ci.ub = (estimate) + 1.96 * sqrt(variance)) %>%
  group_by(time) %>%
  summarize(
    bias=mean(log(estimate))-log(true.RR),
    variance=mean((mean(log(estimate))-log(estimate))^2),
    mse = bias^2 + variance,
    bias_se_ratio= bias/sqrt(variance),
    bias_se_ratio_emp= bias/mean(std.dev),
    coverage=mean(CI.2.5.<=true.RR & true.RR<=CI.97.5.)*100,
    #oracle coverage
    o.coverage=mean(o.ci.lb<=(true.RR) & (true.RR)<= o.ci.ub)*100,
    mean_ci_width=mean(log(CI.97.5.)-log(CI.2.5.))
  ) %>% filter(!is.na(variance)) %>%
  distinct() %>% select(time, coverage, mean_ci_width)   %>% rename(TMLE_coverage =coverage, TMLE_mean_ci_width=mean_ci_width)


perf_tab_over_time_diff_tmle <- sim_res_over_time_tmle %>% filter(!is.na(ate)) %>%
  subset(., select = -c(estimate, std.dev, CI.2.5., CI.97.5.)) %>%
  rename(estimate= ate, std.dev= ate.sd, CI.2.5.=ate.ci.lb, CI.97.5.=ate.ci.ub)%>%
  group_by(time) %>%
  mutate(variance=mean((estimate-mean(estimate))^2),
         o.ci.lb = estimate - 1.96 * sqrt(variance),
         o.ci.ub = estimate + 1.96 * sqrt(variance)) %>%
  group_by(time) %>%
  summarize(
    bias=mean((estimate))-(true.RD),
    variance=mean((estimate-mean(estimate))^2),
    mse = bias^2 + variance,
    bias_se_ratio= bias/sqrt(variance),
    bias_se_ratio_emp= bias/mean(std.dev),
    coverage=mean(CI.2.5.<=true.RD & true.RD<=CI.97.5.)*100,
    #oracle coverage
    o.coverage=mean(o.ci.lb<=true.RD & true.RD<= o.ci.ub)*100,
    mean_ci_width=mean((CI.97.5.)-(CI.2.5.))
  ) %>%
  distinct() %>% select(time, coverage, mean_ci_width)  %>% rename(TMLE_coverage =coverage, TMLE_mean_ci_width=mean_ci_width)



perf_tab_over_time_RR <- perf_tab_over_time_RR %>% rename(IC_coverage =coverage, IC_mean_ci_width=mean_ci_width)
perf_tab_over_time_diff <- perf_tab_over_time_diff %>% rename(IC_coverage =coverage, IC_mean_ci_width=mean_ci_width)

perf_tab_over_time_RR <- left_join(perf_tab_over_time_RR, perf_tab_over_time_RR_tmle, by=c("time"))
perf_tab_over_time_diff <- left_join(perf_tab_over_time_diff, perf_tab_over_time_diff_tmle, by=c("time"))

perf_tab_over_time_RR <- perf_tab_over_time_RR %>% select( time,bias,variance,mse, bias_se_ratio, bias_se_ratio_emp,  o.coverage, IC_coverage, TMLE_coverage, IC_mean_ci_width,  TMLE_mean_ci_width)
perf_tab_over_time_diff <- perf_tab_over_time_diff %>% select( time,bias,variance,mse, bias_se_ratio, bias_se_ratio_emp,  o.coverage, IC_coverage, TMLE_coverage, IC_mean_ci_width,  TMLE_mean_ci_width)

save(perf_tab_over_time_diff, perf_tab_over_time_RR,  file=paste0(here::here(),"/results/performance_over_time.Rdata"))
