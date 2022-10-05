


rm(list=ls())
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))

#--------------------------------
# Load simulation results
#--------------------------------



#Unadjusted
sim_df <- bind_rows(
  resdf_boot_T2 <- readRDS(paste0(here::here(),"/data/sim_res_boot_null_T2.RDS")) %>% mutate(analysis="A")
)

#--------------------------------
# Set truth
#--------------------------------



sim_df$true.RR <- 1
sim_df$true.RD <- 0

#--------------------------------
# Calc performance
#--------------------------------


perf_tab <- sim_df %>% group_by(analysis) %>%
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
    power=mean((CI.2.5. > 1 & CI.97.5.>1)|(CI.2.5. < 1 & CI.97.5.<1))*100
  ) %>%
  distinct()

res <- perf_tab %>% arrange(mse)

kable(res, digits =3)

#--------------------------------
# Save data
#--------------------------------
saveRDS(sim_df, file=paste0(here::here(),"/results/compiled_simulation_results_null.rds"))
