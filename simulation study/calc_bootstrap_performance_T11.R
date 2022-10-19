


rm(list=ls())
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))

#--------------------------------
# Load simulation results
#--------------------------------



#load bootstrap
boot_iter_files <- dir(path=paste0(here::here(),"/data/bootstrap/"), pattern = "*.RDS")
boot_iter_files <- boot_iter_files[grepl("_old_sim_cens_competing_risks_T11",boot_iter_files)]
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

hist(boot_res$ate[boot_res$boot_iter==4])
hist(log(boot_res$estimate[boot_res$boot_iter==4]))



plotdf <- readRDS(file=paste0(here::here(),"/results/compiled_simulatation_results.rds"))


#--------------------------------
# Calc bias, variance, and MSE
#--------------------------------

#relative scale methods:
#https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/s12874-018-0519-5


plotdf <- plotdf[!grepl("common outcome",plotdf$analysis),]

#temp drop extreme outliers
plotdf <- plotdf%>% filter(estimate > 1/128)

perf_tab <- plotdf %>% group_by(analysis) %>%
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

res <- perf_tab %>% arrange(mse)

kable(res, digits =3)



plotdf
#--------------------------------
# Calc performance
#--------------------------------




#calculate bootstrap performance
perf_tab_RR_boot <- data.frame(
  analysis = "Bootstrap - clustered ID",
  coverage = mean(boot_CIs$CI1 <= plotdf$true.RR[1] &  boot_CIs$CI2 >= plotdf$true.RR[1])*100,
  mean_ci_width_logRR=mean(log(boot_CIs$CI2)-log(boot_CIs$CI1)),
  power=mean((boot_CIs$CI1 > 0 & boot_CIs$CI2>0)|(boot_CIs$CI1 < 0 & boot_CIs$CI2<0))*100
  )

perf_tab_diff_boot <- data.frame(
  analysis = "Bootstrap - clustered ID",
  coverage = mean(boot_CIs$ate.CI1 <= plotdf$true.RR[1] &  boot_CIs$ate.CI2 >= plotdf$true.RR[1])*100,
  mean_ci_width=mean((boot_CIs$ate.CI2)-(boot_CIs$ate.CI1)),
  power=mean((boot_CIs$ate.CI1 > 0 & boot_CIs$ate.CI2>0)|(boot_CIs$ate.CI1 < 0 & boot_CIs$ate.CI2<0))*100
)

# perf_tab_RR <- bind_rows(perf_tab_RR, perf_tab_RR_boot)
# perf_tab_diff <- bind_rows(perf_tab_diff, perf_tab_diff_boot)

perf_tab_RR_boot
perf_tab_diff_boot
