


rm(list=ls())
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))

#--------------------------------
# Load simulation results
#--------------------------------


files <- dir(path=paste0(here::here(),"/sim_res/"), pattern = "*.RDS")
files <- files[grepl("outcome_blind",files)]
files <- files[grepl("_T4",files)]

setwd(paste0(here::here(),"/sim_res/"))
d <- files %>% map(readRDS) %>% map_dfr(~bind_rows(.) , .id="analysis")
d <- d %>% mutate(analysis = factor(analysis))
levels(d$analysis) = files[as.numeric(levels(d$analysis))]
d$analysis <- gsub(".RDS","",d$analysis)

#load bootstrap
boot_iter_files <- dir(path=paste0(here::here(),"/data/bootstrap/"), pattern = "*.RDS")
boot_iter_files <- boot_iter_files[grepl("blind_cens_competing_risks_T4",boot_iter_files)]
setwd(paste0(here::here(),"/data/bootstrap/"))
boot_res <- boot_iter_files %>% map(readRDS) %>% map_dfr(~bind_rows(.) , .id="boot_iter")

#load bootstrap - no DetQ
boot_iter_files <- dir(path=paste0(here::here(),"/data/bootstrap/"), pattern = "*.RDS")
boot_iter_files <- boot_iter_files[grepl("_noDetQ_T4",boot_iter_files)]
setwd(paste0(here::here(),"/data/bootstrap/"))
boot_res_noDetQ <- boot_iter_files %>% map(readRDS) %>% map_dfr(~bind_rows(.) , .id="boot_iter")


#calc bootstrap CI's
boot_CIs <- boot_res %>% group_by(iteration) %>%
  summarise(
    CI1=quantile(estimate,.025),
    CI2=quantile(estimate,.975),
    ate.CI1=quantile(ate,.025),
    ate.CI2=quantile(ate,.975)
)
boot_CIs

hist(boot_res$ate[boot_res$iteration==4])
hist(log(boot_res$estimate[boot_res$iteration==4]))

boot_CIs_noDetQ <- boot_res_noDetQ %>% group_by(iteration) %>%
  summarise(
    CI1=quantile(estimate,.025),
    CI2=quantile(estimate,.975),
    ate.CI1=quantile(ate,.025),
    ate.CI2=quantile(ate,.975)
  )
boot_CIs_noDetQ



#--------------------------------
# Set truth
#--------------------------------

load(paste0(here::here(),"/results/truth_blind_T4.Rdata"))

d$true.RR <- cRR3
d$true.RD <- cRD3



# #Truth - manually calculated from SEM
#
# #temp
# d$true.RD <- (-0.075)

#--------------------------------
# Calc performance
#--------------------------------

d_T4_outcome_blind <- d


perf_tab_RR <- d %>% group_by(analysis) %>%
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


perf_tab_diff <- d %>% filter(!is.na(ate)) %>%
  subset(., select = -c(estimate, std.dev, CI.2.5., CI.97.5.)) %>%
  rename(estimate= ate, std.dev= ate.sd, CI.2.5.=ate.ci.lb, CI.97.5.=ate.ci.ub)%>%
  group_by(analysis) %>%
  mutate(variance=mean((estimate-mean(estimate))^2),
         o.ci.lb = estimate - 1.96 * sqrt(variance),
         o.ci.ub = estimate + 1.96 * sqrt(variance)) %>%
  summarize(
    bias=mean((estimate))-(true.RD),
    variance=mean((estimate-mean(estimate))^2),
    mse = bias^2 + variance,
    bias_se_ratio= bias/sqrt(variance),
    coverage=mean(CI.2.5.<=true.RD & true.RD<=CI.97.5.)*100,
    #oracle coverage
    o.coverage=mean(o.ci.lb<=true.RD & true.RD<= o.ci.ub)*100,
    mean_ci_width=mean((CI.97.5.)-(CI.2.5.)),
    power=mean((CI.2.5. > 0 & CI.97.5.>0)|(CI.2.5. < 0 & CI.97.5.<0))*100,
    o.power=mean((o.ci.lb > 0 & o.ci.ub>0)|(o.ci.lb < 0 & o.ci.ub<0))*100
  ) %>%
  distinct()

#calculate bootstrap performance
perf_tab_RR_boot <- data.frame(
  analysis = "Bootstrap - clustered ID",
  coverage = mean(boot_CIs$CI1 <= d$true.RR[1] &  boot_CIs$CI2 >= d$true.RR[1])*100,
  mean_ci_width_logRR=mean(log(boot_CIs$CI2)-log(boot_CIs$CI1)),
  power=mean((boot_CIs$CI1 > 0 & boot_CIs$CI2>0)|(boot_CIs$CI1 < 0 & boot_CIs$CI2<0))*100
  )

perf_tab_diff_boot <- data.frame(
  analysis = "Bootstrap - clustered ID",
  coverage = mean(boot_CIs$ate.CI1 <= d$true.RR[1] &  boot_CIs$ate.CI2 >= d$true.RR[1])*100,
  mean_ci_width=mean((boot_CIs$ate.CI2)-(boot_CIs$ate.CI1)),
  power=mean((boot_CIs$ate.CI1 > 0 & boot_CIs$ate.CI2>0)|(boot_CIs$ate.CI1 < 0 & boot_CIs$ate.CI2<0))*100
)

perf_tab_RR_boot_noDetQ <- data.frame(
  analysis = "Bootstrap - clustered ID, no DetQ",
  coverage = mean(boot_CIs_noDetQ$CI1 <= d$true.RR[1] &  boot_CIs_noDetQ$CI2 >= d$true.RR[1])*100,
  mean_ci_width_logRR=mean(log(boot_CIs_noDetQ$CI2)-log(boot_CIs_noDetQ$CI1)),
  power=mean((boot_CIs_noDetQ$CI1 > 0 & boot_CIs_noDetQ$CI2>0)|(boot_CIs_noDetQ$CI1 < 0 & boot_CIs_noDetQ$CI2<0))*100
)

perf_tab_diff_boot_noDetQ <- data.frame(
  analysis = "Bootstrap - clustered ID, no DetQ",
  coverage = mean(boot_CIs_noDetQ$ate.CI1 <= d$true.RR[1] &  boot_CIs_noDetQ$ate.CI2 >= d$true.RR[1])*100,
  mean_ci_width=mean((boot_CIs_noDetQ$ate.CI2)-(boot_CIs_noDetQ$ate.CI1)),
  power=mean((boot_CIs_noDetQ$ate.CI1 > 0 & boot_CIs_noDetQ$ate.CI2>0)|(boot_CIs_noDetQ$ate.CI1 < 0 & boot_CIs_noDetQ$ate.CI2<0))*100
)

perf_tab_RR_outcome_blind_T4 <- bind_rows(perf_tab_RR, perf_tab_RR_boot, perf_tab_RR_boot_noDetQ)
perf_tab_diff_outcome_blind_T4 <- bind_rows(perf_tab_diff, perf_tab_diff_boot, perf_tab_diff_boot_noDetQ)



#--------------------------------
# Save data
#--------------------------------
save(d_T4_outcome_blind, perf_tab_RR_outcome_blind_T4, perf_tab_diff_outcome_blind_T4, file = paste0(here::here(),"/simulation study/sim_performance_results/T4_outcome_blind_sim_res.Rdata"))

res <- perf_tab_RR %>% arrange(coverage)
knitr::kable(res, digits =3)

res_diff <- perf_tab_diff %>% arrange(coverage)
knitr::kable(res_diff, digits =5)


#--------------------------------
# Make data for CI plot
#--------------------------------

unique(d$analysis)
d2 <- d %>% filter(analysis %in% c("outcome_blind_cens_competing_risks_sim_res_noDetQ_tmle_T4","outcome_blind_cens_competing_risks_sim_res_noDetQ_ic_T4")) %>%
  rename(CI1=CI.2.5., CI2=CI.97.5., ate.CI1=ate.ci.lb,  ate.CI2=ate.ci.ub) %>%
  select(analysis, CI1,CI2, ate.CI1,  ate.CI2, true.RR) %>%
  group_by(analysis) %>% mutate(iteration =row_number())

boot_CIs <- boot_CIs %>% mutate(analysis="bootstrap - clustered ID")

d2 <- bind_rows(d2, boot_CIs) %>%
  arrange(iteration) %>%
  group_by(analysis) %>% slice(1:20)

d2 <- d2 %>% mutate(analysis=gsub("outcome_blind_cens_competing_risks_sim_res_noDetQ_","",analysis))

ggplot(d2, aes(x=iteration, group=analysis, color=analysis)) +
  geom_linerange(aes(ymin=CI1, ymax=CI2), position = position_dodge(0.5)) +
  geom_hline(aes(yintercept = true.RR), linetype="dashed") +
  geom_hline(yintercept = 1) +
  scale_y_continuous(trans = "log10") +
  coord_flip()




#--------------------------------
# Plots
#--------------------------------

# RR plots
set.seed(123)
ggplot(d, aes(y=analysis, x=estimate)) +
  geom_jitter(width=0, height=0.05, alpha=0.75) +
  coord_cartesian(xlim=c(0.1, 2)) +
  geom_vline(xintercept = 1) +
  geom_vline(aes(xintercept = true.RR), linetype="dashed") +
  scale_x_continuous(trans = "log10")


ggplot(d, aes(x=estimate)) +
  facet_wrap(~analysis) +
  geom_density() +
  geom_vline(xintercept = 1) +
  geom_vline(aes(xintercept = true.RR), linetype="dashed") +
  scale_x_continuous(trans = "log10") + theme_bw()


#ATE plots
set.seed(123)
ggplot(d, aes(y=analysis, x=ate)) +
  geom_jitter(width=0, height=0.05, alpha=0.75) +
  geom_vline(xintercept = 0) +
  geom_vline(aes(xintercept = true.RD), linetype="dashed")

ggplot(d, aes(x=ate)) +
  facet_wrap(~analysis) +
  geom_density() +
  geom_vline(xintercept = 0) +
  geom_vline(aes(xintercept = true.RD), linetype="dashed") + theme_bw()
