


rm(list=ls())
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))
source(paste0(here::here(),"/simulation study/0_simulation_cleaning_functions.R"))


#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# update from here and add bootstrap
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

#--------------------------------
# Load simulation results
#--------------------------------


files <- dir(path=paste0(here::here(),"/sim_res/"), pattern = "*.RDS")
files <- files[grepl("outcome_blind_",files)]
files <- files[grepl("_T11",files)]

setwd(paste0(here::here(),"/sim_res/"))
d <- files %>% map(readRDS) %>% map_dfr(~bind_rows(.) , .id="analysis")
d <- d %>% mutate(analysis = factor(analysis))
levels(d$analysis) = files[as.numeric(levels(d$analysis))]
d$analysis <- gsub(".RDS","",d$analysis)

#load bootstrap
boot_iter_files <- dir(path=paste0(here::here(),"/data/bootstrap/"), pattern = "*.RDS")
boot_iter_files <- boot_iter_files[grepl("_T11",boot_iter_files)]
boot_iter_files <- boot_iter_files[!grepl("_null",boot_iter_files)]
boot_iter_files <- boot_iter_files[!grepl("_subsampled",boot_iter_files)]
boot_iter_files <- boot_iter_files[!grepl("_outcome_blind",boot_iter_files)]
length(boot_iter_files)

setwd(paste0(here::here(),"/data/bootstrap/"))
boot_res <- boot_iter_files %>% map(readRDS) %>% map_dfr(~bind_rows(.) , .id="boot_iter")


#XXXXXX
load(paste0(here::here(),"/results/truth_blind_T10.Rdata"))
sim_res <- calc_sim_performance(files, boot_iter_files, cRR, cRD)
d <- calc_sim_performance(files, boot_iter_files, cRR, cRD)

colnames(d$perf_tab_RR)



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


#--------------------------------
# Set truth
#--------------------------------


d$true.RR <- cRR
d$true.RD <- cRD

#Truth - manually calculated from SEM
d$true.RR <- 0.4493685
#d$true.RD <- (-0.000149903)

#temp
#d$true.RD <- (-0.075)

#--------------------------------
# Calc performance
#--------------------------------

d_T10 <- d


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
  mean_ci_width=mean(log(boot_CIs$CI2)-log(boot_CIs$CI1)),
  power=mean((boot_CIs$CI1 > 1 & boot_CIs$CI2>1)|(boot_CIs$CI1 < 1 & boot_CIs$CI2<1))*100
)

perf_tab_diff_boot <- data.frame(
  analysis = "Bootstrap - clustered ID",
  coverage = mean(boot_CIs$ate.CI1 <= d$true.RD[1] &  boot_CIs$ate.CI2 >= d$true.RD[1])*100,
  mean_ci_width=mean((boot_CIs$ate.CI2)-(boot_CIs$ate.CI1)),
  power=mean((boot_CIs$ate.CI1 > 0 & boot_CIs$ate.CI2>0)|(boot_CIs$ate.CI1 < 0 & boot_CIs$ate.CI2<0))*100
)
perf_tab_RR_outcome_blind_T11 <- bind_rows(perf_tab_RR, perf_tab_RR_boot)
perf_tab_diff_outcome_blind_T11 <- bind_rows(perf_tab_diff, perf_tab_diff_boot)


#--------------------------------
# Save data
#--------------------------------
save(d_T10, perf_tab_RR_outcome_blind_T11, perf_tab_diff_outcome_blind_T11, file = paste0(here::here(),"/simulation study/sim_performance_results/T10_sim_res.Rdata"))

res <- perf_tab_RR %>% arrange(mse)
knitr::kable(res, digits =3)

res_diff <- perf_tab_diff %>% arrange(mse)
knitr::kable(res_diff, digits =5)


#--------------------------------
# Make data for CI plot
#--------------------------------

unique(d$analysis)




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
