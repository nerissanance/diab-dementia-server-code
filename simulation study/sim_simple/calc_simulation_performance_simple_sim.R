


rm(list=ls())
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))
source(paste0(here::here(),"/simulation study/0_simulation_cleaning_functions.R"))


#--------------------------------
# Load simulation results
#--------------------------------

load(paste0(here::here(),"/sim_res/simulation_results_simple.Rdata"))

#get iptw
# colnames(resdf_ic_glm)
# resdf_ic_glm<- resdf_ic_glm %>%
# colnames(resdf_ic_glm) <- gsub("ic_iptw_","iptw.",colnames(resdf_ic_glm))
# %>% rename(ic_ci_lb=CI.2.5., ic_ci_ub=CI.97.5., ate_ic_ci_lb=ate.ci.lb, ate_ic_ci_ub=ate.ci.ub)
# resdf_tmle_glm<- resdf_tmle_glm %>% select(CI.2.5., CI.97.5., ate.ci.lb, ate.ci.ub) %>% rename(tmle_ci_lb=CI.2.5., tmle_ci_ub=CI.97.5., ate_tmle_ci_lb=ate.ci.lb, ate_tmle_ci_ub=ate.ci.ub)
# resdf_glm <- cbind(resdf_ic_glm, resdf_tmle_glm) %>% mutate(estimator="GLM", iteration=1:n())

# resdf_ic_glmnet<- resdf_ic_glmnet %>% select(estimate, std.dev, CI.2.5., CI.97.5., ate, ate.sd, ate.ci.lb, ate.ci.ub) %>% rename(ic_ci_lb=CI.2.5., ic_ci_ub=CI.97.5., ate_ic_ci_lb=ate.ci.lb, ate_ic_ci_ub=ate.ci.ub)
# resdf_tmle_glmnet<- resdf_tmle_glmnet %>% select(CI.2.5., CI.97.5., ate.ci.lb, ate.ci.ub) %>% rename(tmle_ci_lb=CI.2.5., tmle_ci_ub=CI.97.5., ate_tmle_ci_lb=ate.ci.lb, ate_tmle_ci_ub=ate.ci.ub)
# resdf_glmnet <- cbind(resdf_ic_glmnet, resdf_tmle_glmnet) %>% mutate(estimator="LASSO", iteration=1:n())



#merge IC and TMLE
colnames(resdf_ic_glm)
resdf_iptw_glm <- resdf_ic_glm %>% select(starts_with("iptw.")) %>% select(iptw.estimate, iptw.ci.lb, iptw.ci.ub, iptw.ate, iptw.ate.ci.lb, iptw.ate.ci.ub)
resdf_ic_glm<- resdf_ic_glm %>% select(estimate, std.dev, CI.2.5., CI.97.5., ate, ate.sd, ate.ci.lb, ate.ci.ub) %>% rename(ic_ci_lb=CI.2.5., ic_ci_ub=CI.97.5., ate_ic_ci_lb=ate.ci.lb, ate_ic_ci_ub=ate.ci.ub)
resdf_tmle_glm<- resdf_tmle_glm %>% select(CI.2.5., CI.97.5., ate.ci.lb, ate.ci.ub) %>% rename(tmle_ci_lb=CI.2.5., tmle_ci_ub=CI.97.5., ate_tmle_ci_lb=ate.ci.lb, ate_tmle_ci_ub=ate.ci.ub)

resdf_glm <- cbind(resdf_ic_glm, resdf_tmle_glm,resdf_iptw_glm)  %>% mutate(estimator="GLM", iteration=1:n())

resdf_iptw_glmnet <- resdf_ic_glmnet %>% select(starts_with("iptw.")) %>% select(iptw.estimate, iptw.ci.lb, iptw.ci.ub, iptw.ate, iptw.ate.ci.lb, iptw.ate.ci.ub)
resdf_ic_glmnet<- resdf_ic_glmnet %>% select(estimate, std.dev, CI.2.5., CI.97.5., ate, ate.sd, ate.ci.lb, ate.ci.ub) %>% rename(ic_ci_lb=CI.2.5., ic_ci_ub=CI.97.5., ate_ic_ci_lb=ate.ci.lb, ate_ic_ci_ub=ate.ci.ub)
resdf_tmle_glmnet<- resdf_tmle_glmnet %>% select(CI.2.5., CI.97.5., ate.ci.lb, ate.ci.ub) %>% rename(tmle_ci_lb=CI.2.5., tmle_ci_ub=CI.97.5., ate_tmle_ci_lb=ate.ci.lb, ate_tmle_ci_ub=ate.ci.ub)
resdf_glmnet <- cbind(resdf_ic_glmnet, resdf_tmle_glmnet, resdf_iptw_glmnet) %>% mutate(estimator="LASSO", iteration=1:n())


#merge in bootstrap

#load bootstrap
boot_iter_files <- dir(path=paste0(here::here(),"/data/sim_simple/bootstrap/"), pattern = "*.RDS")
boot_iter_files_CV <- boot_iter_files[grepl("sim_res_boot_CV_simple_200iter_",boot_iter_files)]
boot_iter_files_no_ties <- boot_iter_files[grepl("sim_res_boot_without_replacement_simple_200iter_",boot_iter_files)]
boot_iter_files_CV_glm <- boot_iter_files[grepl("sim_res_boot_glm_CV_simple_200iter_",boot_iter_files)]
boot_iter_files_no_ties_glm <- boot_iter_files[grepl("sim_res_boot_glm_without_replacement_simple_200iter_",boot_iter_files)]
boot_iter_files_CV_1000iter <- boot_iter_files[grepl("sim_res_boot_CV_simple_1000iter_",boot_iter_files)]
length(boot_iter_files_CV_1000iter)

setwd(paste0(here::here(),"/data/sim_simple/bootstrap/"))
boot_res_CV <- boot_iter_files_CV %>% map(readRDS) %>% map_dfr(~bind_rows(.) , .id="boot_iter")
boot_res_no_ties <- boot_iter_files_no_ties %>% map(readRDS) %>% map_dfr(~bind_rows(.) , .id="boot_iter")
boot_res_CV_glm <- boot_iter_files_CV %>% map(readRDS) %>% map_dfr(~bind_rows(.) , .id="boot_iter")
boot_res_no_ties_glm <- boot_iter_files_no_ties_glm %>% map(readRDS) %>% map_dfr(~bind_rows(.) , .id="boot_iter")
boot_res_CV_1000iter <- boot_iter_files_CV_1000iter %>% map(readRDS) %>% map_dfr(~bind_rows(.) , .id="boot_iter")

#calc bootstrap CI's
boot_CIs_CV <- boot_res_CV %>% group_by(iteration) %>% summarise(
    boot_cv_CI1=quantile(estimate,.025),
    boot_cv_CI2=quantile(estimate,.975),
    boot_cv_iptw_CI1=quantile(iptw.estimate,.025),
    boot_cv_iptw_CI2=quantile(iptw.estimate,.975),
    ate.boot_cv_CI1=quantile(ate,.025),
    ate.boot_cv_CI2=quantile(ate,.975),
    ate.boot_cv_iptw_CI1=quantile(iptw.ate,.025),
    ate.boot_cv_iptw_CI2=quantile(iptw.ate,.975)
    )
boot_CIs_CV


# boot_res_no_ties <- boot_res_no_ties %>%
#   group_by(iteration) %>%
#   summarise(
#     boot_subsamp_CI1=quantile(estimate,.025),
#     boot_subsamp_CI2=quantile(estimate,.975),
#     ate.boot_subsamp_CI1=quantile(ate,.025),
#     ate.boot_subsamp_CI2=quantile(ate,.975),
#     n=mean(N)) #%>%
#   #correct CI's
#   # mutate(boot_subsamp_CI1=boot_subsamp_CI1*(sqrt((2/3)*n)/sqrt(n)),
#   #        boot_subsamp_CI2=boot_subsamp_CI2*(sqrt((2/3)*n)/sqrt(n)),
#   #        ate.boot_subsamp_CI1=ate.boot_subsamp_CI1*(sqrt((2/3)*n)/sqrt(n)),
#   #        ate.boot_subsamp_CI2=ate.boot_subsamp_CI2*(sqrt((2/3)*n)/sqrt(n))
#   #        )
#
# boot_res_no_ties


boot_CIs_CV_glm <- boot_res_CV_glm %>% group_by(iteration) %>% summarise(
  boot_cv_CI1=quantile(estimate,.025),
  boot_cv_CI2=quantile(estimate,.975),
  boot_cv_iptw_CI1=quantile(iptw.estimate,.025),
  boot_cv_iptw_CI2=quantile(iptw.estimate,.975),
  ate.boot_cv_CI1=quantile(ate,.025),
  ate.boot_cv_CI2=quantile(ate,.975),
  ate.boot_cv_iptw_CI1=quantile(iptw.ate,.025),
  ate.boot_cv_iptw_CI2=quantile(iptw.ate,.975)
)

boot_res_CV_1000iter <- boot_res_CV_1000iter %>% group_by(iteration) %>% summarise(
  boot_cv_1000iter_CI1=quantile(estimate,.025),
  boot_cv_1000iter_CI2=quantile(estimate,.975),
  boot_cv_iptw_1000iter_CI1=quantile(iptw.estimate,.025),
  boot_cv_iptw_1000iter_CI2=quantile(iptw.estimate,.975),
  ate.boot_cv_1000iter_CI1=quantile(ate,.025),
  ate.boot_cv_1000iter_CI2=quantile(ate,.975),
  ate.boot_cv_iptw_1000iter_CI1=quantile(iptw.ate,.025),
  ate.boot_cv_iptw_1000iter_CI2=quantile(iptw.ate,.975)
)



# boot_res_no_ties_glm <- boot_res_no_ties_glm %>% group_by(iteration) %>% summarise(
#   boot_subsamp_CI1=quantile(estimate,.025),
#   boot_subsamp_CI2=quantile(estimate,.975),
#   ate.boot_subsamp_CI1=quantile(ate,.025),
#   ate.boot_subsamp_CI2=quantile(ate,.975),
#   n=mean(N))


# #NOTE: need to correct with Mark's methods
# mean(boot_CIs_CV$ate.boot_cv_CI2-boot_CIs_CV$ate.boot_cv_CI1)
# mean(boot_res_no_ties$ate.boot_subsamp_CI2-boot_res_no_ties$ate.boot_subsamp_CI1)
# #Why are subsampled narrower?

resdf_glm <- left_join(resdf_glm, boot_CIs_CV_glm, by="iteration")
#resdf_glm <- left_join(resdf_glm, boot_res_no_ties_glm, by="iteration")

resdf_glmnet <- left_join(resdf_glmnet, boot_CIs_CV, by="iteration")
resdf_glmnet <- left_join(resdf_glmnet, boot_res_CV_1000iter, by="iteration")


#resdf_glmnet <- left_join(resdf_glmnet, boot_res_no_ties, by="iteration")

d <- bind_rows(resdf_glm, resdf_glmnet)
tail(d)

#--------------------------------
# Set truth
#--------------------------------

load(paste0(here::here(),"/sim_res/simple_sim_truth.Rdata"))


d$true.RR <- simple_sim_trueRR
d$true.RD <- simple_sim_trueRD



#--------------------------------
# Calc performance
#--------------------------------


colnames(d)
perf_tab_RR <- d %>% group_by(estimator) %>%
  mutate(variance=mean((log(estimate)-mean(log(estimate)))^2),
         o.ci.lb = log(estimate) - 1.96 * sqrt(variance),
         o.ci.ub = log(estimate) + 1.96 * sqrt(variance),
         iptw_variance=mean((log(iptw.estimate)-mean(log(iptw.estimate)))^2),
         iptw_o.ci.lb = log(iptw.estimate) - 1.96 * sqrt(iptw_variance),
         iptw_o.ci.ub = log(iptw.estimate) + 1.96 * sqrt(iptw_variance)
         ) %>%
  summarize(
    bias=mean(log(estimate))-log(true.RR),
    variance=variance[1],
    mse = bias^2 + variance,
    bias_se_ratio= bias/sqrt(variance),
    o.coverage=mean(o.ci.lb<log(true.RR) & log(true.RR)< o.ci.ub)*100,
    coverage_ic=mean(ic_ci_lb<true.RR & true.RR<ic_ci_ub)*100,
    coverage_tmle=mean(tmle_ci_lb<true.RR & true.RR<tmle_ci_ub)*100,
    coverage_cv_boot=mean(boot_cv_CI1<true.RR & true.RR<boot_cv_CI2)*100,
    coverage_cv_boot_1000iter=mean(boot_cv_1000iter_CI1<true.RR & true.RR<boot_cv_1000iter_CI2)*100,
    bias=mean(log(estimate))-log(true.RR),
    variance=variance[1],
    mse = bias^2 + variance,
    bias_se_ratio= bias/sqrt(variance),
    o.coverage=mean(o.ci.lb<log(true.RR) & log(true.RR)< o.ci.ub)*100,
    bias=mean(log(estimate))-log(true.RR),
    coverage_iptw=mean(iptw.ci.lb<true.RR & true.RR<iptw.ci.ub)*100,
    coverage_iptw_boot=mean(boot_cv_iptw_CI1 <true.RR & true.RR<boot_cv_iptw_CI2)*100#,
    # mean_ci_width=mean(log(CI.97.5.)-log(CI.2.5.)),
    # power=mean((CI.2.5. > 1 & CI.97.5.>1)|(CI.2.5. < 1 & CI.97.5.<1))*100
  ) %>%
  distinct()
perf_tab_RR

colnames(d)
perf_tab_RD <- d %>% group_by(estimator) %>%
  mutate(variance=mean(((ate)-mean((ate)))^2),
         o.ci.lb = (ate) - 1.96 * sqrt(variance),
         o.ci.ub = (ate) + 1.96 * sqrt(variance)) %>%
  summarize(
    bias=mean((ate))-(true.RD),
    variance=variance[1],
    mse = bias^2 + variance,
    bias_se_ratio= bias/sqrt(variance),
    o.coverage=mean(o.ci.lb<(true.RD) & (true.RD)< o.ci.ub)*100,
    coverage_ic=mean(ate_ic_ci_lb<true.RD & true.RD<ate_ic_ci_ub)*100,
    coverage_tmle=mean(ate_tmle_ci_lb<true.RD & true.RD<ate_tmle_ci_ub)*100,
    coverage_cv_boot=mean(ate.boot_cv_CI1<true.RD & true.RD<ate.boot_cv_CI2)*100,
    coverage_cv_boot_1000iter=mean(ate.boot_cv_iptw_1000iter_CI1 <true.RD & true.RD<ate.boot_cv_iptw_1000iter_CI2)*100,
    coverage_iptw=mean(iptw.ate.ci.lb<true.RD & true.RD<iptw.ate.ci.ub)*100,
    coverage_iptw_boot_1000iter=mean(ate.boot_cv_iptw_1000iter_CI1 <true.RD & true.RD<ate.boot_cv_iptw_1000iter_CI2)*100#,
    #coverage_subsamp_boot=mean(ate.boot_subsamp_CI1 <true.RD & true.RD<ate.boot_subsamp_CI2)*100#,
    # mean_ci_width=mean((CI.97.5.)-(CI.2.5.)),
    # power=mean((CI.2.5. > 1 & CI.97.5.>1)|(CI.2.5. < 1 & CI.97.5.<1))*100
  ) %>%
  distinct()
perf_tab_RD


perf_tab_RD
perf_tab_RR

save(perf_tab_RR, perf_tab_RD,  file=paste0(here::here(),"/results/sim_performance_results_simple.Rdata"))

knitr::kable(perf_tab_RD, digits = 4)
knitr::kable(perf_tab_RR, digits = 3)
