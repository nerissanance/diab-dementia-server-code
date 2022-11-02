
rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))



gc()
d_wide_list <- readRDS(file=here("data/simulated_data_list.RDS"))
gc()

d_wide_list <- d_wide_list[1]

#Method 1
start.time <- Sys.time()
resdf = NULL
for(i in 1:length(d_wide_list)){
  resdf <- run_ltmle(d_wide_list[[i]], resdf=resdf)
}
end.time <- Sys.time()

print("runtime: ")
print(difftime(end.time, start.time, units="mins"))
#Time difference of 28.8765 mins

head(resdf)
saveRDS(resdf, here::here("/data/simulation_results/sim_res1.RDS"))

ggplot(resdf, aes(x=estimate)) + geom_density() + scale_x_continuous(trans="log10") + geom_vline(aes(xintercept=1))
saveRDS(resdf, here::here("/data/sim_res1.RDS"))


#To run:
# - IC variance

start.time <- Sys.time()
resdf_ic = NULL
for(i in 1:length(d_wide_list)){
  resdf_ic <- run_ltmle(d_wide_list[[i]], varmethod = "ic", resdf=resdf_ic)
}
end.time <- Sys.time()

print("runtime: ")
print(difftime(end.time, start.time, units="mins"))
#Time difference of 28.92235 mins

ggplot(resdf_ic, aes(x=estimate)) + geom_density() + scale_x_continuous(trans="log10") + geom_vline(aes(xintercept=1))

# ggplot(resdf, aes(x=std.dev)) + geom_density() +  geom_vline(aes(xintercept=0))
# ggplot(resdf_ic, aes(x=std.dev)) + geom_density() +  geom_vline(aes(xintercept=0))
#
#
# ggplot(resdf, aes(x=pvalue)) + geom_density() +  geom_vline(aes(xintercept=0.05))
# ggplot(resdf_ic, aes(x=pvalue)) + geom_density() +  geom_vline(aes(xintercept=0.05))


#Why are TMLE and IC options identical?


# - GLM

start.time <- Sys.time()
resdf_glm = NULL
for(i in 1:length(d_wide_list)){
  resdf_glm <- run_ltmle(d_wide_list[[i]], varmethod = "ic", resdf=resdf_glm, SL.library="glm")
}
end.time <- Sys.time()

print("runtime: ")
print(difftime(end.time, start.time, units="mins"))
#Time difference of 4.559691 mins

ggplot(resdf_glm, aes(x=estimate)) + geom_density() + scale_x_continuous(trans="log10") + geom_vline(aes(xintercept=1))



# -Q-intercept only
start.time <- Sys.time()
resdf_Qint = NULL
for(i in 1:length(d_wide_list)){
  resdf_Qint <- run_ltmle(d_wide_list[[i]], varmethod = "ic", resdf=resdf_Qint, Qint=TRUE)
}
end.time <- Sys.time()

print("runtime: ")
print(difftime(end.time, start.time, units="mins"))
#12.18644 mins

ggplot(resdf_Qint, aes(x=estimate)) + geom_density() + scale_x_continuous(trans="log10") + geom_vline(aes(xintercept=1))


# -Q-intercept only -TMLE variance
# - Elastic Net
# - SuperLearner with robustly coded glmnet (sl.mean, sl.glm, lasso, elastic net)
# - gcomp=T estimation
# - Using AUC


#-try target causal parameter three?


#wishlist
#-initial outcome model AUC
#-covariates selected
#Final timepoint: AUC of the untargeted estimation


#gcomp=T
#use glm
#predict timepoint
