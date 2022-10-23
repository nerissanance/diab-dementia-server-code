
rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))

source(paste0(here::here(),"/simulation study/sim_simple/0_simple_sim_functions.R"))

library(parallel)
library(doParallel)
registerDoParallel(cores=50)

d_wide_list <- readRDS(file=here("data/simulated_data_list_simple.RDS"))



#run IC and TMLE
resdf_ic_glm <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  res <- run_ltmle_simple(d_wide_list[[i]],  varmethod = "ic", SL.library = c("glm"))
  return(res)}
head(resdf_ic_glm)
summary(resdf_ic_glm$estimate)

resdf_tmle_glm <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  res <- run_ltmle_simple(d_wide_list[[i]],  varmethod = "tmle", SL.library = c("glm"))
  return(res)}

#use robust if necessary
i<-1
resdf_ic_glmnet <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  res <- run_ltmle_simple(d_wide_list[[i]],  varmethod = "ic", SL.library = c("SL.glmnet"))
  return(res)}
head(resdf_ic_glm)
summary(resdf_ic_glm$estimate)

resdf_tmle_glmnet <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  res <- run_ltmle_simple(d_wide_list[[i]],  varmethod = "tmle", SL.library = c("SL.glmnet"))
  return(res)}


save(resdf_ic_glm, resdf_tmle_glm, resdf_ic_glmnet, resdf_tmle_glmnet, file=here("sim_res/simulated_data_list_simple.Rdata"))

