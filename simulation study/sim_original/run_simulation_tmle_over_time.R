

rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))


library(parallel)
library(doParallel)
registerDoParallel(cores=64)

gc()
d_wide_list <- readRDS(file=here("data/simulated_data_list.RDS"))
d_wide_list <- d_wide_list[1:200]
gc()


#Ntime=2
resdf_tmle_t2  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]], varmethod = "tmle", N_time=2))
  return(res)}


resdf_tmle_t2$analysis="Y1"


#Ntime=3
int.start.time <- Sys.time()
resdf_tmle_t3  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d_wide_list[[i]], N_time=3))
  return(res)
}

resdf_tmle_t3$analysis="Y2"

#Ntime=4
resdf_tmle_t4  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]], varmethod = "tmle",  N_time=4))
  return(res)
}

resdf_tmle_t4$analysis="Y3"



#Ntime=5
resdf_tmle_t5  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]], varmethod = "tmle",  N_time=5))
  return(res)
}

resdf_tmle_t5$analysis="Y4"


#Ntime=6
resdf_tmle_t6  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]], varmethod = "tmle",  N_time=6))
  return(res)
}

resdf_tmle_t6$analysis="Y5"


#Ntime=7
resdf_tmle_t7  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]], varmethod = "tmle",  N_time=7))
  return(res)
}

resdf_tmle_t7$analysis="Y6"


resdf_tmle_t8  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]], varmethod = "tmle",  N_time=8))
  return(res)
}
resdf_tmle_t8$analysis="Y7"


resdf_tmle_t9  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]], varmethod = "tmle",  N_time=9))
  return(res)
}
resdf_tmle_t9$analysis="Y8"



resdf_tmle_t10  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]], varmethod = "tmle",  N_time=10))
  return(res)
}
resdf_tmle_t10$analysis="Y9"



#Ntime=11
resdf_tmle_t11  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]], varmethod = "tmle",  N_time=11))
  return(res)
}

resdf_tmle_t11$analysis="Y10"




#d <- bind_rows(resdf_tmle_t2, resdf_tmle_t3, resdf_tmle_t4, resdf_tmle_t5, resdf_tmle_t6, resdf_tmle_t7, resdf_tmle_t8, resdf_tmle_t11)
d <- bind_rows(resdf_tmle_t2, resdf_tmle_t3, resdf_tmle_t4, resdf_tmle_t5, resdf_tmle_t6, resdf_tmle_t7, resdf_tmle_t8, resdf_tmle_t9, resdf_tmle_t10, resdf_tmle_t11) %>% mutate(analysis=factor(analysis, levels=unique(analysis)))


save(resdf_tmle_t2, resdf_tmle_t3, resdf_tmle_t4, resdf_tmle_t5, resdf_tmle_t6, resdf_tmle_t7, resdf_tmle_t8, resdf_tmle_t9, resdf_tmle_t10, resdf_tmle_t11, file=paste0(here::here(),"/data/sim_res_tmle_t1-11.Rdata"))
saveRDS(d, paste0(here::here(),"/data/sim_res_tmle_t1-11.RDS"))
