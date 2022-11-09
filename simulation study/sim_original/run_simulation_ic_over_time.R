
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
resdf_ic_t2  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]], N_time=2))
  return(res)}

resdf_ic_t2

resdf_ic_t2$analysis="Y1"
resdf_ic_t2$true.RR <- 1.122672
resdf_ic_t2$true.RD <- (0.000191)


#Ntime=3
int.start.time <- Sys.time()
resdf_ic_t3  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d_wide_list[[i]]))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

resdf_ic_t3$analysis="Y2"
resdf_ic_t3$true.RR <- 0.7286682
resdf_ic_t3$true.RD <- (-0.001202)

#Ntime=4
resdf_ic_t4  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]],  N_time=4))
  return(res)
}

resdf_ic_t4$analysis="Y3"
resdf_ic_t4$true.RR <- 0.5829431
resdf_ic_t4$true.RD <- (-0.002939)


#Ntime=5
resdf_ic_t5  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]],  N_time=5))
  return(res)
}

resdf_ic_t5$analysis="Y4"
resdf_ic_t5$true.RR <- 0.493746
resdf_ic_t5$true.RD <- (-0.004776)

#Ntime=6
resdf_ic_t6  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]],  N_time=6))
  return(res)
}

resdf_ic_t6$analysis="Y5"
resdf_ic_t6$true.RR <- 0.6061559
resdf_ic_t6$true.RD <- (-0.004632)

#Ntime=7
resdf_ic_t7  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]],  N_time=7))
  return(res)
}

resdf_ic_t7$analysis="Y6"
resdf_ic_t7$true.RR <- 0.5570813
resdf_ic_t7$true.RD <- (-0.006064)

resdf_ic_t8  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]],  N_time=8))
  return(res)
}
resdf_ic_t8$analysis="Y7"
resdf_ic_t8$true.RR <- 0.642349
resdf_ic_t8$true.RD <- (-0.00553)

resdf_ic_t9  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]],  N_time=9))
  return(res)
}
resdf_ic_t9$analysis="Y8"
resdf_ic_t9$true.RR <- 0.7184137
resdf_ic_t9$true.RD <- (-0.004736)


resdf_ic_t10  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]],  N_time=10))
  return(res)
}
resdf_ic_t10$analysis="Y9"
resdf_ic_t10$true.RR <- 0.6925836
resdf_ic_t10$true.RD <- (-0.005571)


#Ntime=11
resdf_ic_t11  <- foreach(i = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_test(d=d_wide_list[[i]],  N_time=11))
  return(res)
}

resdf_ic_t11$analysis="Y10"
resdf_ic_t11$true.RR <-  0.6924793
resdf_ic_t11$true.RD <- (-0.005929)



#d <- bind_rows(resdf_ic_t2, resdf_ic_t3, resdf_ic_t4, resdf_ic_t5, resdf_ic_t6, resdf_ic_t7, resdf_ic_t8, resdf_ic_t11)
d <- bind_rows(resdf_ic_t2, resdf_ic_t3, resdf_ic_t4, resdf_ic_t5, resdf_ic_t6, resdf_ic_t7, resdf_ic_t8, resdf_ic_t9, resdf_ic_t10, resdf_ic_t11) %>% mutate(analysis=factor(analysis, levels=unique(analysis)))


save(resdf_ic_t2, resdf_ic_t3, resdf_ic_t4, resdf_ic_t5, resdf_ic_t6, resdf_ic_t7, resdf_ic_t8, resdf_ic_t9, resdf_ic_t10, resdf_ic_t11, file=paste0(here::here(),"/data/sim_res_ic_t1-11.Rdata"))
saveRDS(resdf_ic_t11, paste0(here::here(),"/data/sim_res_noDetQ_ic_v3.RDS"))
saveRDS(d, paste0(here::here(),"/data/sim_res_ic_t1-11.RDS"))
