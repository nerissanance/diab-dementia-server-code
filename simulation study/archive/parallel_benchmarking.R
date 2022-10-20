
rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))

gc()
d_wide_list <- readRDS(file=here("data/simulated_data_list_null_no_cens.RDS"))
d_wide_list <- d_wide_list[1:64]
gc()



#test parallelization
library(parallel)
library(doParallel)
registerDoParallel(cores=64)

int.start.time <- Sys.time()
res1 <- mclapply(1:length(d_wide_list), function(i){
  try(run_ltmle_glmnet_no_cens(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=FALSE, varmethod = "ic",N_time=11))
}, mc.cores = 64)
res1
int.end.time <- Sys.time()
time1 <- difftime(int.end.time, int.start.time, units="mins")
time1

int.start.time <- Sys.time()
res2 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet_no_cens(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=FALSE, varmethod = "ic",N_time=11))
  return(res)
}
int.end.time <- Sys.time()
time2 <- difftime(int.end.time, int.start.time, units="mins")
time2



# int.start.time <- Sys.time()
# res3 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %do% {
#   res <- NULL
#   try(res <- run_ltmle_glmnet_no_cens(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=FALSE, varmethod = "ic",N_time=2))
#   return(res)
# }
# int.end.time <- Sys.time()
# time3 <- difftime(int.end.time, int.start.time, units="mins")
# time3


