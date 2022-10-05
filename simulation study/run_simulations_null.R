
rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))




gc()
d_wide_list <- readRDS(file=here("data/null_simulated_data_list.RDS"))
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
d_wide_list <- d_wide_list[1:200]
gc()

#test
# try(res <- run_ltmle_glmnet(d_wide_list[[1]], resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "ic",N_time=2))
# summary(res)


#Ntime
Ntime=11
=======
=======
>>>>>>> 48c3eea89b036aef116454c69313a8033352a63c
=======
>>>>>>> 48c3eea89b036aef116454c69313a8033352a63c
=======
>>>>>>> 48c3eea89b036aef116454c69313a8033352a63c
d_wide_list <- d_wide_list[1:5]
gc()



<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
>>>>>>> 48c3eea89b036aef116454c69313a8033352a63c
=======
>>>>>>> 48c3eea89b036aef116454c69313a8033352a63c
=======
>>>>>>> 48c3eea89b036aef116454c69313a8033352a63c
=======
>>>>>>> 48c3eea89b036aef116454c69313a8033352a63c

#primary
int.start.time <- Sys.time()
resdf_noDetQ_Qint_tmle <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=FALSE, varmethod = "tmle",N_time=11))
=======
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=FALSE, varmethod = "tmle"))
>>>>>>> 48c3eea89b036aef116454c69313a8033352a63c
=======
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=FALSE, varmethod = "tmle"))
>>>>>>> 48c3eea89b036aef116454c69313a8033352a63c
=======
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=FALSE, varmethod = "tmle"))
>>>>>>> 48c3eea89b036aef116454c69313a8033352a63c
=======
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=FALSE, varmethod = "tmle"))
>>>>>>> 48c3eea89b036aef116454c69313a8033352a63c
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD
<<<<<<< HEAD

saveRDS(resdf_noDetQ_Qint_tmle, paste0(here::here(),"/sim_res/null_sim_res_noDetQ_Qint_tmle_T11.RDS"))

#Ntime
Ntime=11

#primary
int.start.time <- Sys.time()
resdf_noDetQ_Qint_tmle <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "tmle",N_time=11))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_noDetQ_Qint_tmle, paste0(here::here(),"/sim_res/null_sim_res_noDetQ_Qint_tmle_T11.RDS"))

#primary
int.start.time <- Sys.time()
resdf_noDetQ_Qint_ic <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=FALSE, varmethod = "ic",N_time=11))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_noDetQ_Qint_ic, paste0(here::here(),"/sim_res/null_sim_res_noDetQ_Qint_ic_T11.RDS"))


#primary
int.start.time <- Sys.time()
resdf_noDetQ_Qint_ic <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "ic",N_time=11))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_noDetQ_Qint_ic, paste0(here::here(),"/sim_res/null_sim_res_noDetQ_Qint_ic_T11.RDS"))



#Ntime
Ntime=2

#primary
int.start.time <- Sys.time()
resdf_noDetQ_Qint_tmle <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=FALSE, varmethod = "tmle",N_time=2))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_noDetQ_Qint_tmle, paste0(here::here(),"/sim_res/null_sim_res_noDetQ_Qint_tmle_T2.RDS"))


int.start.time <- Sys.time()
resdf_noDetQ_Qint_tmle <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "tmle",N_time=2))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_noDetQ_Qint_tmle, paste0(here::here(),"/sim_res/null_sim_res_noDetQ_tmle_T2.RDS"))



resdf_noDetQ_Qint_ic <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=TRUE, det.Q=FALSE, varmethod = "ic",N_time=2))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_noDetQ_Qint_ic, paste0(here::here(),"/sim_res/null_sim_res_noDetQ_Qint_ic_T2.RDS"))


int.start.time <- Sys.time()
resdf_noDetQ_Qint_ic <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "ic",N_time=2))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")

saveRDS(resdf_noDetQ_Qint_ic, paste0(here::here(),"/sim_res/null_sim_res_noDetQ_ic_T2.RDS"))
=======
resdf_noDetQ_Qint_tmle

#saveRDS(resdf_noDetQ_Qint_tmle, paste0(here::here(),"/data/null_sim_res_noDetQ_Qint_tmle.RDS"))
>>>>>>> 48c3eea89b036aef116454c69313a8033352a63c
=======
resdf_noDetQ_Qint_tmle

#saveRDS(resdf_noDetQ_Qint_tmle, paste0(here::here(),"/data/null_sim_res_noDetQ_Qint_tmle.RDS"))
>>>>>>> 48c3eea89b036aef116454c69313a8033352a63c
=======
resdf_noDetQ_Qint_tmle

#saveRDS(resdf_noDetQ_Qint_tmle, paste0(here::here(),"/data/null_sim_res_noDetQ_Qint_tmle.RDS"))
>>>>>>> 48c3eea89b036aef116454c69313a8033352a63c
=======
resdf_noDetQ_Qint_tmle

#saveRDS(resdf_noDetQ_Qint_tmle, paste0(here::here(),"/data/null_sim_res_noDetQ_Qint_tmle.RDS"))
>>>>>>> 48c3eea89b036aef116454c69313a8033352a63c

