#sink(file=paste0(here::here(),"/simulation study/sim_TCP1_glp1_vs_any_static-IC.Rout"),append=F)

rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))




gc()
d_wide_list <- readRDS(file=here("data/simulated_data_list.RDS"))

d_wide_list <- d_wide_list[1:50]
gc()



int.start.time <- Sys.time()
resdf_ic <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle_glmnet(d_wide_list[[i]], varmethod = "ic", resdf=NULL, Qint=FALSE))
  return(res)
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")




# for(i in 1:length(d_wide_list)){
#   if(i>nrow(resdf_ic)){
#     int.start.time <- Sys.time()
#     resdf_ic <- run_ltmle_glmnet(d_wide_list[[i]], varmethod = "ic", resdf=resdf_ic, Qint=FALSE)
#     int.end.time <- Sys.time()
#     resdf_ic$run_time <- difftime(int.end.time, int.start.time, units="mins")
#   }
# }
# end.time <- Sys.time()
#
# print("runtime: ")
# print(difftime(end.time, start.time, units="hours"))


gc()
saveRDS(resdf_ic, paste0(here::here(),"/data/sim_res_ic.RDS"))

#sink()

