sink(file=paste0(here::here(),"/simulation study/sim_TCP1_glp1_vs_any_static-Qint.Rout"),append=F)

rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))




gc()
d_wide_list <- readRDS(file=here("data/simulated_data_list.RDS"))

d_wide_list <- d_wide_list[1:50]
gc()



# -Q-intercept only
start.time <- Sys.time()
resdf_Qint = NULL
for(i in 1:length(d_wide_list)){
  int.start.time <- Sys.time()
  resdf_Qint <- run_ltmle_glmnet(d_wide_list[[i]], varmethod = "ic", resdf=resdf_Qint, Qint=TRUE)
  int.end.time <- Sys.time()
  print("iteration runtime: ")
  print(difftime(int.end.time, int.start.time, units="mins"))
}
end.time <- Sys.time()

print("runtime: ")
print(difftime(end.time, start.time, units="hours"))
#12.18644 mins per iteration
#66.40086 hours

gc()
saveRDS(resdf_Qint, paste0(here::here(),"/data/sim_res_Qint_ic.RDS"))

sink()

