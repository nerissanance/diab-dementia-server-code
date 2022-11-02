sink(file=paste0(here::here(),"/simulation study/sim_TCP1_glp1_vs_any_static-Qint-sl.Rout"),append=F)

rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))


# #set up parallelization on windows with the Snow package
# library(snow)
# options(snow.cores=8)




gc()
d_wide_list <- readRDS(file=here("data/simulated_data_list.RDS"))
gc()

d_wide_list <- d_wide_list[1:50]


#Make lasso and elastic net robust functions... use the Estimate_override internal function


# -Q-intercept only
gc()
start.time <- Sys.time()
resdf_Qint = NULL
for(i in 1:length(d_wide_list)){
  int.start.time <- Sys.time()
  resdf_Qint <- run_ltmle_sl(d_wide_list[[i]], varmethod = "ic", resdf=resdf_Qint, SL.library = c("SL.mean","SL.glm","SL.glmnet_robust","SL.EN_robust"), Qint=TRUE)
  int.end.time <- Sys.time()
  print("iteration runtime: ")
  print(difftime(int.end.time, int.start.time, units="mins"))

}
end.time <- Sys.time()

print("runtime: ")
print(difftime(end.time, start.time, units="mins"))
#12.18644 mins per iteration

gc()
saveRDS(resdf_Qint, here::here("/data/sim_res_Qint_ic.RDS"))

sink()

