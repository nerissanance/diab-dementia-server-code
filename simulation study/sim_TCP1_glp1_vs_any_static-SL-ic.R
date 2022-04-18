
rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))




gc()
d_wide_list <- readRDS(file=here("data/simulated_data_list.RDS"))

d_wide_list <- d_wide_list[1:50]
gc()







SL.lib <- c("SL.mean", "SL.glm", "SL.EN.robust", "SL.glmnet.robust")

#test
#res <- run_ltmle(d_wide_list[[1]], varmethod = "ic", resdf=NULL, SL.library = SL.lib, Qint=FALSE)

int.start.time <- Sys.time()
resdf_SL_ic <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle(d_wide_list[[i]], varmethod = "ic", resdf=NULL, SL.library = SL.lib, Qint=FALSE))
  return(res)
}
int.SLd.time <- Sys.time()
difftime(int.SLd.time, int.start.time, units="mins")




gc()
saveRDS(resdf_SL_ic, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))



