#sink(file=paste0(here::here(),"/simulation study/sim_TCP1_glp1_vs_any_static-glm.Rout"),append=F)

rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))




gc()
d_wide_list <- readRDS(file=here("data/simulated_data_list.RDS"))
d_wide_list <- d_wide_list[1:100]
gc()


int.start.time <- Sys.time()
resdf_glm <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows') %dopar% {
  res <- run_ltmle(d_wide_list[[i]], varmethod = "ic", resdf=NULL, SL.library="glm")
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")
resdf_glm



gc()
saveRDS(resdf_glm, paste0(here::here(),"/data/sim_res_glm_ic.RDS"))

#sink()

