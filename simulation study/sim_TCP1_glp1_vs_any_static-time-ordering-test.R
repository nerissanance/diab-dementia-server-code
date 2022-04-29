
rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))




gc()
d_wide_list <- readRDS(file=here("data/simulated_data_list.RDS"))
d_wide_list <- d_wide_list[1:2]
gc()


int.start.time <- Sys.time()
resdf_glm1 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows') %dopar% {
  res <- run_ltmle_glmnet(d_wide_list[[i]], varmethod = "ic", resdf=NULL, Qint = T, alt=F)
}
int.end.time <- Sys.time()
diff1<-difftime(int.end.time, int.start.time, units="mins")

int.start.time <- Sys.time()
resdf_glm2 <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows') %dopar% {
  res <- run_ltmle_glmnet(d_wide_list[[i]], varmethod = "ic", resdf=NULL,  Qint = T, alt=T)
}
int.end.time <- Sys.time()
diff2<-difftime(int.end.time, int.start.time, units="mins")

diff1
diff2

resdf_glm1
resdf_glm2


