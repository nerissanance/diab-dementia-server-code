

rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))
source(paste0(here::here(),"/simulation study/sim_simple/0_simple_sim_functions.R"))

library(parallel)
library(doParallel)
registerDoParallel(cores=50)

rm(list=ls())
gc()
d_wide_list <- readRDS(file=here("data/simulated_data_list_simple.RDS"))
gc()

i<-j<-1
resdf_boot = NULL
for(i in 1:length(d_wide_list)){

  cat(i,"\n")
  d <- d_wide_list[[i]]
  d$id <- 1:nrow(d)

  res_df <- NULL
  res_df <- foreach(j = 1:200, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {

    source(here::here("0_config.R"))
    source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
    source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))
    source(paste0(here::here(),"/simulation study/sim_simple/0_simple_sim_functions.R"))

    set.seed(j)
    d <- data.table(d)
    dboot <- d[sample(.N, nrow(d),replace=TRUE)]
    dboot <- dboot %>% group_by(id) %>% slice(1) %>% ungroup() %>% select(id, L1, A1, Y2, L2, A2, Y3)

    res <- NULL
    try(res <- run_ltmle_simple(dboot,  varmethod = "ic", SL.library = c("glm")), silent=TRUE)
    if(!is.null(res)){res$N<-nrow(dboot)}
    return(res)
  }
  res_df

  gc()
  res_df$iteration <- i
  saveRDS(res_df, paste0(here::here(),"/data/sim_simple/bootstrap/sim_res_boot_glm_without_replacement_simple_200iter_",i,".RDS"))

}





