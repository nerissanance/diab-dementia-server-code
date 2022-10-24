

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
for(i in 1:50){

  cat(i,"\n")
  d <- d_wide_list[[i]]
  d$id <- 1:nrow(d)
  d <- d %>% select(id, L1, A1, Y2, L2, A2, Y3)


  res_df <- NULL
  res_df <- foreach(j = 1:100, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {

    source(here::here("0_config.R"))
    source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
    source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))
    source(paste0(here::here(),"/simulation study/sim_simple/0_simple_sim_functions.R"))

    set.seed(j)
    d <- data.table(d)
    res0 <- NULL
    try(res0 <- run_ltmle_simple(d %>% subset(., select=-c(id)),  varmethod = "ic", SL.library = c("glm")), silent=TRUE)
    if(!is.null(res0)){
      res0$boot_analysis="main"
    }
    dboot <- d[sample(.N, nrow(d),replace=TRUE)]

    res1 <- res2 <- NULL
    try(res1 <- run_ltmle_simple(dboot,  varmethod = "ic", SL.library = c("glm"), id=dboot$id), silent=TRUE)
    if(!is.null(res1)){
      res1$boot_analysis="CV"
      }

    dboot <- dboot %>% group_by(id) %>% slice(1) %>% ungroup() %>% select(L1, A1, Y2, L2, A2, Y3)
    try(res2 <- run_ltmle_simple(dboot,  varmethod = "ic", SL.library = c("glm")), silent=TRUE)
    if(!is.null(res2)){
      res2$N<-nrow(dboot)
      res2$boot_analysis="sub"
    }
    res <- bind_rows(res0, res1, res2)
    return(res)
  }
  res_df




  gc()
  res_df$iteration <- i
  saveRDS(res_df, paste0(here::here(),"/data/sim_simple/bootstrap/sim_res_boot_glm_CV_simple_debug_",i,".RDS"))
  resdf_boot = bind_rows(resdf_boot, res_df)
}




head(resdf_boot)
