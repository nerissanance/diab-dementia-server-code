

rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))




gc()
d_wide_list <- readRDS(file=here("data/simulated_data_list.RDS"))
d_wide_list <- d_wide_list[1:2]
gc()

i <- 1
resdf_boot <- NULL
for(i in 1:length(d_wide_list)){

  cat(i,"\n")
  d <- d_wide_list[[i]]
  d$id <- 1:nrow(d)

  #TEMP
  resdf<- foreach(j = 1:2, .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {

    set.seed(j)
    dboot <- d[sample(.N, nrow(d),replace=TRUE)]

    #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
    #run this code to drop ties, or use the ID argument to CV by ID
    dboot <- dboot %>% distinct()
    #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

    res <- NULL
    try(res <- run_ltmle_glmnet(dboot, resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "ic", id=dboot$id))
    print(res)
    return(res)
  }
  resdf

  gc()
  resdf$iteration <- i
  resdf_boot <- bind_rows(resdf_boot, resdf)
  saveRDS(resdf, paste0(here::here(),"/data/sim_res_boot_",i,".RDS"))

}



saveRDS(resdf, paste0(here::here(),"/data/sim_res_boot.RDS"))


