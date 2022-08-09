
setwd(paste0(here::here(),"/simulation study/"))
gc()
d_wide_list <- readRDS(file=here("data/simulated_data_list.RDS"))
gc()

library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))


sink("glmnet_error_checking.txt")


#-updated to print X dimensions

# #temp
# d=d_wide_list[[1]]
# N_time = 11 #number of time points you want to look at
# SL.library = c("SL.glmnet")
# resdf=NULL
# Qint=F
# gcomp=F
# det.Q=F
# gbound = c(0.01, 1)
# override_function=SuperLearner_override
# varmethod = "ic" #variance method
# alt=FALSE
# label=""
try(res <- run_ltmle_glmnet(d_wide_list[[1]], resdf=NULL, Qint=FALSE, det.Q=FALSE, varmethod = "ic"))

print(res)

sink()
