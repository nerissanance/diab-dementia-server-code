
rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))




gc()
d_wide_list <- readRDS(file=here("data/simulated_data_list.RDS"))
d_wide_list <- d_wide_list_full[1:50]
gc()



res <- readRDS(paste0(here::here(),"/data/sim_res_SL_ic.RDS"))
res



SL.lib <- c("SL.mean", "SL.glm", "SL.EN.robust", "SL.glmnet.robust")

#res <- NULL
i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))

res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))i<-1
res <- run_ltmle(d_wide_list[[i]], resdf=res, SL.library = SL.lib, Qint=FALSE)
i<-i+1
gc()
saveRDS(res, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))
i

#test
#res <- run_ltmle(d_wide_list[[1]], varmethod = "ic", resdf=NULL, SL.library = SL.lib, Qint=FALSE)

# int.start.time <- Sys.time()
# resdf_SL_ic <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
#   res <- NULL
#   try(res <- run_ltmle(d_wide_list[[i]], resdf=NULL, SL.library = SL.lib, Qint=FALSE))
#   return(res)
# }
# int.SLd.time <- Sys.time()
# difftime(int.SLd.time, int.start.time, units="mins")
#



gc()
saveRDS(resdf_SL_ic, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))



# d=d_wide_list[[1]]
# resdf=NULL
# SL.library = SL.lib
# Qint=FALSE
#
#          N_time = 11
#          resdf=NULL
#          Qint=F
#          varmethod = "ic"
#          label=""
#
#
#   warn = getOption("warn")
#   options(warn=-1)
#
#   #clean competing events
#   d <-clean_sim_data(d, N_time=N_time)
#
#   #Use only first N time points
#   d <- d %>%
#     dplyr::select(!!(baseline_vars),matches(paste0("_(",paste0(0:(N_time-1),collapse="|"),")$")))
#
#
#   spec_ltmle <- spec_analysis(data=d, c(long_covariates,"event_death_"),
#                               baseline_vars, N_time,
#                               Avars=c("glp1_"),
#                               Yvars=c("event_dementia_"))
#   abar_spec = list(rep(1,N_time),rep(0,N_time))
#
#   #Drop the baseline events
#   spec_ltmle$data <- spec_ltmle$data %>% subset(., select = -c(event_death_0, censor_0, event_dementia_0))
#   spec_ltmle$Cnodes = spec_ltmle$Cnodes[spec_ltmle$Cnodes!="censor_0"]
#   spec_ltmle$Lnodes = spec_ltmle$Lnodes[spec_ltmle$Lnodes!="event_death_0"]
#   spec_ltmle$Ynodes = spec_ltmle$Ynodes[spec_ltmle$Ynodes!="event_dementia_0"]
#
#   set.seed(12345)
#   res = NULL
#
#
#   if(Qint){
#
#     if(N_time==11){
#       qform = c(
#         insulin_0="Q.kplus1 ~ 1",
#         insulin_1="Q.kplus1 ~ 1",
#         event_dementia_1="Q.kplus1 ~ 1",
#         insulin_2="Q.kplus1 ~ 1",
#         event_dementia_2="Q.kplus1 ~ 1",
#         insulin_3="Q.kplus1 ~ 1",
#         event_dementia_3="Q.kplus1 ~ 1",
#         insulin_4="Q.kplus1 ~ 1",
#         event_dementia_4="Q.kplus1 ~ 1",
#         insulin_5="Q.kplus1 ~ 1",
#         event_dementia_5="Q.kplus1 ~ 1",
#         insulin_6="Q.kplus1 ~ 1",
#         event_dementia_6="Q.kplus1 ~ 1",
#         insulin_7="Q.kplus1 ~ 1",
#         event_dementia_7="Q.kplus1 ~ 1",
#         insulin_8="Q.kplus1 ~ 1",
#         event_dementia_8="Q.kplus1 ~ 1",
#         insulin_9="Q.kplus1 ~ 1",
#         event_dementia_9="Q.kplus1 ~ 1",
#         insulin_10="Q.kplus1 ~ 1",
#         event_dementia_10="Q.kplus1 ~ 1"
#       )
#     }
#
#     if(N_time==2){
#       qform = c(
#         insulin_0="Q.kplus1 ~ 1",
#         insulin_1="Q.kplus1 ~ 1",
#         event_dementia_1="Q.kplus1 ~ 1"
#       )
#     }
#
#
#
#
#     try(res <- ltmle(data=spec_ltmle$data,
#                      Anodes = spec_ltmle$Anodes,
#                      Cnodes = spec_ltmle$Cnodes,
#                      Lnodes = spec_ltmle$Lnodes,
#                      Ynodes = spec_ltmle$Ynodes,
#                      survivalOutcome = T,
#                      abar = abar_spec,
#                      Qform = qform,
#                      deterministic.Q.function = det.Q.function,
#                      SL.library = SL.library,
#                      variance.method = varmethod #use tmle variance option for accuracy with positivity violations
#     ))
#
#   }else{
#     try(res <- ltmle(data=spec_ltmle$data,
#                      Anodes = spec_ltmle$Anodes,
#                      Cnodes = spec_ltmle$Cnodes,
#                      Lnodes = spec_ltmle$Lnodes,
#                      Ynodes = spec_ltmle$Ynodes,
#                      survivalOutcome = T,
#                      abar = abar_spec,
#                      deterministic.Q.function = det.Q.function,
#                      SL.library = SL.library,
#                      variance.method = varmethod #use tmle variance option for accuracy with positivity violations
#     ))
#   }
#
#
#   if(!is.null(res)){
#     res <- summary(res)
#     res <- as.data.frame(res$effect.measures$RR)
#     res$label <- label
#   }
#   if(!is.null(resdf)){
#     res <- bind_rows(resdf, res)
#   }
