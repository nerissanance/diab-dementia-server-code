

rm(list=ls())
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))

load(paste0(here::here(),"/results/truth_common.Rdata"))

gc()
d_wide_list <- readRDS(paste0(here::here(),"/data/simulated_data_list_commonoutcome.RDS"))
d_wide_list<-d_wide_list[1:10]
gc()


int.start.time <- Sys.time()
resdf_glm_common <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows') %dopar% {
  res <- run_ltmle(d_wide_list[[i]], Qint=TRUE, det.Q=FALSE, varmethod = "tmle", resdf=NULL, SL.library="glm")
}
int.end.time <- Sys.time()
difftime(int.end.time, int.start.time, units="mins")
resdf_glm_common

saveRDS(resdf_glm_common, paste0(here::here(),"/data/sim_res_glm_common.RDS"))



d <- d_wide_list[[1]]

(prop.dementia <-
    1*(d$event_dementia_1 +
         d$event_dementia_2 +
         d$event_dementia_3 +
         d$event_dementia_4 +
         d$event_dementia_5 +
         d$event_dementia_6 +
         d$event_dementia_7 +
         d$event_dementia_8 +
         d$event_dementia_9 +
         d$event_dementia_10 >0)  %>% table %>% prop.table)


prop.table(table((d$glp1_0 + d$glp1_1 +
                      d$glp1_2 +
                      d$glp1_3 +
                      d$glp1_4 +
                      d$glp1_5 +
                      d$glp1_6 +
                      d$glp1_7 +
                      d$glp1_8 +
                      d$glp1_9 +
                      d$glp1_10 ==11),1*(d$event_dementia_1 +
                       d$event_dementia_2 +
                       d$event_dementia_3 +
                       d$event_dementia_4 +
                       d$event_dementia_5 +
                       d$event_dementia_6 +
                       d$event_dementia_7 +
                       d$event_dementia_8 +
                       d$event_dementia_9 +
                       d$event_dementia_10 >0)),1)

table((d$glp1_0 + d$glp1_1 +
         d$glp1_2 +
         d$glp1_3 +
         d$glp1_4 +
         d$glp1_5 +
         d$glp1_6 +
         d$glp1_7 +
         d$glp1_8 +
         d$glp1_9 +
         d$glp1_10 ==11),1*(d$event_dementia_1 +
                              d$event_dementia_2 +
                              d$event_dementia_3 +
                              d$event_dementia_4 +
                              d$event_dementia_5 +
                              d$event_dementia_6 +
                              d$event_dementia_7 +
                              d$event_dementia_8 +
                              d$event_dementia_9 +
                              d$event_dementia_10 >0))
#
# 0     1
# FALSE 63378 34350
# TRUE   2090   182

(182*63378)/(34350*2090)


