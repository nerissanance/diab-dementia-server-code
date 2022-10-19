### synthesizeDD.R ---
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Nov  3 2021 (15:20)
## Version:
## Last-Updated: Nov  5 2021 (17:47)
##           By: Thomas Alexander Gerds
##     Update #: 6
#----------------------------------------------------------------------
##
### Commentary:
##
### Change Log:
#----------------------------------------------------------------------


rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))





cc <- fread(here::here("data/coefficients_outcome_blind.txt")) %>% subset(., select = -c(V1))


sim_truth <- calc_sim_truth(cc)
cRD<-sim_truth$cRD
cRR<-sim_truth$cRR
cRD3<-sim_truth$cRD3
cRR3<-sim_truth$cRR3



save(cRD, cRR, file=paste0(here::here(),"/results/truth_blind_T10.Rdata"))
save(cRD3, cRR3, file=paste0(here::here(),"/results/truth_blind_T4.Rdata"))

