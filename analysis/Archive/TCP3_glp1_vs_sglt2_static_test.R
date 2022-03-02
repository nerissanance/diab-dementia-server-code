
rm(list=ls())
Sys.Date()
library(here)
source(here::here("0_config.R"))

#-----------------------------------
#-----------------------------------

# NOTE: edit these hyperparameters or inputs as needed

yr = 2011 #year to start cohort definition (depending on which drugs)
N_time = 11 #number of time points you want to look at
N_time = 2 #number of time points you want to look at
ncores = 10 #number of cores to use

SL.library = c("SL.glmnet")
# SL.library = c("glm") #for debugging
nfolds = 10 #number of folds for CV SL
varmethod = "tmle" #variance method

#-----------------------------------
#-----------------------------------




# NOTE: set up to run with just glmnet
#-----------------------------------
#Need to run this script to get glmnet implementation of ltmle, otherwise is CRAN version
#source(here::here("0_glmnet_specs.R"))


#set up parallelization on windows with the Snow package
options(snow.cores=ncores)

# d_wide <- readRDS(file=here("data/time_dem_wide.rds"))
d_wide <- readRDS(file=here("data/data_clean.rds"))

# #subset to after yr
# #ATTN: commenting out for now, need to add secdate in later
# d_wide_sub <- d_wide %>% filter(secdate>=yr)

#Use only first N time points
d <- d_wide %>%
  dplyr::select(!!(baseline_vars),matches(paste0("_(",paste0(0:(N_time-1),collapse="|"),")$")))
colnames(d)


#-------------------------------------------------------------------------------
#  Contrasting if everyone had glp1 versus if everyone was on sglt2
#-------------------------------------------------------------------------------

#specify LTMLE analysis
spec_ltmle <- spec_analysis(data=d, c(long_covariates,"event_death_"),
                            baseline_vars, N_time,
                            Avars=c("glp1_","sglt2_inhib_"),
                            Yvars=c("event_dementia_"))

#specify the intervened treatment
abar_spec = list(rep(c(1,0),N_time),rep(c(0,1),N_time))



start.time <- Sys.time()

package_stub("SuperLearner", "SuperLearner", SuperLearner_override, {
  testthatsomemore::package_stub("ltmle", "Estimate", Estimate_override, {
    res_RR <- ltmle(data=spec_ltmle$data,
                  Anodes = spec_ltmle$Anodes,
                  Cnodes = spec_ltmle$Cnodes,
                  Lnodes = spec_ltmle$Lnodes,
                  Ynodes = spec_ltmle$Ynodes,
                  survivalOutcome = T,
                  abar = abar_spec,
                  deterministic.Q.function = det.Q.function,
                  SL.library = SL.library,
                  SL.cvControl = list(V=nfolds),
                  variance.method = varmethod #use tmle variance option for accuracy with positivity violations
    )})})

end.time <- Sys.time()
print("runtime:")
print(difftime(end.time, start.time, units="mins"))
summary(res_RR)


save(res_RR,file=paste0("data/NOTRANSFER_glp1_sglt2_static",N_time,".RData"))



