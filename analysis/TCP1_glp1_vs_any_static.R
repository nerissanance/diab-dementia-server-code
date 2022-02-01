
rm(list=ls())
# sink(file="./2_analysis/TCP1_glp1_vs_any_static.Rout",append=F)
Sys.Date()
library(here)
source(here::here("0_config.R"))

#-----------------------------------
#-----------------------------------

# NOTE: edit these hyperparameters or inputs as needed

yr = 2011 #year to start cohort definition (depending on which drugs)
N_time = 11 #number of time points you want to look at
ncores = 10 #number of cores to use

#SL.library = c("SL.glmnet", "SL.glm")
SL.library = c("glm") #for debugging
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
d_wide <- readRDS(file=here("data/simdata_10k.rds"))

d_wide
colnames(d_wide)

# #subset to after yr
# #ATTN: commenting out for now, need to add secdate in later
# d_wide_sub <- d_wide %>% filter(secdate>=yr)

#Use only first N time points
d <- d_wide %>%
  dplyr::select(!!(baseline_vars),matches(paste0("_(",paste0(0:(N_time-1),collapse="|"),")$")))
colnames(d)

baseline_vars
# Check class of variables
str(d)
#convert all to numeric and make sure a dataframe
d <- as.data.frame(sapply(d, as.numeric))
baseline_vars

#check for missingness HACK! NEEDS TO BE CHANGED inside LTMLE
sum(is.na(d))
d[is.na(d)] <- 0 #Missingness due to censoring should be coded 0 as long as censoring variable is equal to 1.
sum(is.na(d)) # NOTE! Check that censoring nodes correspond with NA's

d<- data.table(d)
#note: once events jump to 1, need to remain 1 for remainder of follow up
for(i in 1:(N_time+1)){
  j=i+1
  d[get(paste0("event_dementia_",i))==1, (paste0("event_dementia_",j)):=1]
  d[get(paste0("event_death_",i))==1, (paste0("event_death_",j)):=1]
}
# #edit--when one occurs first, set other to zero so there's no competing event:
# dementia.nodes<- grep("event_dementia_",names(d))
# death.nodes<- grep("event_death_",names(d))
# d[, sum_death :=rowSums(.SD,na.rm=T), .SDcols = death.nodes]
# d[, sum_dementia :=rowSums(.SD,na.rm=T), .SDcols = dementia.nodes]
# table(d$sum_death)
# table(d$event_dementia_10)
# table(d$sum_dementia)
# d[sum_death > sum_dementia, (dementia.nodes) := replace(.SD, .SD == 1, 0), .SDcols = dementia.nodes]
# d[sum_death < sum_dementia, (death.nodes) := replace(.SD, .SD == 1, 0), .SDcols = death.nodes]
# table(d$event_dementia_10)

#-------------------------------------------------------------------------------
#  Contrasting if everyone had glp1 versus not
#-------------------------------------------------------------------------------

#specify LTMLE analysis
spec_ltmle <- spec_analysis(data=d, c(long_covariates,"event_death_"),
                            baseline_vars, N_time,
                            Avars=c("glp1_"),
                            Yvars=c("event_dementia_"))

#specify the intervened treatment
abar_spec = list(rep(1,N_time),rep(0,N_time))




package_stub("SuperLearner", "SuperLearner", SuperLearner_override, {
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
  )})


start.time <- Sys.time()

end.time <- Sys.time()
print("runtime:")
print(difftime(end.time, start.time, units="mins"))
summary(res_RR)


saveRDS(res_RR$cum.g,file=here::here(paste0("data/glp1_sglt2_static_cum_g_",N_time,".rds")))
saveRDS(res_RR$cum.g.unbounded,file=here::here(paste0("data/glp1_sglt2_static_g_",N_time,".rds")))
save(res_RR,file=paste0("data/NOTRANSFER_glp1_sglt2_static",N_time,".RData"))

# sink()


