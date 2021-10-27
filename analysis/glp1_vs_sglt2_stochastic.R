
rm(list=ls())
source(here::here("0_config.R"))
sink("./2_analysis/glp2_vs_sglt2_stochastic_competing_risk.Rout",append=T)

#-----------------------------------
#-----------------------------------

# NOTE: edit these hyperparameters or inputs as needed

yr = 2011 #year to start cohort definition (depending on which drugs)
N_time = 11 #number of time points you want to look at
ncores = 11 #number of cores to use

SL.library = c("SL.glmnet", "SL.glm")
# SL.library = c("glm") #for debugging
nfolds = 10 #number of folds for CV SL
varmethod = "tmle" #variance method

#-----------------------------------
#-----------------------------------



#set up parallelization on windows with the Snow package
options(snow.cores=ncores)

d_wide <- readRDS(file=here("data/time_dem_wide.rds"))                   
d_wide
colnames(d_wide)
table(d_wide$event_dementia_26)
prop.table(table(d_wide$event_dementia_26))*100

#subset to after 2011 and subset to older than 2009
#d_wide_sub <- d_wide %>% filter(calender_0 > 2, age_base>=50) #note this is starting after 2013... update to 2011


#Use only first X time points
d_wide_t11 <- d_wide %>% 
  select(!!(baseline_vars),matches("_(10|[0-9])$"))
colnames(d_wide_t11)

d <- d_wide_t11

# Check class of variables
str(d)
#convert all to numeric and make sure a dataframe
d <- as.data.frame(sapply(d, as.numeric))

#check for missingness
sum(is.na(d))
d[is.na(d)] <- 0 #Missingness due to censoring should be coded 0 as long as censoring variable is equal to 1.
sum(is.na(d)) # NOTE! Check that censoring nodes correspond with NA's


 

#-------------------------------------------------------------------------------
#  Contrasting if everyone had glp1 versus if everyone was on other 2nd line
#-------------------------------------------------------------------------------

#specify LTMLE analysis
N_time=11 #TEMP!
spec_ltmle <- spec_analysis(data=d, c(long_covariates,"event_death_"), 
                            baseline_vars, N_time, 
                            Avars=c("glp1_","sglt2_inhib_"), 
                            Yvars=c("event_dementia_"))

#specify the intervened treatment
abar1 <- abar2 <- spec_ltmle$data[, spec_ltmle$Anodes]
abar1[,2] <- abar2[,1] <- 0
for(k in 2:N_time){
  abar1[,k*2-1] = ifelse(abar1[,k*2-3] == 1, abar1[,k*2-1], 1)
  abar1[,k*2] = 0
  abar2[,k*2] = ifelse(abar2[,k*2-2] == 1, abar2[,k*2], 1)
  abar2[,k*2-1] = 0
}
abar1 <- as.matrix(abar1)
abar2 <- as.matrix(abar2)

res_RR <- NULL
start.time <- Sys.time()
res_RR <- ltmle(data=spec_ltmle$data, 
                Anodes = spec_ltmle$Anodes, 
                Cnodes = spec_ltmle$Cnodes, 
                Lnodes = spec_ltmle$Lnodes, 
                 Ynodes = spec_ltmle$Ynodes,
                 survivalOutcome = T, 
                 abar = list(abar1, abar2),
                 deterministic.g.function = SI_function,
                deterministic.Q.function = det.Q.function,
                  SL.library = SL.library, 
                SL.cvControl = list(V=nfolds), 
                 variance.method = varmethod #use influence curve variance for speed
)
end.time <- Sys.time()
difftime(end.time, start.time, units="mins")
summary(res_RR)

#save relevant output
saveRDS(res_RR$cum.g,file=here::here(paste0("data/glp1_sglt2_stochastic_cum_g_",N_time,".rds")))  
saveRDS(res_RR$cum.g.unbounded,file=here::here(paste0("data/glp1_sglt2_stochastic_g_",N_time,".rds")))  
save(res_RR,file=paste0("data/NOTRANSFER_glp1_sglt2_stochastic",N_time,".RData"))

#took 7 hours to run

sink()