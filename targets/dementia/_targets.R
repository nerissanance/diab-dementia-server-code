# JICI project
#
# Collaborators: Nerissa, Andrew, Bochra, Kathrine, Zeyi, Maya, Mark, Thomas, Christian
#
# Register data until 2018 are located at x:/Data/Rawdata_Hurtig/706582
# Corona update data are in the latest delivery (look for updates!)
# Currently we use
#  V:/Data/Workdata/706582/Corona_update/Data/11. levering.
#
# setwd("z:/Workdata/706582/Andrew Mertens/targets_diabetes_dementia/")
library(targets)

#load packages
tar_option_set(packages=c("heaven",
                          "ltmle",
                          "data.table",
                          "tidyverse",
                          "SuperLearner",
                          "tictoc",
                          "glmnet",
                          "Matrix",
                          "Publish",
                          "matrixStats",
                          "speedglm"
                          ,"doParallel"
                          ,"parallel"
                          ,"snow"
                          ,"doSNOW"
                          ,"foreach"))
## debug="time_dem")
#load functions
#rm(list=ls())
set.seed(4534)

nix=lapply(list.files("./functions/", full.names = TRUE, recursive=TRUE), source)

#load global variables/file paths

# -------------------------------------------------------------------------------------------------------------
# Configuration
# -------------------------------------------------------------------------------------------------------------
# path_corona_update='V:/Data/Workdata/706582/Corona_update/Data/13. levering/'
# path_corona_update11='V:/Data/Workdata/706582/Corona_update/Data/11. levering/'
# path_corona_update13='V:/Data/Workdata/706582/Corona_update/Data/13. levering/',
# path_corona_update14='X:/Data/Rawdata_Hurtig/706582/Data til COVID-19 studier/14_lev/',
# Xdrive_path_corona_update='X:/Data/Rawdata_Hurtig/706582/Data til COVID-19 studier/13_lev/' #for data only on the X-drive (used in lab data)
#Xdrive_path_corona_update14='X:/Data/Rawdata_Hurtig/706582/Data til COVID-19 studier/14_lev/',
# path_root_x = 'X:/Data/Rawdata_Hurtig/706582/'

#define some important global variables
N_time=11
node_length=180
todaydate=as.character(Sys.Date())
#Set Inf for all or lower for subsetting loaded data
#nobs=300000
nobs=Inf
#Set Inf for all or lower for subsetting  data used in TMLE
#N_tmle_subset=1000
N_tmle_subset=Inf
final_date="2022-07-20"

#######
#set up config for the boostrap if running it:
# boot==TRUE
# if(make_boot==TRUE){
# #   #set up parallelization
# #   #use about half of space
#   ncores <- 4#floor(detectCores()/2)
#   mycluster <- parallel::makeCluster(ncores)
#   doSNOW::registerDoSNOW(cl=mycluster)
# }
##############

list(
# set some simple targets that will be used for cutoff points for continuous variables
# for exposure definition, etc.
  tar_target(varmethod, "ic"),
  tar_target(breaks_hba1c, c(0,48,53,58,70,150)),
  tar_target(druglist,list(
    insulin="A10A",
    metformin=c("A10BA02","A10BD02","A10BD03","A10BD05","A10BD07","A10BD08","A10BD10",
                "A10BD11","A10BD13","A10BD14","A10BD15","A10BD16","A10BD17",
                "A10BD18","A10BD20","A10BD22","A10BD23","A10BD25","A10BD26"),
    biguanidk_o=c("A10BA01","A10BA03","A10BD01"),
    sulfonylurea=c("A10BB","A10BD01","A10BD02","A10BD04","A10BD06"),
    sulfomid="A10BC",
    alfa_glusidase_inhib=c("A10BF","A10BD17"),
    thiazolodinidion=c("A10BG","A10BD03","A10BD04","A10BD05","A10BD06",
                       "A10BD09","A10BD12","A10BD26"),
    dpp4_inhib=c("A10BH","A10BD07","A10BD08","A10BD09","A10BD10","A10BD11",
                 "A10BD12","A10BD13","A10BD18","A10BD19","A10BD21","A10BD22",
                 "A10BD24","A10BD25"),
    glp1="A10BJ",
    sglt2_inhib=c("A10BK","A10BD15","A10BD16","A10BD19","A10BD20",
                  "A10BD21","A10BD23","A10BD24","A10BD25"),
    repaglinid=c("A10BX","A10BD14")
  )),
  tar_target(baseline_vars, c("age_base","sex", "code5txt", "quartile_income")),#,metformin_dur",
                             # "year_2nd_line_start","code5txt_miss","quartile_income_miss" )),
  tar_target(long_covariates, c("insulin_", "chronic.pulmonary.disease_",
                                "hypertension_", "myocardial.infarction_", "ischemic.heart.disease_",
                                "heart.failure_", "renal.disease_","event_death_"))#, "stroke_"))
  #export these
  ,tar_target(export_globalvars,{

    drugs<- lapply(druglist,FUN=function(x)paste(x,collapse=","))
    drugs <-data.frame(unlist(druglist))
    fwrite(drugs,file=paste0("./export/druglist_",todaydate,".txt"),row.names=T)

    vars <- data.frame(c(baseline_vars,long_covariates,final_date))
    fwrite(vars,file=paste0("./export/covars_",todaydate,".txt"),row.names=F)


  }),


# -------------------------------------------------------------------------------------------------------------
## Define population
# -------------------------------------------------------------------------------------------------------------
# Get all medication codes
# The following named list identifies all types of hypoglycemic therapy.  Note  that many compounds are not  only identified as singular compounds,  but also as part of combination therapy drugs.  Each entry is the "start" of the code defining a drug
  # tar_target(A10, get_atc_codes(path_corona_update=path_corona_update,nobs=nobs)),
  #load pnr's of all people who used diabetes drugs to subset other data to
  # tar_target(diabetes_pop,get_diabetes_population(A10=A10,druglist=druglist)),
  #get diabetes drug usage and make non-overlapping
   # tar_target(drugdiab,get_drug_diabetes(A10=A10, druglist=druglist)),
  #Get sex and imigration status
  # tar_target( demographics, get_demographics(path_corona_update=path_corona_update11,filter=diabetes_pop[,.(pnr=pnr)],nobs=nobs)),
  #Get all diagnostic code
  # tar_target(diagnoses,get_diagnoses(path_corona_update=path_corona_update, filter=diabetes_pop[,.(pnr=pnr)],nobs=nobs)),
  #get income
  # tar_target(income, get_income(pnr=diabetes_pop[,.(pnr=pnr)], nobs=nobs)),
  #get charlson diagnoses, including dementia, (stroke is available here)
  # tar_target(charlson,get_charlson(diagnoses=diagnoses, pnr=diabetes_pop[,.(pnr=pnr, secdate=secdate)])),
  #Get mortality
  # tar_target(doede,get_death(path_corona_update,filter=diabetes_pop[,.(pnr=pnr)],nobs=nobs)),
  #get house information
  # tar_target(household,get_household(pnr=diabetes_pop[,.(pnr=pnr)],nobs=nobs)),
  #get education, etc.
  # tar_target(edu,get_edu(filter=diabetes_pop[,.(pnr=pnr)],nobs=nobs)),
  #get lab values (NOTE! time-intensive, and something is causing it to rerun each new session even without upstream changes)
  #Rerun when changing corona path, otherwise load cached version
  # tar_target(lab,get_labs(path_corona_update=Xdrive_path_corona_update, pnr=diabetes_pop[,.(pnr=pnr)], nobs=nobs,use_cached=TRUE)),
  #merge in dementia (and hypertension) medication prescriptions for combination into dementia outcome
  # tar_target(comorbidities,get_dementia_meds(cov=charlson, path_corona_update=path_corona_update, pnr=diabetes_pop[,.(pnr=pnr)], nobs=nobs)),
  #adding in region of Denmark
  # tar_target(region,get_region(path=path_root_x,filter=diabetes_pop[,.(pnr=pnr)],nobs=nobs)),
# -------------------------------------------------------------------------------------------------------------
# ## CLEAN DATA ----------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------
  # make the cohort file with start date and variables used for exclusion
  # applies exclusions that will happen with all analyses (age>=50, starts secondline)
  # also makes indicator for later exclusions on prior dementia
  #
#   tar_target(raw_diabetes_cohort,subset_diabetes_cohort(diabetes_cohort=diabetes_pop,
#                                                         danish_pop=demographics,
#                                                         comorbidities=comorbidities,
#                                                         doede=doede)),
#   #creates baseline data from the raw files, all time-constant covariates made here
#   tar_target(base_dem,merge_baseline_raw_data(danish_pop=raw_diabetes_cohort,
#                                               comorbidities=comorbidities,
#                                               household=household,
#                                               income=income,
#                                               edu=edu,
#                                               lab=lab,
#                                               region=region)),
#   #all time-varying dx covariates are made here in long format with observed dates
#   tar_target(time_dem, merge_longitudinal_raw_data(  base_dem=base_dem,
#                                                    comorbidities=comorbidities,
#                                                    drugdiab=drugdiab,
#                                                    node_length=node_length,
#                                                    final_date=final_date)),
#   #imputing the baseline data-- to the median or the mode depending on data type
#   #also makes missingness indicators:
#   tar_target(baseline_imputed, impute_baseline_missingness(base_dem=base_dem)),
#   #"imputing" the missing data by LVCF, essentially once you get a dx you have it for the remainer of follow up
#   #also makes missingness indicators:
#   tar_target(longitudinal_imputed, impute_longitudinal_missingness(time_dem=time_dem)),
#   #prepping the time updates of the lab data, currently just A1C values:
#   tar_target(hba1c_wide,{
#       node_length
#       clean_labs(lab=lab,
#                  base_dem=base_dem,
#                  node_length=node_length,breaks_hba1c=breaks_hba1c,
#                  final_date=final_date)
#   }),
#   #making cohort dataset with wide format
#   tar_target(wide_df, transform_long_to_wide(long_df=longitudinal_imputed,
#                                              base_dem=baseline_imputed,
#                                              long_covariates=long_covariates,
#                                              N_time=N_time,
#                                              labdata=hba1c_wide)),
#
#   #make txt files of exclusions and final n
#   tar_target(mat_exclusions, {
#       mat_excl <- make_flowchart(diabetes_pop=diabetes_pop,
#                                  year=2009,
#                                  danish_pop=demographics,
#                                  comorbidities=comorbidities,
#                                  doede=doede,
#                                  wide_df=wide_df)
#       write.table((mat_excl),file=paste0("./export/flowchart.txt"))
#   }),
#   ##make table 1
#   tar_target(tableone,{
#       tab1 <- make_table1(base_dem=base_dem,
#                           wide_df=wide_df,
#                           outcome="event_dementia_",
#                           year=2009,
#                           cvd_subset=NULL)
#
#   fwrite(tab1,file=paste0("./export/tableone_",todaydate,".txt"))
# })
# -------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------
# ## run main analyses ----------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------
#
###for the simulated data at Berkeley, need to make the dtaaset within targets
tar_target(wide_df, {
  simdata_100k <- readRDS("../../data/simdata_100k.rds")
  #add in variable for prior dementia
  simdata_100k$prior_dementia <- 0
  #make seqdate between 2009 and 2011
  simdata_100k$secdate <- sample(seq(as.Date('2009/01/01'),as.Date(final_date),by="day"),replace=T,size=nrow(simdata_100k))
  #make BL hba1c variable (random for now)
  simdata_100k$hba1c365_0 <- sample(x=c("[0,48)","[48,53)","[53,70)","[70,150]"),nrow(simdata_100k),replace=T,prob=c(0.5,0.2,0.2,0.1))
  # need to rename/change censoring nodes
  for(i in 1:10){
    data.table::setnames(simdata_100k,(paste0("censor_",i)),paste0("censor_dem_",i))
    simdata_100k[,paste0("any_second_line_",i):=ifelse(get())]
  }
  return(simdata_100k)
})

## Target Causal parameter 1: static comparison, glp1 only always on vs. never on (adjusted)
###########################################################
#This preps the object to be read by analysis
,tar_target(tmle_spec_TCP1, subset_and_specify_analysis(data=wide_df,
                                                         Avars="glp1_",
                                                         Yvars="event_dementia_",
                                                         Cvars="censor_dem_",
                                                         yr=2009,
                                                         baseline_vars=baseline_vars,
                                                         long_covariates=c(long_covariates),#dont have this in sim data!:,"any_second_line_"),
                                                         N_time=11))
###runs analysis and produces all output
,tar_target(ltmle_results_TCP1, ltmle_analysis(spec_ltmle=tmle_spec_TCP1,
                                               SL.library="SL.glmnet",
                                               abar_spec = list(rep(1,(N_time-1)),rep(0,(N_time-1))),
                                               det.Q.function=NULL,
                                               varmethod=varmethod,
                                               Nsub=N_tmle_subset))

### Target Causal parameter 1: static comparison, glp1 only always on vs. never on (crude)
,tar_target(tmle_spec_TCP1_crude, subset_and_specify_analysis(data=wide_df,
                                                            Avars="glp1_",
                                                            Yvars="event_dementia_",
                                                            Cvars="censor_dem_",
                                                            yr=2009,
                                                            baseline_vars=NULL,
                                                            long_covariates=NULL,
                                                            N_time=11))
,tar_target(ltmle_results_TCP1_crude, ltmle_analysis(spec_ltmle=tmle_spec_TCP1_crude,
                                                   SL.library="SL.glmnet",
                                                   abar_spec = list(rep(1,(N_time-1)),rep(0,(N_time-1))),
                                                   det.Q.function=NULL,
                                                   varmethod=varmethod,
                                                   Nsub=N_tmle_subset))
#
####Adjusting for some additional covariates not in the inital list
,tar_target(tmle_spec_TCP1b, subset_and_specify_analysis(data=wide_df,
                                                        Avars="glp1_",
                                                        Yvars="event_dementia_",
                                                        Cvars="censor_dem_",
                                                        yr=2009,
                                                        baseline_vars=c(baseline_vars,"immigrant","origin_miss"),
                                                        long_covariates=c(long_covariates,"any_second_line_"),
                                                        N_time=11))

#   tar_target(ltmle_results_TCP1b, ltmle_analysis(spec_ltmle=tmle_spec_TCP1b,
#                                                  SL.library="SL.glmnet",
#                                                  abar_spec = list(rep(1,(N_time-1)),rep(0,(N_time-1))),
#                                                  det.Q.function=NULL,
#                                                  varmethod=varmethod,
#                                                  Nsub=N_tmle_subset)),
#


#  ## Target Causal parameter 3: static comparison, active comparator
##########################################################
## intervene on the comparator arm so that they are using it throughout follow up
##3A ## COMPARISON: SGLT2
,tar_target(tmle_spec_TCP3a, subset_and_specify_analysis(data=wide_df,
                                                       Avars=c("glp1_","sglt2_inhib_"),
                                                       Yvars="event_dementia_",
                                                       Cvars="censor_dem_",
                                                       yr=2011,
                                                       baseline_vars=baseline_vars,
                                                       long_covariates=long_covariates,
                                                       N_time=11))
,tar_target(ltmle_results_TCP3a, ltmle_analysis(spec_ltmle=tmle_spec_TCP3a,
                                               SL.library="SL.glmnet",
                                               abar_spec = list(rep(c(1,0),(N_time-1)),rep(c(0,1),(N_time-1))),
                                               det.Q.function=NULL,
                                               varmethod=varmethod,
                                               Nsub=N_tmle_subset))
##3B ## COMPARISON: any second line
,tar_target(tmle_spec_TCP3b, subset_and_specify_analysis(data=wide_df,
                                                       Avars=c("glp1_","any_second_line_"),
                                                       Yvars="event_dementia_",
                                                       Cvars="censor_dem_",
                                                       yr=2011,
                                                       baseline_vars=baseline_vars,
                                                       long_covariates=long_covariates,
                                                       N_time=11))
,tar_target(ltmle_results_TCP3b, ltmle_analysis(spec_ltmle=tmle_spec_TCP3b,
                                               SL.library="SL.glmnet",
                                               abar_spec = list(rep(c(1,0),(N_time-1)),rep(c(0,1),(N_time-1))),
                                               det.Q.function=NULL,
                                               varmethod=varmethod,
                                               Nsub=N_tmle_subset))
##3C ## COMPARISON: DPP4 inhibitors
,tar_target(tmle_spec_TCP3c, subset_and_specify_analysis(data=wide_df,
                                                        Avars=c("glp1_","dpp4_inhib_"),
                                                        Yvars="event_dementia_",
                                                        Cvars="censor_dem_",
                                                        yr=2011,
                                                        baseline_vars=baseline_vars,
                                                        long_covariates=c(long_covariates),
                                                        N_time=11))
,tar_target(ltmle_results_TCP3c, ltmle_analysis(spec_ltmle=tmle_spec_TCP3c,
                                               SL.library="SL.glmnet",
                                               abar_spec = list(rep(c(1,0),(N_time-1)),rep(c(0,1),(N_time-1))),
                                               det.Q.function=NULL,
                                               varmethod=varmethod,
                                               Nsub=N_tmle_subset))




# NEW: Target Causal parameter 4: glp1+sglt2 vs others
##########################################################
#cant do this comparison bc no combined glp1 sglta2 var and I'm too lazy to mke it right now
# ,tar_target(tmle_spec_TCP4, subset_and_specify_analysis(data=wide_df,
#                                                         Avars=c("glp1_or_sglt2_","any_second_line_no_sglt2_"),
#                                                         Yvars="event_dementia_",
#                                                         Cvars="censor_dem_",
#                                                         yr=2009,
#                                                         baseline_vars=baseline_vars,
#                                                         long_covariates=long_covariates,
#                                                         N_time=11))
# ,tar_target(ltmle_results_TCP4, ltmle_analysis(spec_ltmle=tmle_spec_TCP4,
#                                                SL.library="SL.glmnet",
#                                                abar_spec = list(rep(c(1,0),(N_time-1)),rep(c(0,1),(N_time-1))),
#                                                det.Q.function=NULL,
#                                                varmethod=varmethod,
#                                                Nsub=N_tmle_subset))
,tar_target(clean_main_results,{
r1 <- clean_results(fit=ltmle_results_TCP1, TCP="TCP1", analysis="primary", outcome="dementia")
r2 <- clean_results(fit=ltmle_results_TCP3a, TCP="TCP3a", analysis="GLP1_vs_SGLT2", outcome="dementia")
r3 <- clean_results(fit=ltmle_results_TCP3b, TCP="TCP3b", analysis="GLP1_vs_any2nd", outcome="dementia")
r4 <- clean_results(fit=ltmle_results_TCP3c, TCP="TCP3c", analysis="GLP1_vs_DPP4", outcome="dementia")
# r5 <- clean_results(fit=ltmle_results_TCP4, TCP="TCP4", analysis="GLP1_SGLT_together", outcome="dementia")
r6 <- clean_results(fit=ltmle_results_TCP1_crude, TCP="TCP1",analysis="crude",outcome="dementia")

results <- rbind(r1,r2,r3,r4,r6)
fwrite(results,file=paste0("./export/table_ltmle_main_results_dementia_",todaydate,"_",varmethod,".txt"))
})

##########################################################
##### Sensitivity analyses  ----------------------------------------------------------------
##########################################################
#
# #using IC for variance
#   #runs analysis and produces all output
#   ,tar_target(ltmle_results_TCP_ic, ltmle_analysis(spec_ltmle=tmle_spec_TCP1,
#                                                  SL.library="SL.glmnet",
#                                                  abar_spec = list(rep(1,(N_time-1)),rep(0,(N_time-1))),
#                                                  det.Q.function=NULL,
#                                                  varmethod="ic",
#                                                  Nsub=N_tmle_subset))
#
#
#using IC for variance, also with deterministic Q

#runs analysis and produces all output
,tar_target(det.Q.function,{
  function(data, current.node, nodes, called.from.estimate.g){
    death.index <- grep("death_",names(data))
    if(length(death.index)==0)stop("no node found")
    hist.death.index <- death.index[death.index < current.node]
    if(length(hist.death.index)==0)return(NULL)
    if(length(hist.death.index)==1){
      is.deterministic <- data[,hist.death.index]==1
    } else {
      is.deterministic <- as.logical(data[,max(hist.death.index),drop=TRUE])
    }#if death before, remove from fitting
    is.deterministic[is.na(is.deterministic)] <- F
    return(list(is.deterministic=is.deterministic, Q.value=0))
  }})
,tar_target(ltmle_results_TCP_detQ, ltmle_analysis(spec_ltmle=tmle_spec_TCP1,
                                                SL.library="SL.glmnet",
                                                abar_spec = list(rep(1,(N_time-1)),rep(0,(N_time-1))),
                                                det.Q.function=det.Q.function,
                                                varmethod="ic",
                                                Nsub=N_tmle_subset))


# restricting TCP1 to start in 2011
#   ## Target Causal parameter 1: static comparison, glp1 only
#   #This preps the object to be read by analysis
,tar_target(tmle_spec_TCP1_2011, subset_and_specify_analysis(data=wide_df,
                                                       Avars="glp1_",
                                                       Yvars="event_dementia_",
                                                       Cvars="censor_dem_",
                                                       yr=2009,
                                                       baseline_vars=baseline_vars,
                                                       long_covariates=long_covariates,
                                                       N_time=11))
# #   #runs analysis and produces all output
,tar_target(ltmle_results_TCP1_2011, ltmle_analysis(spec_ltmle=tmle_spec_TCP1_2011,
                                              SL.library="SL.glmnet",
                                              abar_spec = list(rep(1,(N_time-1)),rep(0,(N_time-1))),
                                              det.Q.function=NULL,
                                              varmethod=varmethod,
                                              Nsub=N_tmle_subset))

### Sensitivity analysis: 3 month time nodes
### Using TCP1, shortening time nodes
# ,tar_target(time_dem_3mo, merge_longitudinal_raw_data(node_length=90,
#                                                      base_dem=base_dem,
#                                                      comorbidities=comorbidities,
#                                                      final_date=final_date,
#                                                      drugdiab=drugdiab))
# ,tar_target(longitudinal_imputed_3mo, impute_longitudinal_missingness(time_dem=time_dem_3mo))
# ,tar_target(hba1c_wide_3mo,{
#   node_length
#   clean_labs(lab=lab,
#              base_dem=base_dem,
#              node_length=90,breaks_hba1c=breaks_hba1c,
#              final_date=final_date)
# })
# ,tar_target(wide_df_3mo, transform_long_to_wide(long_df=longitudinal_imputed_3mo,
#                                                N_time=22,
#                                                base_dem=baseline_imputed,
#                                                long_covariates=long_covariates,
#                                                labdata=hba1c_wide_3mo))
# ,tar_target(tmle_spec_TCP1_3mo, subset_and_specify_analysis(data=wide_df_3mo,
#                                                            N_time=22,
#                                                            Avars="glp1_",
#                                                            Yvars="event_dementia_",
#                                                            Cvars="censor_dem_",
#                                                            yr=2009,
#                                                            baseline_vars=baseline_vars,
#                                                            long_covariates=long_covariates))
# ,tar_target( ltmle_results_TCP1_3mo, ltmle_analysis(spec_ltmle=tmle_spec_TCP1_3mo,
#                                                    abar_spec = list(rep(1,21),rep(0,21)),
#                                                    SL.library="SL.glmnet",
#                                                    det.Q.function=NULL,
#                                                    varmethod=varmethod,
#                                                    Nsub=N_tmle_subset))
# ### Sensitivity analysis: 12 month time nodes
# ### Using TCP1, lengthening time nodes
# ,   tar_target(time_dem_12mo, merge_longitudinal_raw_data(node_length=360,
#                                                       base_dem=base_dem,
#                                                       comorbidities=comorbidities,
#                                                       drugdiab=drugdiab,
#                                                       final_date=final_date))
# , tar_target(longitudinal_imputed_12mo, impute_longitudinal_missingness(time_dem=time_dem_12mo))
# ,tar_target(hba1c_wide_12mo,{
#   node_length
#   clean_labs(lab=lab,
#              base_dem=base_dem,
#              node_length=365,breaks_hba1c=breaks_hba1c,
#              final_date=final_date)
# })
# , tar_target(wide_df_12mo, transform_long_to_wide(long_df=longitudinal_imputed_12mo,
#                                                    N_time=6,
#                                                    base_dem=baseline_imputed,
#                                                    long_covariates=long_covariates,
#                                                    labdata=hba1c_wide_12mo))
# ,   tar_target(tmle_spec_TCP1_12mo, subset_and_specify_analysis(data=wide_df_12mo,
#                                                                N_time=6,
#                                                                Avars="glp1_",
#                                                                Yvars="event_dementia_",
#                                                                Cvars="censor_dem_",
#                                                                yr=2009,
#                                                                baseline_vars=baseline_vars,
#                                                                long_covariates=long_covariates))
# ,   tar_target(ltmle_results_TCP1_12mo, ltmle_analysis(spec_ltmle=tmle_spec_TCP1_12mo,
#                                                        abar_spec = list(rep(1,5),rep(0,5)),
#                                                        SL.library="SL.glmnet",
#                                                        det.Q.function=NULL,
#                                                        varmethod=varmethod,
#                                                        Nsub=N_tmle_subset))
#
#
#### AIC as an L covariate, Using baseline only, TCP1 only, subset to has measurement
##among those with one
,tar_target(results_tcp1_BLa1c,{

  dat <- wide_df[!is.na(hba1c365_0),]
  spec_BLa1c <- subset_and_specify_analysis(data=dat,
                                            Avars="glp1_",
                                            Yvars="event_dementia_",
                                            Cvars="censor_dem_",
                                            yr=2009,
                                            baseline_vars=c(baseline_vars,"hba1c365_0"),
                                            long_covariates=c(long_covariates,"any_second_line_"),
                                            N_time=N_time)


  results <- ltmle_analysis(spec_ltmle=spec_BLa1c,
                 SL.library="SL.glmnet",
                 abar_spec = list(rep(1,(N_time-1)),rep(0,(N_time-1))),
                 det.Q.function=NULL,
                 varmethod=varmethod,
                 Nsub=N_tmle_subset)
  return(results)
})





## CLEAN AND EXPORT sensitivity RESULTS
,tar_target( table_sens_results,
{
    r7 <- clean_results(fit=ltmle_results_TCP_detQ, TCP="TCP1", analysis="DetQ_IC", outcome="dementia")
    # r8 <- clean_results(fit=ltmle_results_TCP_ic, TCP="TCP1", analysis="IC", outcome="dementia")
    r9 <- clean_results(fit=ltmle_results_TCP1_2011, TCP="TCP1", analysis="subset 2011", outcome="dementia")
    r10 <- clean_results(fit=ltmle_results_TCP1_3mo, TCP="TCP1",analysis="3mo nodes",outcome="dementia")
    r11 <- clean_results(fit=ltmle_results_TCP1_12mo, TCP="TCP1",analysis="12mo nodes",outcome="dementia")



    results <- rbind(r7,r9,r10,r11)
    fwrite(results,file=paste0("./export/table_sensitivity_results_dementia_",todaydate,"_",varmethod,"_.txt"))
})

### Stochastic regimes
##(ADD HERE)
### VISUALIZATIONS ---------------------------------------------------------------

##############################
##############################
### BOOTSTRAP ################
##############################
##############################
# ,tar_target(boot_results,{
#   ncores <- 2#floor(detectCores()/2)
#   mycluster <- parallel::makeCluster(ncores)
#   doSNOW::registerDoSNOW(cl=mycluster)
#   library(foreach)
#   boot_res <- foreach::foreach(1:ncores, .combine="rbind",
#                                 .errorhandling = "remove", .packages = c("SuperLearner","ltmle","tidyverse","matrixStats","foreach")) %do% {
#     boot_fun(spec_ltmle=spec_ltmle,
#              abar_spec=abar_spec,
#              SL.library=SL.library,
#              det.Q.function=det.Q.function,
#              package_stub=package_stub
#              ,SuperLearner_override=SuperLearner_override
#              ,Estimate_override=Estimate_override)}
#   # data.table::fwrite(data.frame(boot_res),file=paste0("./export/boot_res.txt"))
#
#  return(boot_res)
#   # if(make_boot==T){
#     set.seed(235569)
#   # ncores <- 4#floor(detectCores()/2)
#   # mycluster <- parallel::makeCluster(ncores)
#   # doSNOW::registerDoSNOW(cl=mycluster)
#      boot_res_iteration <- foreach(1:ncores, .combine="rbind",
#                                    .errorhandling = "pass", .packages = c("SuperLearner","ltmle","tidyverse","matrixStats")) %dopar% {
#         boot_fun(spec_ltmle=tmle_spec_TCP1,
#                abar_spec=list(rep(1,(N_time-1)),rep(0,(N_time-1))),
#                SL.library="glm",
#                det.Q.function=det.Q.function)
#                                    }
    # res <- bootstrap_tmle(spec_ltmle=tmle_spec_TCP1,
    # abar_spec=list(rep(1,(N_time-1)),rep(0,(N_time-1))),
    # SL.library="glm",
    # ncores=ncores,
    # det.Q.function=det.Q.function)
    # return(boot_res_iteration)
  # }else{
    # return("bootstrap=F")
  # }
#
# })
#
)
