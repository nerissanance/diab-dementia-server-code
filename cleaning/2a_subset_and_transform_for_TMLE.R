sink(file="./1_data_cleaning/2a_subset_and_transform_for_TMLE.Rout",append=F)
Sys.Date()
# Created: 2021-10-12
# Author(s): zyp6582 - Christian Torp-Pedersen
# Input files: A series of preprocessed files from working directory defined
#   below.
# Output files: A longitudinal data.table with covariates and datasets 
#                prepared  for LTMLE
#
# Data in input files:
# drugdiab has for each class of diabetes drug 
#   pnr is patient identifier
#   drugdiab is hypoglycermic class (character)
#   start/end is start end end of each treatment per prescription
#   value=1 - indicating that treatment is given
# n_metformin defines first secondary treatment
#   pnr is patient identifier
#   first_other name of class of first other treatment 
#   secdate is date of  first other treatment following metformin
# pop
#   pnr is patient identifier
#   fdato is date of birth
#   first_ind   last_ind first_ud last_ud are dates of first entry, first exit
#       last entry and last exit from the country
#   ie_type 0=Dane 2=immigrant 3= descentent of an immigrant
#   opr_land is code for country of origin
#   bef_year is last year with data indicating real residence - not relevant
#   sex 0=female 1=male
#   bop_vfra seneste_indvandring familie_id e_faelle_id - are not used
# diag are all diagnoses of the diabetes population within selected range. 
#    Currently both inpatient and out  patient diagnoses are accepted
#   pnr is patient identifier
#   diag is ICD code for condition
#   inddto is date of admission
#   pattype is not used. Zero identifies hospital admission, but only with 
#     the older version of patient register.
# charlson is a list of two data.tables
#   charlsoncode has pnr and charlson index for all with an index above zero
#   charlsoncomp has the components of of charlson index with 1 indicating presence.
#   Charlson is calculated at a specific date, currently first ever secondary
#   treatment.  It needs to be recalculated for other times.
# cov are selected covariates by diagnosis the dataset includes pnr and
#   first date of the following conditions: "any.malignancy", "chronic.pulmonary.disease", 
#   "dementia", "heart.failure","hypertension","ischemic.heart.disease",
#   "myocardial.infarction", "renal.disease" 
#   Note: Dementia and hypertension is also defined by medication in the dataset
# doede - Death registration. Includes pnr and doddato=date of death.
#   Until end of 2018 it also includes specific causes of death as:
#   dod_AMI, dod_stroke and dod_cv (cardiovascular) with "1" indicating presence
# lab - All laboratory for diabetes population
#   class - CREA, HBA1C, T-chol, LDL-chol, HDL-chol
#   res - value,  
#   start - date 
# edu - Education level each year
#   pnr - patient identifier
#   year - calender year
#   code5txt - five classes of education
# income - income each year
#   pnr - patient identifier
#   year
#   indiv_indk_index - indexed individual income
# household - Number of people in hourhold each year
#   pnr - patient identifier
#   year
#   household - number of people
######################################################################

rm(list=ls())
source(here::here("0_config.R"))

setwd('z:/Workdata/706582/christiantorp-pedersen/Diabetes/data')

######################################################################
#
# Baseline data 
# pnr                      : cpr-number
# first_other              : first other diabetes medication
# secdate                  : date of first other
# fdato                    : birthdate
# first_ind                : first immigration into DK
# last_ind                 : last immigration into DK
# first_ud                 : first immigration out of DK
# last_ud                  : last immigration out of DK
# ie_type                  : Resident type 1=DK, 2=other 3=child of other
# opr_land                 : Country of origin
# sex                      : 0=female, 1=male
# any.malignancy           : date
# chronic.pulmonary.disease: Date
# dementia                 : Date
# heart.failure            : Date
# hypertension             : Date
# ischemic.heart.disease   : Date
# myocardial.infarction    : Date
# renal.disease            : Date
# doddato                  : Date
# dod_ami                  : 1 for AMI death until 2018
# dod_stroke               : 1 for stroke death until 2018
# dod_cv                   : 1 for CV-death until 2018
# crea_base                : creatinine at baseline
# age_base                 : Baseline age
# egfr_base                : Baseline eGFR, MDRD formula
######################################################################
n_metformin <- readRDS('n_metformin.rds')
pop <- readRDS('pop.rds')
baseline <- merge(n_metformin,pop[,.(pnr,fdato,first_ind,last_ind,first_ud,last_ud,ie_type,opr_land,sex)])
baseline[,age_base:=as.numeric((secdate-fdato)/365.25)]
# Select age >=50 and year>=2009
baseline <- baseline[age_base>=50 & year(secdate)>=2009]

help <- baseline[,.(pnr,secdate)]
help[,year:=year(secdate)]
# income by quartiles at baseline
income <- readRDS('income.rds')
average_income <- averageIncome(baseline,income,c("pnr","secdate"),c("pnr","year","indiv_indk_index"))
average_income[,quartile_income:=cut(income,quantile(income,probs = 0:4/4),include.lowest = TRUE,
                                     labels=c("Income_q1","Income_q2","Income_q3","Income_q4"))]
average_income[,income:=NULL]
# Baseline education
edu <- readRDS('edu.rds')
edu <- merge(edu,help,by=c("pnr","year"))
edu[,secdate:=NULL]
# Baseline lab values
lab <- readRDS('lab.rds')
lab <- merge(lab,help,by="pnr")
lab <- lab[start<=secdate]
setkeyv(lab,c("pnr","class","start"))
lab <- lab[,.SD[.N],by=c("pnr","class")]
lab <- dcast(lab,pnr~class,value.var="res")
setnames(lab,c("CREA","HBA1C","HDL_chol","LDL_chol","T_chol"),c("CREA_base","HBA1C_base",
                                                                "HDL_chol_base","LDL_chol_base","T_chol_base"))
#Time dependent covariates
cov <- readRDS('cov.rds')
#dead
doede <- readRDS('doede.rds')

baseline <- Reduce(function(x,y){merge(x,y,all.x=TRUE,by="pnr")},
                   list(baseline,average_income,edu,lab,cov[,.(pnr,dementia)],doede))
baseline[,egfr_base:=175*(CREA_base/88.4)**-1.154*age_base**-0.203] # Only for about 100,000 people
baseline[sex==0,egfr_base:=egfr_base*0.742]
saveRDS(baseline,file='baseline.rds')

######################################################################
#
# Baseline data 
# pnr                      : cpr-number
# first_other              : first other diabetes medication
# secdate                  : date of first other
# fdato                    : birthdate
# first_ind                : first immigration into DK
# last_ind                 : last immigration into DK
# first_ud                 : first immigration out of DK
# last_ud                  : last immigration out of DK
# ie_type                  : Resident type 1=DK, 2=other 3=child of other
# opr_land                 : Country of origin
# sex                      : 0=female, 1=male
# age_base                 : Age at baseline
# quartile_income          : Baseline quartile of average income during 5 prior years
# code5txt                 : Five classes of maximal education at baseline
# CREA_base                : Creatinne prior to baseline, but closest
# HBA1C_base               : 
# HDL_chol_base            : 
# LDL_chol_base            : 
# T_chol_base              : 
# dementia                 : Date of dementia by medication or diagnosis when present
# doddato                  : Date of death
# dod_ami                  : 1 for AMI death until 2018
# dod_stroke               : 1 for stroke death until 2018
# dod_cv                   : 1 for CV-death until 2018
# crea_base                : creatinine at baseline
# egfr_base                : Baseline eGFR, MDRD formula
######################################################################
#
# Prepare for time dependent variables
#
######################################################################
base_dem <- baseline[is.na(dementia) | dementia>secdate] #dementia after start excluded
base_dem[,inn:=secdate] # start of first secondary treatment
base_dem[,out:=pmin(doddato,dementia,as.Date("2021-08-04"),na.rm=TRUE)] # last date
base_dem[,last_date:=out] #keep track of last date for def of censoring
base_dem <- base_dem[out>inn] # 18 patients died prior to picking up medication
base_dem[,dummy:=1] #necessary dummy for splitting


drugdiab <- readRDS('drugdiab.rds')
######################################################################
# drugdiab: name of drug - all classes of diabetes drugs
# pnr     : cpr_number
# start   : starting date
# end     : End date
# value   : Strength - alway "1" - for treatment given
######################################################################

cov <- readRDS('cov.rds')
######################################################################
# pnr                      : cpr number
# any.malignancy           
# chronic.pulmonary.disease
# dementia                 : Diagnosis or treatment date
# heart.failure            : 
# hypertension             : 
# ischemic.heart.disease   : 
# myocardial.infarction    : 
# renal.disease            : 
######################################################################

#make sure all datasets have the right class of merge ID variable
base_dem$pnr <- as.numeric(base_dem$pnr)
cov$pnr <- as.numeric(cov$pnr)
drugdiab$pnr <- as.numeric(drugdiab$pnr)

#save base_dem with correct ID class
saveRDS(base_dem,file=here::here("data/base_dem.rds"))


# Select part of base_dem that needs splitting
# Keep only variables defining intervals and dates that are part of splitting - plus dummy! and dementia
base_dem_split <- base_dem[,.(pnr,inn,out,last_date,dummy,fdato,doddato,secdate,dementia)]
# Split by time of covariates
time_dem <- lexisTwo(base_dem_split,cov,invars = c("pnr","inn","out","dummy"),
                     splitvars = c('any.malignancy','chronic.pulmonary.disease','heart.failure','hypertension',
                                   'ischemic.heart.disease','myocardial.infarction','renal.disease'))
#Split by diabetes treatment
time_dem <- lexisFromTo(time_dem,drugdiab,invars = c("pnr","inn","out","dummy"),
                        splitvars=c("pnr","start","end","value","drugdiab"))
# Split by calender time
time_dem <- lexisSeq(time_dem,invars = c("pnr","inn","out","dummy"),
                     varname = NULL,
                     splitvector = as.Date(c("1995-01-01","2015-01-01","2022-01-01")),
                     value="calender",format="vector")
# Split in 6 month periods since baseline
time_dem <- lexisSeq(time_dem,invars = c("pnr","inn","out","dummy"),
                     varname="secdate",splitvector = c(0,25*365,180),
                     format="seq",value="splittime")       
# Split in 5 year age intervals
time_dem <- lexisSeq(time_dem,invars = c("pnr","inn","out","dummy"),
                     varname="fdato",splitvector=c(0,100*365,5*365),
                     format="seq",value="age")
time_dem[,age:=age*5]
# Outcome node
time_dem[,event_dementia:=0]
time_dem[!is.na(dementia) & dementia>=inn & dementia<=out,event_dementia:=1]
time_dem[,event_dementia:=max(event_dementia),by=c("pnr","splittime")]


# Outcome node - death
time_dem[,event_death:=0]
time_dem[!is.na(doddato) & doddato>=inn & doddato<=out,event_death:=1]
time_dem[,event_death:=max(event_death),by=c("pnr","splittime")]

# Censoring node
time_dem[,censor:=0]
time_dem[out==last_date & event_dementia !=1, censor:=1]
time_dem[,censor:=max(censor),by=c("pnr","splittime")]

###############################################################
#
# Add empty records so that all individuals complete records
#
###############################################################
setkeyv(time_dem,c("pnr","splittime"))
pnr2 <- unique(time_dem[,pnr])
splittime <- unique(time_dem[,splittime])
pnr3 <- data.table(pnr=rep(pnr2,each=length(splittime)),splittime=rep(0:(length(splittime)-1),length(pnr2)))
time_dem <- merge(time_dem,pnr3,by=c("pnr","splittime"),all=TRUE)

#make sure data is sorted correctly
time_dem <- time_dem %>% arrange(pnr, splittime)
head(time_dem)

#imputation checking
# d <- time_dem %>% filter(pnr==1092351 | pnr==1216776  ) %>% select(pnr, splittime, inn, event_dementia)
# df<-d
# d[,event_dementia:=nafill(event_dementia,type="locf")]


#save non-imputed dementia and death for tabulation
time_dem$first_dementia <- time_dem$event_dementia
time_dem$first_death <- time_dem$event_death

# Fill event with 1 and censor with 1
setkeyv(time_dem,c("pnr","splittime"))
time_dem[,event_dementia:=nafill(event_dementia,type="locf")]
time_dem[,event_death:=nafill(event_death,type="locf")]
time_dem[,censor:=nafill(censor,type="locf")]

#save long-form data
saveRDS(time_dem,file=here::here("data/time_dem_long.rds"))
sink()
