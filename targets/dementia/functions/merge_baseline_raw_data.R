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
# danish_pop
#   pnr is patient identifier
#   birth_date is date of birth
#   first_ind   last_ind first_ud last_ud are dates of first entry, first exit
#       last entry and last exit from the country
#   ie_type 0=Dane 2=immigrant 3= descentent of an immigrant
#   opr_land is code for country of origin
#   bef_year is last year with data indicating real residence - not relevant
#   sex 0=female 1=male
#   bop_vfra seneste_indvandring familie_id e_faelle_id - are not used
# diag are all diagnoses of the diabetes danish_population within selected range. 
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
# lab - All laboratory for diabetes danish_population
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



######################################################################
#
# Baseline data 
# pnr                      : cpr-number
# first_other              : first other diabetes medication
# secdate                  : date of first other
# birth_date                    : birthdate
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

if(FALSE){
  danish_pop <- tar_read(raw_diabetes_cohort)
  tar_load(comorbidities)
  tar_load(household)
  tar_load(income)
  tar_load(edu)
  tar_load(lab)
  tar_load(region)
    
}
merge_baseline_raw_data <- function(danish_pop,
                                    comorbidities,
                                    household,
                                    income,
                                    edu,
                                    lab,
                                    region){

  
  # copy data to compare at end of file
  pop_cp <- copy(danish_pop$pnr)
  danish_pop[,year:=year(secdate)]
  
  # income by quartiles at baseline
  average_income <- averageIncome(danish_pop,income,c("pnr","secdate"),c("pnr","year","indiv_indk_index"))
  average_income[,quartile_income:=cut(income,quantile(income,probs = 0:4/4),include.lowest = TRUE,
                                       labels=c("Income_q1","Income_q2","Income_q3","Income_q4"))]
  average_income[,income:=NULL]
  # Baseline education
  edu <- merge(edu,danish_pop[,.(pnr,secdate, year)],by=c("pnr","year"))
  edu[,secdate:=NULL]
  ##add in 3-category education, grouping:
  ###Upper secondary with Vocational training 
  edu[,edu_4cat := dplyr::recode_factor(code5txt,
      "Upper secondary"="Upsec_vocational",
      "Vocational training"="Upsec_vocational"
  )]
  
  # Baseline lab values
  lab <- merge(lab,danish_pop[,.(pnr,secdate, year)],by="pnr")
  lab <- lab[start<=secdate]
  #clean out of range values
  lab <- lab[!(class=="HBA1C"&(res>150 |res<1)),]
  #TODO: clean other lab values
  
  setkeyv(lab,c("pnr","class","start"))
  #select msot recent lab prior to secdate
  lab <- lab[,.SD[.N],by=c("pnr","class")]
  

  
  lab <- dcast(lab,pnr~class,value.var="res")
  setnames(lab,c("CREA","HBA1C","HDL_chol","LDL_chol","T_chol"),c("CREA_base","HBA1C_base",
                                                                  "HDL_chol_base","LDL_chol_base","T_chol_base"))
  #drop year from sub-datsets
  #edu[,year:=NULL]

  danish_pop <- Reduce(function(x,y){merge(x,y,all.x=TRUE,by="pnr")},
                     list(danish_pop,average_income,edu,lab,
                          comorbidities[,.(pnr,dementia)]))
  danish_pop[,egfr_base:=175*(CREA_base/88.4)**-1.154*age_base**-0.203] # Only for about 100,000 people
  danish_pop[sex==0,egfr_base:=egfr_base*0.742]
  danish_pop[,year_2nd_line_start:=year(secdate)]
  
  
  ##Adding in region of Denmark by year of second line start
  
  setkeyv(region,c("pnr","year"))
  danish_pop2 <- merge(danish_pop,region, by=c("pnr","year"),all.x = T,all.y = F)
  if(nrow(danish_pop)!=nrow(danish_pop2))stop("issue with merge in merge_baseline_raw_data.R file")
  
  
  
  #make sure baseline is distinct
  ## dim(danish_pop) 
  ##danish_pop2 <- danish_pop2 %>% distinct() %>% as.data.table
  ##this shouldn't be happening anymore, fixed above
  ##adding extra check just in case
  if(length(unique(pop_cp))!=length(unique(danish_pop2$pnr)))stop("nrow input not equal to nrow output--double check data")
  if(length(unique(danish_pop2$pnr))!=length(danish_pop2$pnr))stop("pnrs not unique--issue with data")
  ## dim(danish_pop)
  return(danish_pop2)
  
}
  
