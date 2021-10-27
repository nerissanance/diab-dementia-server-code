# Created: 2021-09-26
# Author(s): zyp6582 - Christian Torp-Pedersen
# Input files: 
# This program uses first datasets located: x:/Data/Rawdata_Hurtig/706582
#  and this represents data until and including 2018. Later datasets were
#  delivered for Covid projects and are mostly found in the latest delivery:
#  V:/Data/Workdata/706582/Corona_update/Data/11. levering. The hospital registry
#  changed to a new version during 2019 and the final delivery of the older 
#  version is from: V:/Data/Workdata/706582/Corona_update/Data
# Output files: For flexibility all files with detailed data are saved and
# the final part of the program creates a longitudinal version of baseline
# data. Preparation for LTMLE is a separate program
#
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
#   class - CREA, HBA1C, T_chol, LDL_chol, HDL_chol
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
#   
######################################################################
library(data.table)
library(heaven)
setwd('z:/Workdata/706582/christiantorp-pedersen/Diabetes/data')
# Extra working version of importSAS named isas
#source('Z:/Workdata/706582/christiantorp-pedersen/importSAS_til_TAG/isas_CTP.R')

######################################################################
#
# Diabetes population
# For the current study the diabetes population is defined simply as
# the first date of any use of a hypoglycemic drug (A10). Since secondary
# therapy guarantees real type 2 diabetes no efforts or used to deal with
# polycystic ovarian syndrome or pregnancy  related diabetes
# The diabetes  population is identified first to east the burden of other
# variables focusing only  on diabetes patients.
#
######################################################################
A10_1 <- importSAS('x:/Data/Rawdata_Hurtig/706582/lmdb.sas7bdat', 
                   date.vars = "eksd", where="atc=:'A10'")
A10_2 <- importSAS('V:/Data/Workdata/706582/Corona_update/Data/11. levering/lmdb.sas7bdat', 
                   date.vars = "eksd", where="atc=:'A10'")

A10 <- rbind(A10_1[eksd<=as.Date("2018-12-31")],A10_2,fill=TRUE)
rm(A10_1)
rm(A10_2)
gc()
# The following named list identifies all types of hypoglycemic therapy
# Note  that many compounds are not  only identified as singular compounds,
# but also as part of combination therapy drugs.  Each entry is the "start" of
# the code defining a drug
diab <- list(
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
)
drugdiab <- findCondition(A10,"atc",c("pnr","eksd"),diab,match="start")
f_metformin <- drugdiab[X=="metformin"]
setkeyv(f_metformin,c("pnr","eksd"))
f_metformin <- f_metformin[,.SD[1],by="pnr"]
setnames(f_metformin,"eksd","metd")
f_metformin <- f_metformin[,.(pnr,metd)]
n_metformin <-drugdiab[X!="metformin"]
n_metformin <- merge(n_metformin,f_metformin,by="pnr")
n_metformin <- n_metformin[eksd>=metd]
setkeyv(n_metformin,c("pnr","eksd"))
n_metformin <- n_metformin[,.SD[1],by="pnr"]
n_metformin <- n_metformin[,.(pnr,X,eksd)]
pnr <- n_metformin[,.(pnr)]
setnames(n_metformin,c("X","eksd"),c("first_other","secdate"))
# n_metformin holds date of first secondary treatment (secdate) and the secondary treatment - 
# only for those starting secondary treatment.

# Prepare drugdiab for splitting in time intervals. The splitting program
# requires that there are no time overlaps within a single drug class. The
# use currently is to let any prescription identify treatment during the next 
# six months OR until there is another prescription.
setnames(drugdiab,c("X","eksd"),c("drugdiab","start"))
setkeyv(drugdiab,c("pnr","drugdiab","start"))
drugdiab[,end:=start+180]
drugdiab[,end2:=shift(start,type="lead"),by=c("pnr","drugdiab")]
drugdiab[,end:=pmin(end,end2,na.rm=TRUE)]
drugdiab[,end2:=NULL]
drugdiab[,value:=1]
saveRDS(drugdiab,file='drugdiab.rdss')
saveRDS(n_metformin,file='n_metformin.rds')
######################################################################
#
# Input Danish population
#
######################################################################
pop1 <- importSAS('x:/Data/Rawdata_Hurtig/706582/pop.sas7bdat', 
                  date.vars = c("fdato","first_ind","last_ind","first_ud","last_ud"),
                  drop="quarter",where="bef_year ne .",filter = pnr)
pop2 <- importSAS('V:/Data/Workdata/706582/Corona_update/Data/pop_9march2020.sas7bdat',
                  date.vars = c("fdato","bop_vfra","seneste_indvandring"),
                  drop=c("e_faelle_id","familie_type"),filter = pnr)
pop3 <- importSAS('V:/Data/Workdata/706582/Corona_update/Data/11. levering/pop_20jun2021.sas7bdat',
                  date.vars = c("fdato","bop_vfra","seneste_indvandring"),filter = pnr)
pop <- rbind(pop1,pop2,pop3,fill=TRUE)
setkey(pop,"pnr")
pop <- pop[,.SD[1],by="pnr"]
rm(pop1,pop2,pop3)
gc()
saveRDS(pop,file="pop.rds")

######################################################################
#
# Diagnoses for outcomes, exclusion and covariates
#
######################################################################

diag1 <- importSAS('x:/Data/Rawdata_Hurtig/706582/diag_indl.sas7bdat', 
                   date.vars = "inddto", keep=c("pnr","diag","inddto","pattype"),
                   where="(substr(diag,1,1) in ('0','1','2','4','5')
                         or substr(diag,1,2) in ('DI','DG','DH','DJ','DM','DB','DK','DE','DN','DC'))
                         and pattype=0",filter = pnr)
diag2 <- importSAS('V:/Data/Workdata/706582/Corona_update/Data/diag_indl_9march2020.sas7bdat',
                   date.vars = "inddto", keep=c("pnr","diag","inddto","pattype"),
                   where="substr(diag,1,2) in ('DI','DG','DH','DJ','DM','DB','DK','DE','DN','DC')
                         and pattype='0'",filter = pnr)
diag3 <- isas('V:/Data/Workdata/706582/Corona_update/Data/11. levering/diag_indl_lpr3.sas7bdat',
              where="substr(diag,1,2) in ('DI','DG','DH','DJ','DM','DB','DK','DE','DN','DC')", filter=pnr,
              keep=c("pnr","diag","inddto","starttidspunkt","sluttidspunkt"))
diag3 <- diag3[(sluttidspunkt-starttidspunkt)/(60*60)>12] 
diag3[,c("starttidspunkt","sluttidspunkt"):=NULL]
diag3[,dif:=NULL]
diag3[,inddto:=as.Date(inddto,format="%Y%m%d")]
diag <- rbind(diag1,diag2,diag3,fill=TRUE)
rm(diag1, diag2, diag3)
gc()
saveRDS(diag,file="diag.rds")
######################################################################
#
# Charlson index and components at time of entry
# looking back 5 years - currently from first ever secondary treatment
#
######################################################################
diag <- merge(diag,n_metformin[.(pnr,secdate)],by="pnr")
charlson <- charlsonIndex(diag,ptid='pnr',vars='diag',data.date='inddto',charlson.date='secdate')
charlsoncode <- charlson[[1]]
charlsoncomp <- charlson[[2]]
saveRDS(charlson,file='charlson.rds')
######################################################################
#
# AMI - dementia - and other covariates - defined by first date
#
######################################################################
covariates <- c(charlson.codes["heart.failure"],
                charlson.codes["renal.disease"],charlson.codes["chronic.pulmonary.disease"],
                charlson.codes["dementia"],charlson.codes["any.malignancy"])
covariates <- c(covariates,list(
  ischemic.heart.disease=c("411","412","413","414","I20","I23","I24","I25"),
  myocardial.infarction=c("410","I21"),
  hypertension=c(paste0('40',1:4),'41009','41099','I10','I109','I11','I110','I119','I119A',         
                 'I12','I120','I129','I129A','I13','I130','I131','I132','I139','I15','I150','I151','I152','I158','I159')
))

cov <- findCondition(diag,"diag",c("pnr","inddto"),covariates,match="start")
setkeyv(cov,c("pnr","X","inddto"))
cov <- cov[,.SD[1],by=c("pnr","X")]
cov <- dcast(cov,pnr~X,value.var="inddto")
#saveRDS(cov,file='cov.rds') Only saved when medication is also added
######################################################################
#
# Death
#
######################################################################
doede <- importSAS('X:/Data/Rawdata_Hurtig/706582/doede.sas7bdat',
                   keep=c("pnr","doddato","dod_ami","dod_cv","dod_stroke"),date.vars = "doddato")
doede_ny <- importSAS('V:/Data/Workdata/706582/Corona_update/Data/11. levering/doede.sas7bdat',
                      date.vars = "doddato")
doede <- rbind(doede,doede_ny,fill=TRUE)
for(x in c("dod_ami","dod_stroke","dod_cv"))
  doede[get(x)=='.',(x):=NA]
saveRDS(doede,file='doede.rds')
######################################################################
#
# Hypertension medication - and dementia medication
#
######################################################################
c1 <- importSAS('x:/Data/Rawdata_Hurtig/706582/lmdb.sas7bdat', 
                date.vars = "eksd", where="atc=:'C'or atc=:'N06D'",filter=pnr)
c2 <- importSAS('V:/Data/Workdata/706582/Corona_update/Data/11. levering/lmdb.sas7bdat', 
                date.vars = "eksd", where="atc=:'C' or atc=:'N06D'",filter=pnr)
c12 <- rbind(c1,c2,fill=TRUE)
c12 <- c12[,.(pnr,eksd,atc)]
hyp <- hypertensionMedication(c12,vars=c("pnr","atc","eksd"))
hyp <- hyp[,.(pnr,hypertension)]
setnames(hyp,"hypertension","hypertension_med")

dementia_med <- c12[grepl("^N06D",atc)]
setkeyv(dementia_med,c("pnr","eksd"))
dementia_med <- dementia_med[,.SD[1],by="pnr"]
dementia_med <- dementia_med[,.(pnr,eksd)]
setnames(dementia_med,"eksd","dementia_med")
#adjustment of cov
cov <- Reduce(function(x,y){merge(x,y,all=TRUE,by="pnr")},list(cov,dementia_med,hyp))
cov[,hypertension:=pmin(hypertension,hypertension_med,na.rm = TRUE)]
cov[,dementia:=pmin(dementia,dementia_med,na.rm=TRUE)]
cov[,c("dementia_med","hypertension_med"):=NULL]
saveRDS(cov,file='cov.rds')
######################################################################
#
# Hb1AC - creatinine - cholesterol
#
######################################################################
# Kbh amt - cholesterol difficult, old data
hbc1 <- importSAS('X:/Data/Rawdata_Hurtig/706582/blodprove_kbhamt.sas7bdat',
                  where="testcode=:'HBA1C' or testcode=:'CREA' or batterycode=:'CHOL' or batterycode=:'LDL' or batterycode=:'HDL'",
                  keep=c("pnr","batterycode","testcode","testresult","collectdate","result_normal_range"),
                  character.vars = c("pnr","testresult","result_normal_range"), filter=pnr)
hbc1[,testresult:=as.numeric(gsub('>','',testresult))]
hbc1 <- hbc1[!is.na(testresult)]
hbc1[,start:=as.Date(collectdate)]
hbc1[grepl('^CREA',testcode),class:='CREA']
hbc1[grepl('^CHOL',testcode),class:='T_chol']
hbc1[grepl('^LDL',testcode) & !grepl('LDLED',testcode),class:='LDL_chol']
hbc1[grepl('^HDL',testcode) ,class:='HDL_chol']
hbc1[grepl('^HBA1C',testcode),class:='HBA1C']
hbc1[class=='HBA1C' & testresult<1,testresult:=testresult*100]
hbc1[class=='HBA1C',testresult:=testresult*10.93-23.5]
hbc1 <- hbc1[!is.na(class)]
hbc1 <- hbc1[,.(pnr,class,testresult,start)]
setnames(hbc1,"testresult","res")

# KPLL - strange numbers for hba1c - diabetic patients with hba1c with peak at 4
hbc2 <- importSAS('X:/Data/Rawdata_Hurtig/706582/blodprove_kpll.sas7bdat',
                  where = "col3='CR' or col3 in ('CHO','LDL','HDL')",filter=pnr)
hbc2[,res:=as.numeric(gsub(',','.',col5))]
hbc2[,start:=as.Date(col2,format="%d-%m-%Y")]
hbc2[col3=='CR',class:='CREA']
hbc2[col3=='CHO',class:='T_chol']
hbc2[col3=='LDL',class:='LDL_chol']
hbc2[col3=='HDL',class:='HDL_chol']
hbc2 <- hbc2[,.(pnr,class,res,start)]

#Region Nord
hbc3 <- isas('X:/Data/Rawdata_Hurtig/706582/blodprove_nord0607.sas7bdat',filter=pnr)
hbc4 <- isas('X:/Data/Rawdata_Hurtig/706582/blodprove_nord0809.sas7bdat',filter=pnr)
hbc5 <- isas('X:/Data/Rawdata_Hurtig/706582/blodprove_nord1213.sas7bdat',filter=pnr)
hbc345 <- rbind(hbc3,hbc4,hbc5)
nord <- importSAS('X:/Data/Rawdata_Hurtig/706582/analyser_labkaii.sas7bdat',character.vars = "analyse_id")
hbc345 <- merge(hbc345,nord,by="analyse_id")
hbc345[,resultat:=gsub('>','',resultat)]
hbc345[,res:=as.numeric(gsub(",",".",resultat))]
hbc345[,start:=as.Date(prv_datoklok)]
hbc345[analysenavn=='P-Creatinin',class:='CREA']
hbc345[grepl('DCCT',component)|grepl('IFCC',component),class:='HBA1C']
hbc345[grepl('DCCT',component) & resultat<1,resultat:=res*100]
hbc345[grepl('DCCT',component),resultat:=res*10.93-23.5]
hbc345[npucode=='NPU01566',class:='T_chol']
hbc345[npucode=='NPU01568',class:='LDL_chol']
hbc345[npucode=='NPU01567',class:='HDL_chol']
hbc345 <- hbc345[,.(pnr,class,res,start)]
hbc345 <- hbc345[!is.na(class)]

hbc6 <- importSAS('X:/Data/Rawdata_Hurtig/706582/blodprove_nordfinal.sas7bdat',
                  where="component='Hemoglobin A1c' or component='Creatinin' or npucode=:'NPU0156'"
                  ,filter=pnr)
hbc6[,res:=as.numeric(result)]
hbc6[,start:=as.Date(sampledate,format="%d.%m.%Y")]
hbc6[component=='Creatinin',class:='CREA']
hbc6[component=='Hemoglobin A1c',class:='HBA1C']
hbc6[class=='HBA1C' & res<1,res:=res*100]
hbc6[class=='HBA1C',res:=res*10.93-23.5]
hbc6[npucode=='NPU01566',class:='T_chol']
hbc6[npucode=='NPU01568',class:='LDL_chol']
hbc6[npucode=='NPU01567',class:='HDL_chol']
hbc6 <- hbc6[,.(pnr,class,res,start)]

#Roskilde
hbc7 <- importSAS('X:/Data/Rawdata_Hurtig/706582/blodprove_roskilde.sas7bdat',
                  where="analyse in('HBA1C','CREA','CHOL','LDL','HDL')",filter=pnr)
hbc7[,resultat:=as.numeric(gsub('=','',resultat))]
hbc7[,start:=as.Date(dato,format="%d%B%y")]

hbc7[analyse=='CREA',class:='CREA']
hbc7[analyse=='HBA1C',class:='HBA1C']
hbc7[analyse=='HBA1C' & resultat<1,resultat:=resultat*100]
hbc7[analyse=='HBA1C',resultat:=resultat*10.93-23.5]
hbc7[analyse=='CHOL',class:='T_chol']
hbc7[analyse=='LDL',class:='LDL_chol']
hbc7[analyse=='HDL',class:='HDL_chol']
hbc7 <- hbc7[,.(pnr,class,resultat,start)]
setnames(hbc7,"resultat","res")

#Labka
hbc8 <- importSAS('X:/Data/Rawdata_Hurtig/706582/lab_dm_forskerny.sas7bdat',
                  where="analysiscode  in ('NPU27300','NPU03835','NPU02307','NPU18016','NPU18105','NPU01807',
      'NPU01566','NPU01567','NPU01568')",
                  filter=pnr)

hbc9 <- importSAS('X:/Data/Rawdata_Hurtig/706582/Data til COVID-19 studier/11_lev/lab_forsker.sas7bdat',
                  where="analysiscode  in ('NPU27300','NPU03835','NPU02307','NPU18016','NPU18105','NPU01807',
      'NPU01566','NPU01567','NPU01568')",
                  character.vars="patient_cpr")
setnames(hbc9,"patient_cpr","pnr") 
hbc9 <- merge(hbc9,pnr,by="pnr")
hbc89 <- rbind(hbc8,hbc9)
hbc89[,res:=as.numeric(value)]
setnames(hbc89,"samplingdate","start")

hbc89[analysiscode %in% c('NPU18016','NPU18105','NPU01807'),class:='CREA']
hbc89[analysiscode %in% c('NPU27300','NPU03835','NPU02307'),class:='HBA1C']
hbc89[analysiscode=='NPU03835' & res<1,res:=res*100]
hbc89[analysiscode=='NPU03835', res:=res*10.93-23.5]
hbc89[class=='HBA1C' & res<20, res:=NA]
hbc89[analysiscode=='NPU01566',class:='T_chol']
hbc89[analysiscode=='NPU01568',class:='LDL_chol']
hbc89[analysiscode=='NPU01567',class:='HDL_chol']
hbc89 <- hbc89[,.(pnr,class,res,start)]

lab <- rbind(hbc1,hbc2,hbc345,hbc6,hbc7,hbc89)
lab <- lab[!is.na(res)]
lab <- unique(lab)
saveRDS(lab,file='lab.rds')

rm(hbc1,hbc2,hbc3,hbc4,hbc5,hbc345,hbc6,hbc7,hbc8,hbc9,hbc89,nord)
gc()

######################################################################
#
# Education - available until 2018
# Missing education assume to be primary
#
######################################################################

edu <- importSAS('X:/Data/Rawdata_Hurtig/706582/uddan.sas7bdat',filter=pnr,
                 keep=c('pnr','hfaudd','year'))
edu <- merge(edu,edu_code,by="hfaudd")
edu <- edu[,.(pnr,year,code5txt)]
saveRDS(edu,file='edu.rds')

######################################################################
#
# Income - available until 2018
#
# The right income to use has to be checked - currently use individual income
#
######################################################################

income <- importSAS('X:/Data/Rawdata_Hurtig/706582/husstandsindk.sas7bdat',
                    filter=pnr, keep=c('pnr','year','indiv_indk_index'))
saveRDS(income,file='income.rds')

######################################################################
#
# Household size 
#
######################################################################

filelist <- paste0('bef',1994:2020,'12.sas7bdat')
household <- rbindlist(lapply(filelist,function(x){
  #browser()
  dat <- importSAS(paste0('X:/Data/Rawdata_Hurtig/706582/Data til COVID-19 studier/',x),
                   keep=c("pnr","familie_id"))
  setkey(dat,"familie_id")
  dat[,year:=as.numeric(substr(x,4,7))]
  dat[,household:=.N,by="familie_id"]
  dat <- merge(dat,pnr,by="pnr")
  dat[,familie_id:=NULL]
  dat
}))
saveRDS(household,file='household.rds')
