
rm(list=ls())
library(here)
source(here::here("0_config.R"))

#-----------------------------------
#-----------------------------------

# NOTE: edit these hyperparameters or inputs as needed

yr = 2011 #year to start cohort definition (depending on which drugs)
N_time = 11 #number of time points you want to look at
ncores = 10 #number of cores to use



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
## UNCOMMENT FOR RUNNING MANUAL COMPETING RISK FIX
## edit--when one occurs first, set other to zero so there's no competing event:
dementia.nodes<- grep("event_dementia_",names(d))
death.nodes<- grep("event_death_",names(d))
d[, sum_death :=rowSums(.SD,na.rm=T), .SDcols = death.nodes]
d[, sum_dementia :=rowSums(.SD,na.rm=T), .SDcols = dementia.nodes]
table(d$sum_death)
table(d$event_dementia_10)
table(d$sum_dementia)
d[sum_death > sum_dementia, (dementia.nodes) := replace(.SD, .SD == 1, 0), .SDcols = dementia.nodes]
d[sum_death < sum_dementia, (death.nodes) := replace(.SD, .SD == 1, 0), .SDcols = death.nodes]
# NOTE: decided to prioritize dementia in the event that both death and dementia occur in the same time bin
d[sum_death== sum_dementia, (death.nodes) := replace(.SD, .SD == 1, 0), .SDcols = death.nodes]
table(d$event_dementia_10)


table(d$glp1_2, d$event_dementia_10)
table(d$glp1_3, d$event_dementia_10)
prop.table(table(d$glp1_0, d$event_dementia_10),1) *100
prop.table(table(d$glp1_1, d$event_dementia_10),1) *100
prop.table(table(d$glp1_2, d$event_dementia_10),1) *100
prop.table(table(d$glp1_3, d$event_dementia_10),1) *100
prop.table(table(d$glp1_4, d$event_dementia_10),1) *100
prop.table(table(d$glp1_5, d$event_dementia_10),1) *100
prop.table(table(d$glp1_6, d$event_dementia_10),1) *100
prop.table(table(d$glp1_7, d$event_dementia_10),1) *100
prop.table(table(d$glp1_8, d$event_dementia_10),1) *100
prop.table(table(d$glp1_9, d$event_dementia_10),1) *100
prop.table(table(d$glp1_10, d$event_dementia_10),1) *100
