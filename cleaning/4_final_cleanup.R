# LAST cleaning file
# implementing manual fixes including
# 1) overwriting NA with 0 (needs to be fixed in data! this is temporary fix)
# 2) manual overwrite of competing event (death vs dementia)

d <- readRDS(file=here("data/simdata_10k.rds"))
# d <- readRDS(file=here("data/time_dem_wide.rds"))


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


saveRDS(d,file=here("data/data_clean.rds"))
