

rm(list=ls())
library(lava)
library(tidyverse)
library(data.table)
source(paste0(here::here(),"/synthesizeDD.R"))

cc <- fread(paste0(here::here(),"/data/coefficients.txt"))


#Remove Y-A association and mediating associations through L
cc<-as.data.frame(cc)
cc[,grepl("glp1_",colnames(cc))] <- NA


u <- synthesizeDD(cc)

set.seed(12345)
sim_list <- NULL
n<-200
N_time=10


for(i in 1:n){
  cat("\ni: ",i,"\n")

  d <- sim(u,115698)

  #note: once events jump to 1, need to remain 1 for remainder of follow up
  for(k in 1:(N_time+1)){
    j=k+1
    d[get(paste0("event_dementia_",k))==1, (paste0("event_dementia_",j)):=1]
    d[get(paste0("event_death_",k))==1, (paste0("event_death_",j)):=1]
    d[get(paste0("event_death_",k))==1, (paste0("censor_",i)):=0]
    d[get(paste0("event_death_",k))==1, (paste0("censor_",j)):=1]
    d[get(paste0("censor_",k))==1, (paste0("censor_",j)):=1]

  }
  ## UNCOMMENT FOR RUNNING MANUAL COMPETING RISK FIX
  ## edit--when one occurs first, set other to zero so there's no competing event:
  dementia.nodes<- grep("event_dementia_",names(d))
  death.nodes<- grep("event_death_",names(d))
  d[, sum_death :=rowSums(.SD,na.rm=T), .SDcols = death.nodes]
  d[, sum_dementia :=rowSums(.SD,na.rm=T), .SDcols = dementia.nodes]

  d[sum_death > sum_dementia, (dementia.nodes) := replace(.SD, .SD == 1, 0), .SDcols = dementia.nodes]
  d[sum_death < sum_dementia, (death.nodes) := replace(.SD, .SD == 1, 0), .SDcols = death.nodes]
  # NOTE: decided to prioritize dementia in the event that both death and dementia occur in the same time bin
  d[sum_death== sum_dementia, (death.nodes) := replace(.SD, .SD == 1, 0), .SDcols = death.nodes]


  sim_list[[i]] <- d
  gc()
}

saveRDS(sim_list, paste0(here::here(),"/data/null_simulated_data_list.RDS"))

