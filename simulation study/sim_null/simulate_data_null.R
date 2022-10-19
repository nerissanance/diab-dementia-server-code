

rm(list=ls())
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))
source(paste0(here::here(),"/synthesizeDD.R"))

cc <- fread(paste0(here::here(),"/data/coefficients.txt"))
cc <- as.data.frame(cc)


#Remove Y-A association and mediating associations through L
cc[!grepl("glp1_",cc$var),grepl("glp1_",colnames(cc))] <- NA
#Check glp1 still predicts itself
cc[grepl("glp1_",cc$var),grepl("glp1_",colnames(cc))]


#make outcome more common
cc[cc$var=="event_dementia_1",colnames(cc)=="(Intercept)"] <- cc[cc$var=="event_dementia_1",colnames(cc)=="(Intercept)"] + 1
cc[cc$var=="event_dementia_2",colnames(cc)=="(Intercept)"] <-  cc[cc$var=="event_dementia_2",colnames(cc)=="(Intercept)"] + 1
cc[cc$var=="event_dementia_3",colnames(cc)=="(Intercept)"] <-  cc[cc$var=="event_dementia_3",colnames(cc)=="(Intercept)"] + 1
cc[cc$var=="event_dementia_4",colnames(cc)=="(Intercept)"] <-  cc[cc$var=="event_dementia_4",colnames(cc)=="(Intercept)"] + 1
cc[cc$var=="event_dementia_5",colnames(cc)=="(Intercept)"] <-  cc[cc$var=="event_dementia_5",colnames(cc)=="(Intercept)"] + 1
cc[cc$var=="event_dementia_6",colnames(cc)=="(Intercept)"] <-  cc[cc$var=="event_dementia_6",colnames(cc)=="(Intercept)"] + 1
cc[cc$var=="event_dementia_7",colnames(cc)=="(Intercept)"] <-  cc[cc$var=="event_dementia_7",colnames(cc)=="(Intercept)"] + 1
cc[cc$var=="event_dementia_8",colnames(cc)=="(Intercept)"] <-  cc[cc$var=="event_dementia_8",colnames(cc)=="(Intercept)"] + 1
cc[cc$var=="event_dementia_9",colnames(cc)=="(Intercept)"] <-  cc[cc$var=="event_dementia_9",colnames(cc)=="(Intercept)"] + 1
cc[cc$var=="event_dementia_10",colnames(cc)=="(Intercept)"] <-  cc[cc$var=="event_dementia_10",colnames(cc)=="(Intercept)"] + 1

#Check null truth
sim_truth <- calc_sim_truth(cc)


u <- synthesizeDD(cc)

set.seed(12345)
sim_list <- NULL
n<-200
N_time=10
d <- sim(u,115698)
table(d$glp1_0)
table(d$glp1_1)
table(d$glp1_2)

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

