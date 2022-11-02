

rm(list=ls())
library(lava)
library(data.table)
source(paste0(here::here(),"/synthesizeDD.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))

cc <- fread(paste0(here::here(),"/data/coefficients.txt"))
u <- synthesizeDD(cc)

set.seed(12345)
sim_list <- NULL
n<-200
N_time=10

#calc truth
sim_truth <- calc_sim_truth(cc)
cRD <- sim_truth$cRD
cRR <- sim_truth$cRR
save(cRD, cRR, file=paste0(here::here(),"/results/truth_rare.Rdata"))

homas:
# #alt truth calculation
# #original: 0.3930283
# sim_truth2 <- calc_sim_truth(cc, nsamp=1000001)
# #0.3955411
# sim_truth3 <- calc_sim_truth(cc, nsamp=2000001)
# #0.3921497
# sim_truth4 <- calc_sim_truth(cc, nsamp=3000001)
# #0.3934562
# sim_truth5 <- calc_sim_truth(cc, nsamp=3200001)
# #0.3927931



for(i in 1:n){
  cat("\ni: ",i,"\n")

  d <- sim(u,115698)
  #Make sure no super rare events
  # flag <- TRUE
  # for(j in 1:20){
  #   if(flag){
  #     set.seed(i*100 + j)
  #     d <- sim(u,115698)
  #     d$cum_glp1 <- d$glp1_0+d$glp1_1+d$glp1_2+d$glp1_3+d$glp1_4+d$glp1_5+d$glp1_6+d$glp1_7+d$glp1_8+d$glp1_9+d$glp1_10
  #     d$cum_dementia <- d$event_dementia_0+d$event_dementia_1+d$event_dementia_2+d$event_dementia_3+d$event_dementia_4+d$event_dementia_5+d$event_dementia_6+d$event_dementia_7+d$event_dementia_8+d$event_dementia_9+d$event_dementia_10
  #     cat(min(table(d$cum_glp1==11, d$cum_dementia>0)),", ")
  #     flag <- ifelse(min(table(d$cum_glp1==11, d$cum_dementia>0))<6, TRUE, FALSE)
  #   }
  # }

  #note: once events jump to 1, need to remain 1 for remainder of follow up
  for(k in 1:(N_time+1)){
    j=k+1
    d[get(paste0("event_dementia_",k))==1, (paste0("event_dementia_",j)):=1]
    d[get(paste0("event_death_",k))==1, (paste0("event_death_",j)):=1]
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

saveRDS(sim_list, paste0(here::here(),"/data/simulated_data_list.RDS"))
