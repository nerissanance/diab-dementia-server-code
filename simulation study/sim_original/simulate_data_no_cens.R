

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

# #for thomas:
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

  #note: once events jump to 1, need to remain 1 for remainder of follow up
  for(k in 1:(N_time+1)){
    j=k+1
    d[get(paste0("event_death_",k))==1, (paste0("event_death_",k)):=0]
    d[get(paste0("censor_",k))==1, (paste0("censor_",k)):=0]

  }

  sim_list[[i]] <- d
  gc()
}

saveRDS(sim_list, paste0(here::here(),"/data/simulated_data_list_no_cens.RDS"))
