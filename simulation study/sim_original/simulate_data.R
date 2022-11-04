

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

Thomas:
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

  d$dem_prev <-    mean(1*(d$event_dementia_1 +
                        d$event_dementia_2 +
                        d$event_dementia_3 +
                        d$event_dementia_4 +
                        d$event_dementia_5 +
                        d$event_dementia_6 +
                        d$event_dementia_7 +
                        d$event_dementia_8 +
                        d$event_dementia_9 +
                        d$event_dementia_10 >0))

  d<- clean_sim_data(d, N_time = 10)

  sim_list[[i]] <- d
  gc()
}


# prev2 <-prev1 <- rep(NA, 200)
# for(i in 1:200){
#   prev1[i] <- sim_list[[i]]$dem_prev[1]
#   prev2[i] <- prop.table(table(sim_list[[i]]$event_dementia_10))[2]
#
# }
# mean(prev1)*100
# mean(prev2)*100
#
# table(1*sim_list[[i]]$event_dementia_7, sim_list[[i]]$event_dementia_8)
# table(1*sim_list[[i]]$event_dementia_8, sim_list[[i]]$event_dementia_9)
# table(1*sim_list[[i]]$event_dementia_9, sim_list[[i]]$event_dementia_10)
#
#
# mean(1*(sim_list[[i]]$event_dementia_1 +
#           sim_list[[i]]$event_dementia_2 +
#           sim_list[[i]]$event_dementia_3 +
#           sim_list[[i]]$event_dementia_4 +
#           sim_list[[i]]$event_dementia_5 +
#           sim_list[[i]]$event_dementia_6 +
#           sim_list[[i]]$event_dementia_7 +
#           sim_list[[i]]$event_dementia_8 +
#           sim_list[[i]]$event_dementia_9 +
#           sim_list[[i]]$event_dementia_10 >0))
#
# #Prevalence: 5.8% before competing risk fix
#              #2% after

saveRDS(sim_list, paste0(here::here(),"/data/simulated_data_list.RDS"))
d_wide_list<-sim_list
