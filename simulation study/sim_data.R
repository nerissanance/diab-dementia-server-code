

rm(list=ls())
library(lava)
library(data.table)
source(paste0(here::here(),"/synthesizeDD.R"))

cc <- fread(paste0(here::here(),"/data/coefficients.txt"))
u <- synthesizeDD(cc)
#d <- sim(u,1000)
#head(d)

sim_list <- NULL
n<-100

for(i in 1:n){
  print(i)
  set.seed(i)
  sim_list[[i]] <- sim(u,100000)
  gc()
}

saveRDS(sim_list, paste0(here::here(),"/data/simulated_data_list.RDS"))
