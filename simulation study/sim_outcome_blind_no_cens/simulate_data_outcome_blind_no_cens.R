

rm(list=ls())
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))
source(paste0(here::here(),"/synthesizeDD.R"))

cc <- fread(paste0(here::here(),"/data/coefficients.txt"))

#make a dataset with no censoring or death
cc<-as.data.frame(cc)
cc <- cc %>% filter(!grepl("censor_",var), !grepl("event_death",var)) %>%  select(!starts_with("censor_"),!starts_with("event_death"))
# cc[grepl("event_death",cc$var),-c(1:2)] <- 0
# cc[grepl("censor_",cc$var),-c(1:2)] <-  0


#Artificially set Y-A association
cc[cc$var=="event_dementia_1",which(colnames(cc)=="glp1_1")] <- (-0.8)
cc[cc$var=="event_dementia_2",colnames(cc)=="glp1_2"] <- (-0.8)
cc[cc$var=="event_dementia_3",colnames(cc)=="glp1_3"] <- (-0.8)
cc[cc$var=="event_dementia_4",colnames(cc)=="glp1_4"] <- (-0.8)
cc[cc$var=="event_dementia_5",colnames(cc)=="glp1_5"] <- (-0.8)
cc[cc$var=="event_dementia_6",colnames(cc)=="glp1_6"] <- (-0.8)
cc[cc$var=="event_dementia_7",colnames(cc)=="glp1_7"] <- (-0.8)
cc[cc$var=="event_dementia_8",colnames(cc)=="glp1_8"] <- (-0.8)
cc[cc$var=="event_dementia_9",colnames(cc)=="glp1_9"] <- (-0.8)
cc[cc$var=="event_dementia_10",colnames(cc)=="glp1_10"] <- (-0.8)

#Make dementia slightly more common
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


write.csv(cc, paste0(here::here(),"/data/coefficients_outcome_blind.txt"))


#calculate truth
sim_truth <- calc_sim_truth(cc)
cRD<-sim_truth$cRD
cRR<-sim_truth$cRR
cRD3<-sim_truth$cRD3
cRR3<-sim_truth$cRR3


save(cRD, cRR, file=paste0(here::here(),"/results/truth_blind_T10.Rdata"))
save(cRD3, cRR3, file=paste0(here::here(),"/results/truth_blind_T4.Rdata"))



#simulate data
u <- synthesizeDD(cc)

set.seed(12345)
sim_list <- NULL
n<-200
N_time=10


for(i in 1:n){
  cat("\ni: ",i,"\n")


  set.seed(i*100)
  d <- sim(u,115698)


  #note: once events jump to 1, need to remain 1 for remainder of follow up
  for(k in 1:(N_time+1)){
    j=k+1
    d[get(paste0("event_dementia_",k))==1, (paste0("event_dementia_",j)):=1]
  }

  sim_list[[i]] <- d
  gc()
}

saveRDS(sim_list, paste0(here::here(),"/data/simulated_data_list_outcome_blind_no_cens.RDS"))

