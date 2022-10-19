

rm(list=ls())
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))
source(paste0(here::here(),"/synthesizeDD.R"))

cc <- fread(paste0(here::here(),"/data/coefficients.txt"))

#make a dataset with no censoring or death
cc<-as.data.frame(cc)
cc <- cc %>% filter(!grepl("censor_",var), !grepl("event_death",var)) %>%  select(!starts_with("censor_"),!starts_with("event_death"))


#NOTE! NEED TO FIX so no association, and before generating, check the truth in the data


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


#double check truth

#calculate truth
sim_truth <- calc_sim_truth(cc)


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
  }

  sim_list[[i]] <- d
  gc()
}

saveRDS(sim_list, paste0(here::here(),"/data/simulated_data_list_null_no_cens.RDS"))

