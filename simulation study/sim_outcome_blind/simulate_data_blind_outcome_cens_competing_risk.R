

rm(list=ls())
library(lava)
library(tidyverse)
library(data.table)
source(paste0(here::here(),"/synthesizeDD.R"))

cc <- fread(paste0(here::here(),"/data/coefficients.txt"))

#make a dataset with no censoring or death
cc<-as.data.frame(cc)



#Artificially set Y-A association
cc[cc$var=="event_dementia_1",colnames(cc)=="glp1_1"] <- (-0.8)
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

exp(cc[cc$var=="event_dementia_1",which(colnames(cc)=="glp1_1")])
exp(cc[cc$var=="event_dementia_10",which(colnames(cc)=="glp1_10")])
0.449329^10

exp(cc[cc$var=="event_dementia_1",which(colnames(cc)=="glp1_1")])

# event_dementia_6 ~ age_base+sex+code5txt+quartile_income+ie_type+insulin_6+any.malignancy_6+chronic.pulmonary.disease_6+hypertension_6+myocardial.infarction_6+ischemic.heart.disease_6+heart.failure_6+renal.disease_6+glp1_6+sglt2_inhib_6                         binomial(logit)
#
# event_dementia_1 ~ age_base+sex+code5txt+quartile_income+ie_type+insulin_1+any.malignancy_1+chronic.pulmonary.disease_1+hypertension_1+myocardial.infarction_1+ischemic.heart.disease_1+heart.failure_1+renal.disease_1+glp1_1+sglt2_inhib_1


# #update so there is an decaying relationship between past variable and each variable, so that the whole history is used
# head(cc)
# i=ncol(cc)-9
# j=nrow(cc)
# k=10
# coef_decay=4
# for(i in ncol(cc):9){
#   for(k in 10:1){
#   for(j in nrow(cc):20){
#
#     # colnames(cc)[i]
#     # colnames(cc)[i+8]
#     # cc[j,2]
#       if(i+(8*k) < ncol(cc) & (j-i >= 11*k)){
#         cc[j,i] <- cc[j,i+(8*k)]/(coef_decay*k)
#       }
#     }
#   }
# }

write.csv(cc, paste0(here::here(),"/data/coefficients_outcome_blind_cens_competing_risks.txt"))



u <- synthesizeDD(cc)


set.seed(12345)
sim_list <- NULL
n<-200
N_time=10


for(i in 1:n){
  cat("\ni: ",i,"\n")


      set.seed(i*100)
      d <- sim(u,115698)
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

saveRDS(sim_list, paste0(here::here(),"/data/simulated_data_list_outcome_blind_cens_competing_risks.RDS"))
