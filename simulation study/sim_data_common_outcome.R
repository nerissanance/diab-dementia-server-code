

rm(list=ls())
library(lava)
library(data.table)
source(paste0(here::here(),"/synthesizeDD.R"))

cc <- fread(paste0(here::here(),"/data/coefficients.txt"))

#update coefficients

rows <- grep("dementia",cc$var)

for(i in rows){
  cc[[i,"(Intercept)"]] <- cc[[i,"(Intercept)"]]+4.2

}


u <- synthesizeDD(cc)
#d <- sim(u,1000)
#head(d)

sim_list <- NULL
n<-100


for(i in 1:n){
  cat("\ni: ",i,"\n")

  flag <- TRUE
  for(j in 1:20){
    if(flag){
      set.seed(i*100 + j)
      d <- sim(u,100000)
      d$cum_glp1 <- d$glp1_0+d$glp1_1+d$glp1_2+d$glp1_3+d$glp1_4+d$glp1_5+d$glp1_6+d$glp1_7+d$glp1_8+d$glp1_9+d$glp1_10
      d$cum_dementia <- d$event_dementia_0+d$event_dementia_1+d$event_dementia_2+d$event_dementia_3+d$event_dementia_4+d$event_dementia_5+d$event_dementia_6+d$event_dementia_7+d$event_dementia_8+d$event_dementia_9+d$event_dementia_10
      cat(min(table(d$cum_glp1==11, d$cum_dementia>0)),", ")
      flag <- ifelse(min(table(d$cum_glp1==11, d$cum_dementia>0))<6, TRUE, FALSE)
    }
  }

  sim_list[[i]] <- d
  gc()
}
prop.table(table(sim_list[[3]]$event_dementia_1))
prop.table(table(sim_list[[3]]$event_dementia_12))
saveRDS(sim_list, paste0(here::here(),"/data/simulated_data_list_commonoutcome.RDS"))

