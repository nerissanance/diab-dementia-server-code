

rm(list=ls())
library(lava)
library(data.table)
source(paste0(here::here(),"/synthesizeDD.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))

cc <- fread(paste0(here::here(),"/data/coefficients.txt"))

cc <- as.data.frame(cc)
cc <- cc %>% filter(!grepl("event_death",var), !grepl("censor",var))


synthesizeDD<-function(coefficients){
  requireNamespace("lava")
  coefficients <- data.table(coefficients)
  XNAMES <- names(coefficients)[-(1:3)]
  BETA <- coefficients[,-(1:3),with=0L]
  INTERCEPT <- coefficients[["(Intercept)"]]
  # empty lava model for simulation
  m <- lvm()
  distribution(m,"age_base") <- normal.lvm(mean=70,sd=10)
  distribution(m,"sex") <- binomial.lvm(p=0.4)
  m <- addvar(m,"ie_type")
  m <- addvar(m,"code5txt")
  m <- addvar(m,"quartile_income")
  # loop across time and variables
  for (j in 1:NROW(coefficients)){
    V <- coefficients$var[j]
    beta <- unlist(BETA[j,])
    X <- XNAMES[!is.na(beta)]
    beta <- beta[!is.na(beta)]
    # add V ~ Intercept + beta X
    distribution(m,V) <- binomial.lvm()
    intercept(m,V) <- INTERCEPT[j]
    regression(m,from=X,to=V) <- beta
  }
  class(m) <- c("synthesizeDD",class(m))
  m
}

u <- synthesizeDD(cc)

set.seed(12345)
sim_list <- NULL
n<-200
N_time=10



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


  #note: once events jump to 1, need to remain 1 for remainder of follow up
  for(k in 1:(N_time+1)){
    j=k+1
    d[get(paste0("event_dementia_",k))==1, (paste0("event_dementia_",j)):=1]
    # d[get(paste0("event_death_",k))==1, (paste0("event_death_",j)):=1]
    # d[get(paste0("event_death_",k))==1, (paste0("censor_",j)):=1]
    #d[get(paste0("censor_",k))==1, (paste0("censor_",j)):=1]
  }


  sim_list[[i]] <- d
  gc()
}

saveRDS(sim_list, paste0(here::here(),"/data/simulated_data_list_no_death.RDS"))
d_wide_list<-sim_list




