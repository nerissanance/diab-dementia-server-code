



rm(list=ls())
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))
source(paste0(here::here(),"/simulation study/0_simulation_cleaning_functions.R"))
cc <- fread(paste0(here::here(),"/data/coefficients.txt"))



synthesizeDD.always <- function(coefficients, A_name = "glp1"){
  requireNamespace("lava")
  coefficients <- data.table(coefficients)
  XNAMES <- names(coefficients)[-(1:3)]
  BETA <- coefficients[,-(1:3),with=0L]
  # collect At  nodes; intervene At=1 and Ct=0 later
  loc_A <- grep(paste0("^", A_name, "_"), XNAMES)
  beta_A <- BETA[, loc_A, with = F]


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
    # At constant 1 -> intercept becomes intercept + At coefficient
    # also remove At from fitted betas
    temp_intercept <- INTERCEPT[j]
    temp_sum_A_coef <- rowSums(beta_A[j], na.rm = T)  # intercept + At coefficients
    temp_intercept <- temp_intercept + temp_sum_A_coef

    V <- coefficients$var[j]
    beta <- unlist(BETA[j,])
    beta[loc_A] <- NA  # absorb A coefficient into intercept for always-on group; not depending on observed A values any more

    X <- XNAMES[!is.na(beta)]
    beta <- beta[!is.na(beta)]
    # add V ~ Intercept + beta X
    distribution(m,V) <- binomial.lvm()
    intercept(m,V) <- ifelse(grepl("event_death",V), INTERCEPT[j], temp_intercept) #remove competing risk
    regression(m,from=X,to=V) <- beta
  }
  class(m) <- c("synthesizeDD",class(m))
  m
}



synthesizeDD.never <- function(coefficients, A_name = "glp1"){
  requireNamespace("lava")
  coefficients <- data.table(coefficients)
  XNAMES <- names(coefficients)[-(1:3)]
  BETA <- coefficients[,-(1:3),with=0L]
  # collect At and Ct nodes; intervene At=1 and Ct=0 later
  loc_A <- grep(paste0("^", A_name, "_"), XNAMES)
  beta_A <- BETA[, loc_A, with = F]


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
    beta[loc_A] <- NA  # absorb A coefficient into intercept for always-on group; not depending on observed A values any more
    X <- XNAMES[!is.na(beta)]
    beta <- beta[!is.na(beta)]
    # add V ~ Intercept + beta X
    distribution(m,V) <- binomial.lvm()
    intercept(m,V) <- INTERCEPT[j] #keep only intercept for "never on"
    regression(m,from=X,to=V) <- beta
  }
  class(m) <- c("synthesizeDD",class(m))
  m
}


#Dementia after death should be NA!

clean_sim_data <- function(d, N_time=10){

  d<- data.table(d)

  for(i in 1:(N_time+1)){
    j=i+1
    d[is.na(get(paste0("event_dementia_",i))), (paste0("event_dementia_",j)):=NA]
    d[get(paste0("event_dementia_",i))==1, (paste0("event_dementia_",j)):=1]
    d[get(paste0("event_death_",i))==1, (paste0("event_death_",j)):=1]
    d[get(paste0("event_death_",i))==1, (paste0("event_dementia_",j)):=NA]
  }
  return(d)
}





 seed <- 3457347
 nsamp=5000000
 #nsamp=3000000

  set.seed(seed)
  # u <- synthesizeDD(cc)
  # d.full <- sim(u, nsamp)

  set.seed(seed)
  u.always <- synthesizeDD.always(cc)
  d.always.full <- sim(u.always, nsamp)

  set.seed(seed)
  u.never <- synthesizeDD.never(cc)
  d.never.full <- sim(u.never, nsamp)

  d.always <- d.always.full
  d.never <- d.never.full
  N_time=10


  # #get deaths from the never on in case confounding by glp1 effect on comorbidities
  # ddeath <- d.never.full %>% select(starts_with("event_death"))
  # d.always.full <- d.always.full %>% select(!starts_with("event_death"))
  # d.always.full <- bind_cols(d.always.full, ddeath)


  #d <- clean_sim_data(d.full, 10)
  d.always <- clean_sim_data(d.always.full, 10)
  d.never <- clean_sim_data(d.never.full, 10)


 tRR1 <- mean(d.always$event_dementia_1,na.rm=T)/mean(d.never$event_dementia_1,na.rm=T)
 tRR2 <- mean(d.always$event_dementia_2,na.rm=T)/mean(d.never$event_dementia_2,na.rm=T)
 tRR3 <- mean(d.always$event_dementia_3,na.rm=T)/mean(d.never$event_dementia_3,na.rm=T)
 tRR4 <- mean(d.always$event_dementia_4,na.rm=T)/mean(d.never$event_dementia_4,na.rm=T)
 tRR5 <- mean(d.always$event_dementia_5,na.rm=T)/mean(d.never$event_dementia_5,na.rm=T)
 tRR6 <- mean(d.always$event_dementia_6,na.rm=T)/mean(d.never$event_dementia_6,na.rm=T)
 tRR7 <- mean(d.always$event_dementia_7,na.rm=T)/mean(d.never$event_dementia_7,na.rm=T)
 tRR8 <- mean(d.always$event_dementia_8,na.rm=T)/mean(d.never$event_dementia_8,na.rm=T)
 tRR9 <- mean(d.always$event_dementia_9,na.rm=T)/mean(d.never$event_dementia_9,na.rm=T)
 tRR10 <- mean(d.always$event_dementia_10,na.rm=T)/mean(d.never$event_dementia_10,na.rm=T)

 tRD1 <- mean(d.always$event_dementia_1,na.rm=T) - mean(d.never$event_dementia_1,na.rm=T)
 tRD2 <- mean(d.always$event_dementia_2,na.rm=T) - mean(d.never$event_dementia_2,na.rm=T)
 tRD3 <- mean(d.always$event_dementia_3,na.rm=T) - mean(d.never$event_dementia_3,na.rm=T)
 tRD4 <- mean(d.always$event_dementia_4,na.rm=T) - mean(d.never$event_dementia_4,na.rm=T)
 tRD5 <- mean(d.always$event_dementia_5,na.rm=T) - mean(d.never$event_dementia_5,na.rm=T)
 tRD6 <- mean(d.always$event_dementia_6,na.rm=T) - mean(d.never$event_dementia_6,na.rm=T)
 tRD7 <- mean(d.always$event_dementia_7,na.rm=T) - mean(d.never$event_dementia_7,na.rm=T)
 tRD8 <- mean(d.always$event_dementia_8,na.rm=T) - mean(d.never$event_dementia_8,na.rm=T)
 tRD9 <- mean(d.always$event_dementia_9,na.rm=T) - mean(d.never$event_dementia_9,na.rm=T)
 tRD10 <- mean(d.always$event_dementia_10,na.rm=T) - mean(d.never$event_dementia_10,na.rm=T)

truth_df <- data.frame(time=1:10, RR=c(tRR1,tRR2,tRR3,tRR4,tRR5,tRR6,tRR7,tRR8,tRR9,tRR10), RD=c(tRD1,tRD2,tRD3,tRD4,tRD5,tRD6,tRD7,tRD8,tRD9,tRD10))
truth_df
saveRDS(truth_df, file=paste0(here::here(),"/data/sim_res_truth.RDS"))




  mean(d.always$event_dementia_1,na.rm=T)
  mean(d.always$event_dementia_2,na.rm=T)
  mean(d.always$event_dementia_3,na.rm=T)
  mean(d.always$event_dementia_4,na.rm=T)
  mean(d.always$event_dementia_5,na.rm=T)
  mean(d.always$event_dementia_6,na.rm=T)
  mean(d.always$event_dementia_7,na.rm=T)
  mean(d.always$event_dementia_8,na.rm=T)
  mean(d.always$event_dementia_9,na.rm=T)
  mean(d.always$event_dementia_10,na.rm=T)


  mean(d.never$event_dementia_1,na.rm=T)
  mean(d.never$event_dementia_2,na.rm=T)
  mean(d.never$event_dementia_3,na.rm=T)
  mean(d.never$event_dementia_4,na.rm=T)
  mean(d.never$event_dementia_5,na.rm=T)
  mean(d.never$event_dementia_6,na.rm=T)
  mean(d.never$event_dementia_7,na.rm=T)
  mean(d.never$event_dementia_8,na.rm=T)
  mean(d.never$event_dementia_9,na.rm=T)
  mean(d.never$event_dementia_10,na.rm=T)

