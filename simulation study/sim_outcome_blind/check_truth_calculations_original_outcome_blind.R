



rm(list=ls())
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))
source(paste0(here::here(),"/simulation study/0_simulation_cleaning_functions.R"))
cc <- fread(paste0(here::here(),"/data/coefficients.txt"))
cc<-as.data.frame(cc)

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
 #nsamp=1000000
 nsamp=5000000

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


  #Temp: get deaths from the never on in case confounding by glp1 effect on comorbidities
  # ddeath <- d.never.full %>% select(starts_with("event_death"))
  # d.always.full <- d.always.full %>% select(!starts_with("event_death"))
  # d.always.full <- bind_cols(d.always.full, ddeath)

  ddeath <- d.always.full %>% select(starts_with("event_death"))
  d.never.full <- d.never.full %>% select(!starts_with("event_death"))
  d.never.full <- bind_cols(d.never.full, ddeath)

  #d <- clean_sim_data(d.full, 10)
  d.always <- clean_sim_data(d.always.full, 10)
  d.never <- clean_sim_data(d.never.full, 10)


  mean(d.always$event_dementia_1,na.rm=T)/mean(d.never$event_dementia_1,na.rm=T)
  mean(d.always$event_dementia_2,na.rm=T)/mean(d.never$event_dementia_2,na.rm=T)
  mean(d.always$event_dementia_3,na.rm=T)/mean(d.never$event_dementia_3,na.rm=T)
  mean(d.always$event_dementia_4,na.rm=T)/mean(d.never$event_dementia_4,na.rm=T)
  mean(d.always$event_dementia_5,na.rm=T)/mean(d.never$event_dementia_5,na.rm=T)
  mean(d.always$event_dementia_6,na.rm=T)/mean(d.never$event_dementia_6,na.rm=T)
  mean(d.always$event_dementia_7,na.rm=T)/mean(d.never$event_dementia_7,na.rm=T)
  mean(d.always$event_dementia_8,na.rm=T)/mean(d.never$event_dementia_8,na.rm=T)
  mean(d.always$event_dementia_9,na.rm=T)/mean(d.never$event_dementia_9,na.rm=T)
  mean(d.always$event_dementia_10,na.rm=T)/mean(d.never$event_dementia_10,na.rm=T)




  mean(d.always$event_dementia_1,na.rm=T) - mean(d.never$event_dementia_1,na.rm=T)
  mean(d.always$event_dementia_2,na.rm=T) - mean(d.never$event_dementia_2,na.rm=T)
  mean(d.always$event_dementia_3,na.rm=T) - mean(d.never$event_dementia_3,na.rm=T)
  mean(d.always$event_dementia_4,na.rm=T) - mean(d.never$event_dementia_4,na.rm=T)
  mean(d.always$event_dementia_5,na.rm=T) - mean(d.never$event_dementia_5,na.rm=T)
  mean(d.always$event_dementia_6,na.rm=T) - mean(d.never$event_dementia_6,na.rm=T)
  mean(d.always$event_dementia_7,na.rm=T) - mean(d.never$event_dementia_7,na.rm=T)
  mean(d.always$event_dementia_8,na.rm=T) - mean(d.never$event_dementia_8,na.rm=T)
  mean(d.always$event_dementia_9,na.rm=T) - mean(d.never$event_dementia_9,na.rm=T)
  mean(d.always$event_dementia_10,na.rm=T) - mean(d.never$event_dementia_10,na.rm=T)





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

