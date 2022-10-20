

rm(list=ls())
library(lava)
library(tidyverse)
library(data.table)
source(paste0(here::here(),"/synthesizeDD.R"))

cc <- fread(paste0(here::here(),"/data/coefficients.txt"))

#make a dataset with no censoring or death
cc<-as.data.frame(cc)
cc[,-c(1:3)] <- NA
cc <- cc %>% filter(grepl("event_dementia",cc$var) | grepl("glp1",cc$var))
cc <- cc[,c(1:3, which(grepl("glp1",colnames(cc))))]
cc[!grepl("event_dementia",cc$var) & !grepl("glp1",cc$var),colnames(cc)=="(Intercept)"] <- NA


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


synthesizeDD.always <- function(coefficients, A_name = "glp1"){
  requireNamespace("lava")
  coefficients <- data.table(coefficients)
  XNAMES <- names(coefficients)[-(1:3)]
  BETA <- coefficients[,-(1:3),with=0L]
  # collect At and Ct nodes; intervene At=1 and Ct=0 later
  loc_A <- grep(paste0("^", A_name, "_"), XNAMES)
  beta_A <- BETA[, loc_A, with = F]
  loc_C <- grep("^censor_", XNAMES)
  beta_C <- BETA[, loc_C, with = F]


  INTERCEPT <- coefficients[["(Intercept)"]]
  # empty lava model for simulation
  m <- lvm()
  # distribution(m,"age_base") <- normal.lvm(mean=70,sd=10)
  # distribution(m,"sex") <- binomial.lvm(p=0.4)
  # m <- addvar(m,"ie_type")
  # m <- addvar(m,"code5txt")
  # m <- addvar(m,"quartile_income")
  # loop across time and variables
  for (j in 1:NROW(coefficients)){
    # At constant 1 -> intercept becomes intercept + At coefficient
    # also remove At from fitted betas
    # Ct constant 0 -> intercept no change, remove Ct from fitted betas
    temp_intercept <- INTERCEPT
    temp_sum_A_coef <- rowSums(beta_A, na.rm = T)  # intercept + At coefficients
    temp_intercept <- temp_intercept + temp_sum_A_coef

    V <- coefficients$var[j]
    beta <- unlist(BETA[j,])
    beta[loc_A] <- NA  # absorb A coefficient into intercept for always-on group; not depending on observed A values any more
    beta[loc_C] <- NA  # C is also intervened so will be ignored in conditional logistic models

    X <- XNAMES[!is.na(beta)]
    beta <- beta[!is.na(beta)]
    # add V ~ Intercept + beta X
    distribution(m,V) <- binomial.lvm()
    # intercept(m,V) <- INTERCEPT[j]
    intercept(m,V) <- temp_intercept[j]
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
  loc_C <- grep("^censor_", XNAMES)
  beta_C <- BETA[, loc_C, with = F]


  INTERCEPT <- coefficients[["(Intercept)"]]
  # empty lava model for simulation
  m <- lvm()
  # distribution(m,"age_base") <- normal.lvm(mean=70,sd=10)
  # distribution(m,"sex") <- binomial.lvm(p=0.4)
  # m <- addvar(m,"ie_type")
  # m <- addvar(m,"code5txt")
  # m <- addvar(m,"quartile_income")
  # loop across time and variables
  for (j in 1:NROW(coefficients)){
    # At constant 1 -> intercept becomes intercept + At coefficient
    # also remove At from fitted betas
    # Ct constant 0 -> intercept no change, remove Ct from fitted betas
    temp_intercept <- INTERCEPT
    temp_sum_A_coef <- rowSums(beta_A, na.rm = T)  # intercept + At coefficients
    temp_intercept <- temp_intercept + temp_sum_A_coef

    V <- coefficients$var[j]
    beta <- unlist(BETA[j,])
    beta[loc_A] <- NA  # absorb A coefficient into intercept for always-on group; not depending on observed A values any more
    beta[loc_C] <- NA  # C is also intervened so will be ignored in conditional logistic models

    X <- XNAMES[!is.na(beta)]
    beta <- beta[!is.na(beta)]
    # add V ~ Intercept + beta X
    distribution(m,V) <- binomial.lvm()
    intercept(m,V) <- INTERCEPT[j]#keep only intercept for "never on"
    # intercept(m,V) <- temp_intercept[j]
    regression(m,from=X,to=V) <- beta
  }
  class(m) <- c("synthesizeDD",class(m))
  m
}


seed <- 3457347
nsamp <- 1000000

set.seed(seed)
u.always <- synthesizeDD.always(cc)
d.always <- sim(u.always, nsamp)
head(d.always)

set.seed(seed)
u.never <- synthesizeDD.never(cc)
d.never <- sim(u.never, nsamp)




(prop.always <-
    1*(d.always$event_dementia_1 +
         d.always$event_dementia_2 +
         d.always$event_dementia_3 +
         d.always$event_dementia_4 +
         d.always$event_dementia_5 +
         d.always$event_dementia_6 +
         d.always$event_dementia_7 +
         d.always$event_dementia_8 +
         d.always$event_dementia_9 +
         d.always$event_dementia_10 >0)  %>% table %>% prop.table)
(prop.always10 <- d.always$event_dementia_10 %>% table %>% prop.table)


(prop.never <-
    1*(d.never$event_dementia_1 +
         d.never$event_dementia_2 +
         d.never$event_dementia_3 +
         d.never$event_dementia_4 +
         d.never$event_dementia_5 +
         d.never$event_dementia_6 +
         d.never$event_dementia_7 +
         d.never$event_dementia_8 +
         d.never$event_dementia_9 +
         d.never$event_dementia_10 >0) %>% table %>% prop.table)
(prop.never10 <- d.never$event_dementia_10 %>% table %>% prop.table)


(cRD <-  prop.always[2] - prop.never[2])
(cRR <- (prop.always[2])/prop.never[2])
cRD
cRR
#0.4398625  #Data-generated truth
#0.4493685  #Hand-calculated

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

#need to account for cumulative survival
cc <- as.data.frame(cc)

colSums(cc[cc$var=="event_dementia_1",-c(1:2)], na.rm = FALSE)
Y_a1_t1 <- (sum(as.numeric(cc[cc$var=="event_dementia_1",-c(1:2)]), na.rm =  T))
Y_a0_t1 <- (sum(as.numeric(cc[cc$var=="event_dementia_1",-c(1:2)]), na.rm =  T) - cc[cc$var=="event_dementia_1",which(colnames(cc)=="glp1_1")])
exp(Y_a1_t1)/exp(Y_a0_t1)

logit2prob(Y_a1_t1)
logit2prob(Y_a0_t1)

Y0_a1_t1 <- 1-logit2prob(sum(as.numeric(cc[cc$var=="event_dementia_1",-c(1:2)]), na.rm =  T))
Y0_a0_t1 <- 1-logit2prob(sum(as.numeric(cc[cc$var=="event_dementia_1",-c(1:2)]), na.rm =  T) - cc[cc$var=="event_dementia_1",which(colnames(cc)=="glp1_1")])
Y0_a1_t2 <- 1-logit2prob(sum(as.numeric(cc[cc$var=="event_dementia_2",-c(1:2)]), na.rm =  T))
Y0_a0_t2 <- 1-logit2prob(sum(as.numeric(cc[cc$var=="event_dementia_2",-c(1:2)]), na.rm =  T) - cc[cc$var=="event_dementia_2",which(colnames(cc)=="glp1_2")])
Y0_a1_t3 <- 1-logit2prob(sum(as.numeric(cc[cc$var=="event_dementia_3",-c(1:2)]), na.rm =  T))
Y0_a0_t3 <- 1-logit2prob(sum(as.numeric(cc[cc$var=="event_dementia_3",-c(1:2)]), na.rm =  T) - cc[cc$var=="event_dementia_3",which(colnames(cc)=="glp1_3")])
Y0_a1_t4 <- 1-logit2prob(sum(as.numeric(cc[cc$var=="event_dementia_4",-c(1:2)]), na.rm =  T))
Y0_a0_t4 <- 1-logit2prob(sum(as.numeric(cc[cc$var=="event_dementia_4",-c(1:2)]), na.rm =  T) - cc[cc$var=="event_dementia_4",which(colnames(cc)=="glp1_4")])
Y0_a1_t5 <- 1-logit2prob(sum(as.numeric(cc[cc$var=="event_dementia_5",-c(1:2)]), na.rm =  T))
Y0_a0_t5 <- 1-logit2prob(sum(as.numeric(cc[cc$var=="event_dementia_5",-c(1:2)]), na.rm =  T) - cc[cc$var=="event_dementia_5",which(colnames(cc)=="glp1_5")])
Y0_a1_t6 <- 1-logit2prob(sum(as.numeric(cc[cc$var=="event_dementia_6",-c(1:2)]), na.rm =  T))
Y0_a0_t6 <- 1-logit2prob(sum(as.numeric(cc[cc$var=="event_dementia_6",-c(1:2)]), na.rm =  T) - cc[cc$var=="event_dementia_6",which(colnames(cc)=="glp1_6")])
Y0_a1_t7 <- 1-logit2prob(sum(as.numeric(cc[cc$var=="event_dementia_7",-c(1:2)]), na.rm =  T))
Y0_a0_t7 <- 1-logit2prob(sum(as.numeric(cc[cc$var=="event_dementia_7",-c(1:2)]), na.rm =  T) - cc[cc$var=="event_dementia_7",which(colnames(cc)=="glp1_7")])
Y0_a1_t8 <- 1-logit2prob(sum(as.numeric(cc[cc$var=="event_dementia_8",-c(1:2)]), na.rm =  T))
Y0_a0_t8 <- 1-logit2prob(sum(as.numeric(cc[cc$var=="event_dementia_8",-c(1:2)]), na.rm =  T) - cc[cc$var=="event_dementia_8",which(colnames(cc)=="glp1_8")])
Y0_a1_t9 <- 1-logit2prob(sum(as.numeric(cc[cc$var=="event_dementia_9",-c(1:2)]), na.rm =  T))
Y0_a0_t9 <- 1-logit2prob(sum(as.numeric(cc[cc$var=="event_dementia_9",-c(1:2)]), na.rm =  T) - cc[cc$var=="event_dementia_9",which(colnames(cc)=="glp1_9")])
Y0_a1_t10 <- 1-logit2prob(sum(as.numeric(cc[cc$var=="event_dementia_10",-c(1:2)]), na.rm =  T))
Y0_a0_t10 <- 1-logit2prob(sum(as.numeric(cc[cc$var=="event_dementia_10",-c(1:2)]), na.rm =  T) - cc[cc$var=="event_dementia_10",which(colnames(cc)=="glp1_10")])

CI_A1 <- 1-(Y0_a1_t1*Y0_a1_t2*Y0_a1_t3*Y0_a1_t4*Y0_a1_t5*Y0_a1_t6*Y0_a1_t7*Y0_a1_t8*Y0_a1_t9*Y0_a1_t10)
CI_A0 <-1-(Y0_a0_t1*Y0_a0_t2*Y0_a0_t3*Y0_a0_t4*Y0_a0_t5*Y0_a0_t6*Y0_a0_t7*Y0_a0_t8*Y0_a0_t9*Y0_a0_t10)

CI_A1/CI_A0
CI_A1-CI_A0



CI_A1_T4 <- 1-(Y0_a1_t1*Y0_a1_t2*Y0_a1_t3)
CI_A0_T4  <-1-(Y0_a0_t1*Y0_a0_t2*Y0_a0_t3)

CI_A1_T4 /CI_A0_T4
CI_A1_T4 -CI_A0_T4



