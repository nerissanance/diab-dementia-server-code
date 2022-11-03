



rm(list=ls())
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))
source(paste0(here::here(),"/simulation study/0_simulation_cleaning_functions.R"))
cc <- fread(paste0(here::here(),"/data/coefficients.txt"))


# #Calculate truth
# synthesizeDD <- function(coefficients){
#   requireNamespace("lava")
#   coefficients <- data.table(coefficients)
#   XNAMES <- names(coefficients)[-(1:3)]
#   BETA <- coefficients[,-(1:3),with=0L]
#   INTERCEPT <- coefficients[["(Intercept)"]]
#   # empty lava model for simulation
#   m <- lvm()
#   distribution(m,"age_base") <- normal.lvm(mean=70,sd=10)
#   distribution(m,"sex") <- binomial.lvm(p=0.4)
#   m <- addvar(m,"ie_type")
#   m <- addvar(m,"code5txt")
#   m <- addvar(m,"quartile_income")
#   # loop across time and variables
#   for (j in 1:NROW(coefficients)){
#     V <- coefficients$var[j]
#     beta <- unlist(BETA[j,])
#     X <- XNAMES[!is.na(beta)]
#     beta <- beta[!is.na(beta)]
#     # add V ~ Intercept + beta X
#     distribution(m,V) <- binomial.lvm()
#     intercept(m,V) <- INTERCEPT[j]
#     regression(m,from=X,to=V) <- beta
#   }
#   class(m) <- c("synthesizeDD",class(m))
#   m
# }
#
#
# synthesizeDD.always <- function(coefficients, A_name = "glp1"){
#   requireNamespace("lava")
#   coefficients <- data.table(coefficients)
#   XNAMES <- names(coefficients)[-(1:3)]
#   BETA <- coefficients[,-(1:3),with=0L]
#   # collect At and Ct nodes; intervene At=1 and Ct=0 later
#   loc_A <- grep(paste0("^", A_name, "_"), XNAMES)
#   beta_A <- BETA[, loc_A, with = F]
#   # loc_C <- grep("^censor_", XNAMES)
#   # beta_C <- BETA[, loc_C, with = F]
#
#   colnames(BETA)[loc_A]
#
#
#   INTERCEPT <- coefficients[["(Intercept)"]]
#   # empty lava model for simulation
#   m <- lvm()
#   distribution(m,"age_base") <- normal.lvm(mean=70,sd=10)
#   distribution(m,"sex") <- binomial.lvm(p=0.4)
#   m <- addvar(m,"ie_type")
#   m <- addvar(m,"code5txt")
#   m <- addvar(m,"quartile_income")
#   # loop across time and variables
#   for (j in 1:NROW(coefficients)){
#     # At constant 1 -> intercept becomes intercept + At coefficient
#     # also remove At from fitted betas
#     # Ct constant 0 -> intercept no change, remove Ct from fitted betas
#     temp_intercept <- INTERCEPT[j]
#     temp_sum_A_coef <- rowSums(beta_A[j], na.rm = T)  # intercept + At coefficients
#     temp_intercept <- temp_intercept + temp_sum_A_coef
#
#     V <- coefficients$var[j]
#     beta <- unlist(BETA[j,])
#     beta[loc_A] <- NA  # absorb A coefficient into intercept for always-on group; not depending on observed A values any more
#     #beta[loc_C] <- NA  # C is also intervened so will be ignored in conditional logistic models
#
#     X <- XNAMES[!is.na(beta)]
#     beta <- beta[!is.na(beta)]
#     # add V ~ Intercept + beta X
#     distribution(m,V) <- binomial.lvm()
#     # intercept(m,V) <- INTERCEPT[j]
#     intercept(m,V) <- temp_intercept
#     regression(m,from=X,to=V) <- beta
#   }
#   class(m) <- c("synthesizeDD",class(m))
#   m
# }



# synthesizeDD.never <- function(coefficients, A_name = "glp1"){
#   requireNamespace("lava")
#   coefficients <- data.table(coefficients)
#   XNAMES <- names(coefficients)[-(1:3)]
#   BETA <- coefficients[,-(1:3),with=0L]
#   # collect At and Ct nodes; intervene At=1 and Ct=0 later
#   loc_A <- grep(paste0("^", A_name, "_"), XNAMES)
#   beta_A <- BETA[, loc_A, with = F]
#   # loc_C <- grep("^censor_", XNAMES)
#   # beta_C <- BETA[, loc_C, with = F]
#
#
#   INTERCEPT <- coefficients[["(Intercept)"]]
#   # empty lava model for simulation
#   m <- lvm()
#   distribution(m,"age_base") <- normal.lvm(mean=70,sd=10)
#   distribution(m,"sex") <- binomial.lvm(p=0.4)
#   m <- addvar(m,"ie_type")
#   m <- addvar(m,"code5txt")
#   m <- addvar(m,"quartile_income")
#   # loop across time and variables
#   for (j in 1:NROW(coefficients)){
#     # At constant 1 -> intercept becomes intercept + At coefficient
#     # also remove At from fitted betas
#     # Ct constant 0 -> intercept no change, remove Ct from fitted betas
#
#     V <- coefficients$var[j]
#     beta <- unlist(BETA[j,])
#     beta[loc_A] <- NA  # absorb A coefficient into intercept for always-on group; not depending on observed A values any more
#     #beta[loc_C] <- NA  # C is also intervened so will be ignored in conditional logistic models
#
#     X <- XNAMES[!is.na(beta)]
#     beta <- beta[!is.na(beta)]
#     # add V ~ Intercept + beta X
#     distribution(m,V) <- binomial.lvm()
#     intercept(m,V) <- INTERCEPT[j] #keep only intercept for "never on"
#     # intercept(m,V) <- temp_intercept[j]
#     regression(m,from=X,to=V) <- beta
#   }
#   class(m) <- c("synthesizeDD",class(m))
#   m
# }
#
clean_sim_data <- function(d, N_time=10){

  #d <- as.data.frame(sapply(d, as.numeric))
  #d[is.na(d)] <- 0 #Missingness due to censoring should be coded 0 as long as censoring variable is equal to 1.
  d<- data.table(d)

  for(i in 1:(N_time+1)){
    j=i+1
    d[get(paste0("event_dementia_",i))==1, (paste0("event_dementia_",j)):=1]
    d[get(paste0("event_death_",i))==1, (paste0("event_death_",j)):=1]
  }

  dementia.nodes<- grep("event_dementia_",names(d))
  death.nodes<- grep("event_death_",names(d))
  d[, sum_death :=rowSums(.SD,na.rm=T), .SDcols = death.nodes]
  d[, sum_dementia :=rowSums(.SD,na.rm=T), .SDcols = dementia.nodes]
  d[sum_death > sum_dementia, (dementia.nodes) := replace(.SD, .SD == 1, 0), .SDcols = dementia.nodes]
  d[sum_death < sum_dementia, (death.nodes) := replace(.SD, .SD == 1, 0), .SDcols = death.nodes]
  d[sum_death== sum_dementia, (death.nodes) := replace(.SD, .SD == 1, 0), .SDcols = death.nodes]
  return(d)
}





nsamp=115698
# seed <- 3457347
 nsamp=1000000

  #set.seed(seed)
  set.seed(12345)
  u <- synthesizeDD(cc)
  d.full <- sim(u, nsamp)

  #set.seed(12345)
  set.seed(12345)
  u.always <- synthesizeDD.always(cc)
  d.always.full <- sim(u.always, nsamp)

  #set.seed(seed)
  set.seed(12345)
  u.never <- synthesizeDD.never(cc)
  d.never.full <- sim(u.never, nsamp)

  d.always <- d.always.full
  d.never <- d.never.full
  N_time=10

  d <- clean_sim_data(d.full, 10)
  d.always <- clean_sim_data(d.always.full, 10)
  d.never <- clean_sim_data(d.never.full, 10)

  tab_comp <- function(var){
    print(table(d.full[[var]]))
    print(table(d.always.full[[var]]))
    print(table(d.never.full[[var]]))
    cat("\nAfter cleaning:\n")
    print(table(d[[var]]))
    print(table(d.always[[var]]))
    print(table(d.never[[var]]))
  }

  tab_comp("event_dementia_1")
  tab_comp("event_dementia_2")

  #table(d$glp1_0+d$glp1_1+d$glp1_0)
  d <- d %>% filter()
  tab_comp("event_dementia_10")


  (prop <-
      1*(d$event_dementia_1 +
           d$event_dementia_2 +
           d$event_dementia_3 +
           d$event_dementia_4 +
           d$event_dementia_5 +
           d$event_dementia_6 +
           d$event_dementia_7 +
           d$event_dementia_8 +
           d$event_dementia_9 +
           d$event_dementia_10 >0)  %>% table %>% prop.table)
  (prop9 <- d$event_dementia_9 %>% table %>% prop.table)
  (prop10 <- d$event_dementia_10 %>% table %>% prop.table)
  (prop11 <- d$event_dementia_11 %>% table %>% prop.table)


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
  (prop.always1 <- d.always$event_dementia_1 %>% table %>% prop.table)
  (prop.always2 <- d.always$event_dementia_2 %>% table %>% prop.table)
  (prop.always9 <- d.always$event_dementia_9 %>% table %>% prop.table)
  (prop.always10 <- d.always$event_dementia_10 %>% table %>% prop.table)
  (prop.always11 <- d.always$event_dementia_11 %>% table %>% prop.table)


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
  (prop.never1 <- d.never$event_dementia_1 %>% table %>% prop.table)
  (prop.never2 <- d.never$event_dementia_2 %>% table %>% prop.table)
  (prop.never9 <- d.never$event_dementia_9 %>% table %>% prop.table)
  (prop.never10 <- d.never$event_dementia_10 %>% table %>% prop.table)
  (prop.never11 <- d.never$event_dementia_11 %>% table %>% prop.table)



  prop*100
  prop.always*100
  prop.never*100
  #mean(d_wide_list[[1]]$event_dementia_10)*100

  prop10*100
  prop.always10*100
  prop.never10*100

  (cRD <-  prop.always[2] - prop.never[2])
  (cRR <- (prop.always[2])/(prop.never[2]))

  cat("Risk ratio T10: ",cRR,"\n")
  cat("Risk difference T10: ",cRD,"\n")



  (prop.always1[2])/(prop.never1[2])
  (prop.always2[2])/(prop.never2[2])
  (prop.always2[2])-(prop.never2[2])

    (prop.always9[2])/(prop.never9[2])
  (prop.always10[2])/(prop.never10[2])
  (prop.always11[2])/(prop.never11[2])


  mean(d.always$event_dementia_1)/mean(d.never$event_dementia_1)
  mean(d.always$event_dementia_2)/mean(d.never$event_dementia_2)
  mean(d.always$event_dementia_3)/mean(d.never$event_dementia_3)
  mean(d.always$event_dementia_4)/mean(d.never$event_dementia_4)
  mean(d.always$event_dementia_5)/mean(d.never$event_dementia_5)
  mean(d.always$event_dementia_6)/mean(d.never$event_dementia_6)
  mean(d.always$event_dementia_7)/mean(d.never$event_dementia_7)
  mean(d.always$event_dementia_8)/mean(d.never$event_dementia_8)
  mean(d.always$event_dementia_9)/mean(d.never$event_dementia_9)


  mean(d.always$event_dementia_1) - mean(d.never$event_dementia_1)
  mean(d.always$event_dementia_2) - mean(d.never$event_dementia_2)
  mean(d.always$event_dementia_3) - mean(d.never$event_dementia_3)
  mean(d.always$event_dementia_4) - mean(d.never$event_dementia_4)
  mean(d.always$event_dementia_5) - mean(d.never$event_dementia_5)
  mean(d.always$event_dementia_6) - mean(d.never$event_dementia_6)
  mean(d.always$event_dementia_7) - mean(d.never$event_dementia_7)
  mean(d.always$event_dementia_8) - mean(d.never$event_dementia_8)
  mean(d.always$event_dementia_9) - mean(d.never$event_dementia_9)


  mean(d.always$event_dementia_10)/mean(d.never$event_dementia_10)
  mean(d.always$event_dementia_10) - mean(d.never$event_dementia_10)

