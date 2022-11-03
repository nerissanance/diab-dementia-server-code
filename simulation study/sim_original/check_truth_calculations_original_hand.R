

rm(list=ls())
library(lava)
library(tidyverse)
library(data.table)
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))
source(paste0(here::here(),"/synthesizeDD.R"))
options(scipen = 30)

cc <- fread(paste0(here::here(),"/data/coefficients.txt"))

#make a dataset with no censoring or death
cc<-as.data.frame(cc)
# cc[,-c(1:3)] <- NA
# cc <- cc %>% filter(grepl("event_dementia",cc$var) | grepl("glp1",cc$var))
# cc <- cc[,c(1:3, which(grepl("glp1",colnames(cc))))]
#cc[!grepl("event_dementia",cc$var) & !grepl("glp1",cc$var),colnames(cc)=="(Intercept)"] <- NA


#look at Y-A association
cc[cc$var=="event_dementia_1",colnames(cc)=="glp1_1"]
cc[cc$var=="event_dementia_2",colnames(cc)=="glp1_2"]
cc[cc$var=="event_dementia_3",colnames(cc)=="glp1_3"]
cc[cc$var=="event_dementia_4",colnames(cc)=="glp1_4"]
cc[cc$var=="event_dementia_5",colnames(cc)=="glp1_5"]
cc[cc$var=="event_dementia_6",colnames(cc)=="glp1_6"]
cc[cc$var=="event_dementia_7",colnames(cc)=="glp1_7"]
cc[cc$var=="event_dementia_8",colnames(cc)=="glp1_8"]
cc[cc$var=="event_dementia_9",colnames(cc)=="glp1_9"]
cc[cc$var=="event_dementia_10",colnames(cc)=="glp1_10"]

#look at A-death association
cc[cc$var=="event_death_1",colnames(cc)=="glp1_1"]
cc[cc$var=="event_death_2",colnames(cc)=="glp1_2"]
cc[cc$var=="event_death_3",colnames(cc)=="glp1_3"]
cc[cc$var=="event_death_4",colnames(cc)=="glp1_4"]
cc[cc$var=="event_death_5",colnames(cc)=="glp1_5"]
cc[cc$var=="event_death_6",colnames(cc)=="glp1_6"]
cc[cc$var=="event_death_7",colnames(cc)=="glp1_7"]
cc[cc$var=="event_death_8",colnames(cc)=="glp1_8"]
cc[cc$var=="event_death_9",colnames(cc)=="glp1_9"]
cc[cc$var=="event_death_10",colnames(cc)=="glp1_10"]


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

# Y1death_a1_t1 <- logit2prob(sum(as.numeric(cc[cc$var=="event_death_1",-c(1:2)]), na.rm =  T))
# Y1death_a0_t1 <- logit2prob(sum(as.numeric(cc[cc$var=="event_death_1",-c(1:2)]), na.rm =  T) - cc[cc$var=="event_death_1",which(colnames(cc)=="glp1_1")])
# Y1death_a1_t2 <- logit2prob(sum(as.numeric(cc[cc$var=="event_death_2",-c(1:2)]), na.rm =  T))
# Y1death_a0_t2 <- logit2prob(sum(as.numeric(cc[cc$var=="event_death_2",-c(1:2)]), na.rm =  T) - cc[cc$var=="event_death_2",which(colnames(cc)=="glp1_2")])
# Y1death_a1_t3 <- logit2prob(sum(as.numeric(cc[cc$var=="event_death_3",-c(1:2)]), na.rm =  T))
# Y1death_a0_t3 <- logit2prob(sum(as.numeric(cc[cc$var=="event_death_3",-c(1:2)]), na.rm =  T) - cc[cc$var=="event_death_3",which(colnames(cc)=="glp1_3")])
# Y1death_a1_t4 <- logit2prob(sum(as.numeric(cc[cc$var=="event_death_4",-c(1:2)]), na.rm =  T))
# Y1death_a0_t4 <- logit2prob(sum(as.numeric(cc[cc$var=="event_death_4",-c(1:2)]), na.rm =  T) - cc[cc$var=="event_death_4",which(colnames(cc)=="glp1_4")])
# Y1death_a1_t5 <- logit2prob(sum(as.numeric(cc[cc$var=="event_death_5",-c(1:2)]), na.rm =  T))
# Y1death_a0_t5 <- logit2prob(sum(as.numeric(cc[cc$var=="event_death_5",-c(1:2)]), na.rm =  T) - cc[cc$var=="event_death_5",which(colnames(cc)=="glp1_5")])
# Y1death_a1_t6 <- logit2prob(sum(as.numeric(cc[cc$var=="event_death_6",-c(1:2)]), na.rm =  T))
# Y1death_a0_t6 <- logit2prob(sum(as.numeric(cc[cc$var=="event_death_6",-c(1:2)]), na.rm =  T) - cc[cc$var=="event_death_6",which(colnames(cc)=="glp1_6")])
# Y1death_a1_t7 <- logit2prob(sum(as.numeric(cc[cc$var=="event_death_7",-c(1:2)]), na.rm =  T))
# Y1death_a0_t7 <- logit2prob(sum(as.numeric(cc[cc$var=="event_death_7",-c(1:2)]), na.rm =  T) - cc[cc$var=="event_death_7",which(colnames(cc)=="glp1_7")])
# Y1death_a1_t8 <- logit2prob(sum(as.numeric(cc[cc$var=="event_death_8",-c(1:2)]), na.rm =  T))
# Y1death_a0_t8 <- logit2prob(sum(as.numeric(cc[cc$var=="event_death_8",-c(1:2)]), na.rm =  T) - cc[cc$var=="event_death_8",which(colnames(cc)=="glp1_8")])
# Y1death_a1_t9 <- logit2prob(sum(as.numeric(cc[cc$var=="event_death_9",-c(1:2)]), na.rm =  T))
# Y1death_a0_t9 <- logit2prob(sum(as.numeric(cc[cc$var=="event_death_9",-c(1:2)]), na.rm =  T) - cc[cc$var=="event_death_9",which(colnames(cc)=="glp1_9")])
# Y1death_a1_t10 <- logit2prob(sum(as.numeric(cc[cc$var=="event_death_10",-c(1:2)]), na.rm =  T))
# Y1death_a0_t10 <- logit2prob(sum(as.numeric(cc[cc$var=="event_death_10",-c(1:2)]), na.rm =  T) - cc[cc$var=="event_death_10",which(colnames(cc)=="glp1_10")])
#
#
# Y0_a1_t1 <- Y0_a1_t1 -Y1death_a1_t1
# Y0_a1_t2 <- Y0_a1_t2 -Y1death_a1_t2
# Y0_a1_t3 <- Y0_a1_t3 -Y1death_a1_t3
# Y0_a1_t4 <- Y0_a1_t4 -Y1death_a1_t4
# Y0_a1_t5 <- Y0_a1_t5 -Y1death_a1_t5
# Y0_a1_t6 <- Y0_a1_t6 -Y1death_a1_t6
# Y0_a1_t7 <- Y0_a1_t7 -Y1death_a1_t7
# Y0_a1_t8 <- Y0_a1_t8 -Y1death_a1_t8
# Y0_a1_t9 <- Y0_a1_t9 -Y1death_a1_t9
# Y0_a1_t10 <- Y0_a1_t10 -Y1death_a1_t10
#
# Y0_a0_t1 <- Y0_a0_t1 -Y1death_a0_t1
# Y0_a0_t2 <- Y0_a0_t2 -Y1death_a0_t2
# Y0_a0_t3 <- Y0_a0_t3 -Y1death_a0_t3
# Y0_a0_t4 <- Y0_a0_t4 -Y1death_a0_t4
# Y0_a0_t5 <- Y0_a0_t5 -Y1death_a0_t5
# Y0_a0_t6 <- Y0_a0_t6 -Y1death_a0_t6
# Y0_a0_t7 <- Y0_a0_t7 -Y1death_a0_t7
# Y0_a0_t8 <- Y0_a0_t8 -Y1death_a0_t8
# Y0_a0_t9 <- Y0_a0_t9 -Y1death_a0_t9
# Y0_a0_t10 <- Y0_a0_t10 -Y1death_a0_t10



#XXXX NOTE!!!!
#Also, see what happens when drop out censoring in the truth calculation
#Also, run sim with no censoring/death
#WHY IS "TRUTH" the same at t1 without subtracting death as it is in the simulated truth -Because if they co-occur, we assume dementia happened first.
#need to subtract history of death at each time point
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# CI_A1 <- 1-(Y0_a1_t1*Y0_a1_t2*Y0_a1_t3*Y0_a1_t4*Y0_a1_t5*Y0_a1_t6*Y0_a1_t7*Y0_a1_t8*Y0_a1_t9*Y0_a1_t10)
# CI_A0 <-1-(Y0_a0_t1*Y0_a0_t2*Y0_a0_t3*Y0_a0_t4*Y0_a0_t5*Y0_a0_t6*Y0_a0_t7*Y0_a0_t8*Y0_a0_t9*Y0_a0_t10)
#
# CI_A1/CI_A0
# CI_A1-CI_A0


CI_A1_t1 <- 1-(Y0_a1_t1)
CI_A0_t1 <-1-(Y0_a0_t1)
CI_A1_t1/CI_A0_t1
CI_A1_t1-CI_A0_t1




CI_A1_t8 <- 1-(Y0_a1_t1*Y0_a1_t2*Y0_a1_t3*Y0_a1_t4*Y0_a1_t5*Y0_a1_t6*Y0_a1_t7*Y0_a1_t8)
CI_A0_t8 <-1-(Y0_a0_t1*Y0_a0_t2*Y0_a0_t3*Y0_a0_t4*Y0_a0_t5*Y0_a0_t6*Y0_a0_t7*Y0_a0_t8)
CI_A1_t8/CI_A0_t8
CI_A1_t8-CI_A0_t8

CI_A1_t9 <- 1-(Y0_a1_t1*Y0_a1_t2*Y0_a1_t3*Y0_a1_t4*Y0_a1_t5*Y0_a1_t6*Y0_a1_t7*Y0_a1_t8*Y0_a1_t9)
CI_A0_t9 <-1-(Y0_a0_t1*Y0_a0_t2*Y0_a0_t3*Y0_a0_t4*Y0_a0_t5*Y0_a0_t6*Y0_a0_t7*Y0_a0_t8*Y0_a0_t9)
CI_A1_t9/CI_A0_t9
CI_A1_t9-CI_A0_t9


#Def wrong... should be increasing CI over time




