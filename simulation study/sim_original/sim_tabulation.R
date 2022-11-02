
rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))



#> prop.always[2]
# TRUE
# 2.3215%
# > prop.never[2]
# TRUE
# 5.9067%

#observed
#1.91974

#XXXXXXXXXXXXXXXXXXXXXXXX
#WHY IS THE SIMULATED DATA PREVALENCE SO MUCH LOWER THAN THE TRUTH!!!
#XXXXXXXXXXXXXXXXXXXXXXXX

gc()
d_wide_list <- readRDS(file=here("data/simulated_data_list.RDS"))
d_wide_list <- d_wide_list[1:100]

prev <- rep(NA, 100)
for(i in 1:100){
  prev[i] <- prop.table(table(d_wide_list[[i]]$event_dementia_10))[2]*100
  A<-1
  d_A1 <- d_wide_list[[i]] %>% filter(glp1_1==!!(A), glp1_2==!!(A), glp1_3==!!(A), glp1_4==!!(A), glp1_5==!!(A), glp1_6==!!(A), glp1_7==!!(A), glp1_8==!!(A), glp1_9==!!(A), glp1_10==!!(A))

}
mean(prev)

d <- d_wide_list[[1]]
prop.table(table(d$event_dementia_10))*100
table(d$event_dementia_9, d$event_dementia_10)
#d1 <- d %>%
