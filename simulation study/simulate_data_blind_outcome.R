

rm(list=ls())
library(lava)
library(tidyverse)
library(data.table)
source(paste0(here::here(),"/synthesizeDD.R"))

cc <- fread(paste0(here::here(),"/data/coefficients.txt"))


#artificially set relationships with dementia outcomes
colnames(cc)
cc$var


#make a dataset with no censoring or death
cc<-as.data.frame(cc)
cc <- cc %>% filter(!grepl("censor_",var), !grepl("event_death",var)) %>%  select(!starts_with("censor_"),!starts_with("event_death"))

#Remove Y-A association
cc[grepl("event_dem",cc$var),grepl("glp1_",colnames(cc))] <- NA

# #need to realistically simulate outcome:
# data.frame(time=1,
#            var=var
#            time=
#            var=
#            X.Intercept.
#            ie_type
#            age_base= ,
#            sex= ,
#            code5txt
#            quartile_income
#            insulin_1
#            any.malignancy_1
#            chronic.pulmonary.disease_1
#            hypertension_1
#            myocardial.infarction_1
#            ischemic.heart.disease_1
#           heart.failure_1
#            renal.disease_1
#            glp1_1
#            sglt2_inhib_1
#            )



#make a dataset with no censoring or death


#update coefficients to make dementia outcomes
rows <- grep("dementia",cc$var)


u <- synthesizeDD(cc)
d <- sim(u,100000)
head(d)


table(d$glp1_1)
table(d$event_dementia_1)



table(d$glp1_2)
table(d$event_dementia_2)


table(d$event_dementia_1, d$event_dementia_2)
table(d$glp1_2, d$event_dementia_2)
table(d$glp1_1==1 & d$glp1_2==1  & d$glp1_3==1, d$event_dementia_3)
