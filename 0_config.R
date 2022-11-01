
#clean up memory
gc()

#load required libraries
try(library(heaven))
library(tidyverse)
library(data.table)
library(SuperLearner)
library(ltmle)
library(snow)
library(caret)
library(hal9001)
library(here)
library(foreach)
library(glmnet)
library(testthatsomemore)
library(parallel)
library(doSNOW)


#load functions
source(here::here("0_diabetes_dementia_functions.R"))

#source(here::here("0_glmnet_specs.R"))


#Set universal SuperLearner library
#Sl.lib = "glm"
Sl.lib = "SL.glmnet"


#baseline covariates
baseline_vars = c("ie_type","age_base","sex", "code5txt", "quartile_income")
# baseline_vars = c("ie_type","age_base","CREA_base" ,"sex", "opr_land", "egfr_base","code5txt", "quartile_income")

#Longitudinal covariates
long_covariates = c("insulin_","any.malignancy_", "chronic.pulmonary.disease_","hypertension_",
                    "myocardial.infarction_", "ischemic.heart.disease_","heart.failure_", "renal.disease_", "sglt2_inhib_"   )
