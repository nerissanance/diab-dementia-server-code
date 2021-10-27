---
title: "Read-Me: documentation on Diabetes Dementia Dynamics analysis"
author: "Andrew Mertens"
date: "5/10/2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## 0_config.R
sources below files and sets up some covariate names.

## 0_diabetes_dementia_functions.R

helper functions to specify LTMLE calls, subset and clean data, set dynamic treatment regimes, and custom SL algorithms


## 0_run_analysis.R

File for running all analysis subfiles (will need edited)

## 0_run_cleaning.R

File for running all cleaning files. Will eventually need edited and run once for final dataset

# 1_data_cleaning folder


## 1_prepare_dataset.R

 Input files: No prior scripts - basic raw data used
 Output files: A longitudinal data.table with covariates and datasets
                prepared  for LTMLE

## 2_subset_and_transform_for_TMLE.R

Prepare data for LTMLE of dementia

  *  pnr                      : cpr-number
  *  first_other              : first other diabetes medication (2nd line beyond metformin)
  *  secdate                  : date of first second line treatment
  *  fdato                    : birthdate
  *  first_ind                : first immigration into DK (could be used to exclude those out of Denmark)
  *  last_ind                 : last immigration into DK
  *  first_ud                 : first immigration out of DK
  *  last_ud                  : last immigration out of DK
  *  ie_type                  : Resident type 1=DK, 2=other 3=child of other
  *  opr_land                 : Country of origin
  *  sex                      : 0=female, 1=male
  *  any.malignancy           : first date in the registry where the condition is recorded (LPR)
  *  chronic.pulmonary.disease: Date
  *  dementia                 : Date- defined by diagnosis or particular group of medication
  *  heart.failure            : Date
  *  hypertension             : Date
  *  ischemic.heart.disease   : Date
  *  myocardial.infarction    : Date
  *  renal.disease            : Date
  *  doddato                  : Date of death until 4 august 2021
  *  dod_ami                  : 1 for death by AMI until 2018
  *  dod_stroke               : 1 for stroke death until 2018
  *  dod_cv                   : 1 for CV-death until 2018
  *  crea_base                : creatinine at baseline
  *  age_base                 : Baseline age
  *  egfr_base                : Baseline eGFR, MDRD formula

  *  drugdiab: name of drug - all classes of diabetes drugs
  *  pnr     : cpr_number
  *  start   : starting date
  *  end     : End date
  *  value   : Strength - alway "1" - for treatment given

  *  pnr                      : cpr number
  *  any.malignancy           
  *  chronic.pulmonary.disease
  *  dementia                 : Diagnosis or treatment date
  *  heart.failure            :
  *  hypertension             :
  *  ischemic.heart.disease   :
  *  myocardial.infarction    :
  *  renal.disease            :

## 3_define_covariates.R
Save vectors of baseline and time-dependent covariate names

## 4_tabulate data.R

tabulations of the exposure data

## 5_tabulate regimes.R




# 2_analysis

## glp1_vs_sglt2_static

comparing glp1 and sglt2 as static (continuous use) regimes

## glp1_vs_sglt_stochastic

comparing glp1 and sglt2 as stocahstic regimes allowing for 1 time bin gaps in use


# 3_results

# 4_reports

- EDA: tabulations of longitudinal variables
- table 1 : table 1 of baseline values





# Example code

## 3_LTMLE_full.R
Benchmark SL and contrast static regimes of continuous GLP1 use vs continuous SGLT2 use

## 4_LTMLE.R


## Scripts to make
  *  MSM
  *  Dynamic treatment
  *  Repeat of above scripts with:
      *  GLP1 vs any second-line
      *  Death outcome
      *other sensitivity analyses
  *  Rmd script of all relevant results to export off the server
