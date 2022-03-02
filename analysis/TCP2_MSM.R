getwd()  # under folder diabetes-dementia-TL-roadmap
pkg_dir <- "../diabetes-dementia-TL-roadmap/simulation_ZW/DK_trip_2021/pkgs/ltmle/cvSL_snow_skipspeedglm_20211005/ltmle-master/R/"  # load ltmle with snow parallel SL; skip speedglm runs
# pkg_dir <- "./simulation_ZW/DK_trip_2021/pkgs/ltmle/cvSL_skipspeedglm_20210915/ltmle-master/R/"  # load ltmle with multicore parallel SL; skip speedglm runs
data_path <- "../diabetes-dementia-TL-roadmap/simulation_ZW/DK_trip_2021/dt_use_backup_MSM.rds"  # example data


rm(list=ls())

library(dplyr)
library(data.table)
library(ltmle)
Sys.Date()
library(here)
source(here::here("0_config.R"))

#-----------------------------------
#-----------------------------------

# NOTE: edit these hyperparameters or inputs as needed

yr = 2011 #year to start cohort definition (depending on which drugs)
N_time = 11 #number of time points you want to look at
ncores = 10 #number of cores to use

#SL.library = c("SL.glmnet", "SL.glm")
SL.library = c("glm") #for debugging
nfolds = 10 #number of folds for CV SL
varmethod = "tmle" #variance method

#-----------------------------------
#-----------------------------------



{

  dt_use_backup <- readRDS(data_path)

  set.seed(123)
  dt_use <- dt_use_backup[sample(nrow(dt_use_backup), 2000, T), ]  # target sample size
  K <- 10  # total time points
  dt_use[, first_date_2nd_line := NULL]  # remove index dates; equivalence with censoring process
  node_names <- names(dt_use)

  data <- as.data.frame(dt_use)
}

# code summary covaraites in MSM models; now include accumulated exposure and time
{
  n <- nrow(data)
  time.points <- 10
  n_pool <- 10
  regime.matrix <- as.matrix(expand.grid(rep(list(0:1), time.points)))
  #select random subset n=100 (preference from observed frequencies, sample from empirical density)
  dim(regime.matrix)
  num.regimes <- 2^time.points
  regimes <- array(dim = c(n, time.points, num.regimes)) #n x numAnodes x numRegimes = n x time.points x 2^time.points
  # summary.measures <- array(dim = c(num.regimes, 1, 1)) #numRegimes x num.summary.measures x num.final.Ynodes = 2^time.points x 1 x 1
  summary.measures <- array(dim = c(num.regimes, 2, n_pool)) #numRegimes x num.summary.measures x num.final.Ynodes
  test.treated <- array(dim = c(num.regimes, 1, 1))
  for (i in 1:num.regimes) {
    regimes[, , i] <- matrix(regime.matrix[i, ], byrow = TRUE, nrow = n, ncol = time.points)
    for (j in 1:n_pool) {
      summary.measures[i, , j] <- c(sum(regime.matrix[i, 1:(time.points - n_pool + j)]), time.points - n_pool + j)  # accumulated exposure, time
    }
    # test.treated[i, 1, 1] <- !any(diff(which(regime.matrix[i, ] == 0)) == 1)

    ###
    # specify maximum lengths of each interruption and total interruption here
    ###
    test.treated[i, 1, 1] <- all(diff(c(0, which(regime.matrix[i, ] == 1), 11)) <= 2) &  # max length of one interruption: this first number -1; for example, 2-1 = 1 max each interruption
      sum(regime.matrix[i, ] == 0) <=5  # max total interruption: this second number; for example, max total interruption 2
  }
  colnames(test.treated) <- "if.in.group"
  test.treated[, 1, 1] %>% table

  summary.measures <- summary.measures[test.treated[, 1, 1], , ]
  for (j in 1:n_pool) {
    colnames(summary.measures[, , j]) <- c("time.on.treatment", "time")
  }
  colnames(summary.measures) <- c("time.on.treatment", "time")
}


# ltmleMSM call

package_stub("SuperLearner", "SuperLearner", SuperLearner_override, {
  testthatsomemore::package_stub("ltmle", "Estimate", Estimate_override, {
    res_RR <- ltmleMSM(data, Anodes = grep("^A1_", node_names),
                 Lnodes = paste0("L_", 1:11),
                 Ynodes = grep("^Y_", node_names),
                 Cnodes = grep("^C_", node_names),
                 survivalOutcome = T,
                 SL.library = SL.library,
                 regimes = regimes[, , test.treated[, 1, 1]],  # only the candidate regimes
                 summary.measures = summary.measures,   # corresponding summary measures for the candidate regimes
                 working.msm = "Y~time.on.treatment + time",
                 variance.method = "ic",  # direct EIC plug in; might be underestimated with positivity and rare outcomes
                 final.Ynodes = paste0("Y_", seq(to = K+1, length.out = n_pool, by = 1)),   # to pool across these nodes
                 msm.weights = "empirical",  # h weights by obs data
                 SL.cvControl = list(V = ncores),  # control CV fold numbers; might be used in paralleled version
                 # gbounds = c(0.05, 0.95),
                 estimate.time = F  # do not run on a n=50 subsample for predicting computation time
    )})})

summary(res_RR)
save(res_RR,file=paste0("./data/NOTRANSFER_glp1_MSM",N_time,".RData"))

