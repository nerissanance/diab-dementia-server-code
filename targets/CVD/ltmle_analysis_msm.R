
if(FALSE){
  spec_ltmle=tar_read(CVD_spec_TCP1)
  N_time=5
  max_interruptions=4
  varmethod="ic"
  SL.library=c("SL.glmnet")
  
}
ltmle_analysis_msm <- function(spec_ltmle,N_time,max_interruptions,SL.library,varmethod){

  
# code summary covariates in MSM models; now include accumulated exposure and time
n <- nrow(spec_ltmle$data)
time.points <- (N_time-1)
n_pool <- time.points #number of Y nodes to pool over, -1 because event_0 has no events
regime.matrix <- as.matrix(expand.grid(rep(list(0:1), time.points)))
dim(regime.matrix)
num.regimes <- 2^time.points
regimes <- array(dim = c(n, time.points, num.regimes)) #n x numAnodes x numRegimes = n x time.points x 2^time.points
# summary.measures <- array(dim = c(num.regimes, 1, 1)) #numRegimes x num.summary.measures x num.final.Ynodes = 2^time.points x 1 x 1
summary.measures <- array(dim = c(num.regimes, 2, n_pool)) #numRegimes x num.summary.measures x num.final.Ynodes

for (i in 1:num.regimes) {
  regimes[, , i] <- matrix(regime.matrix[i, ], byrow = TRUE, nrow = n, ncol = time.points)
 
for (j in 1:n_pool) {
    summary.measures[i, , j] <- c(sum(regime.matrix[i, 1:(time.points - n_pool + j)]), time.points - n_pool + j)  # accumulated exposure, time
} 
}
  # test.treated[i, 1, 1] <- !any(diff(which(regime.matrix[i, ] == 0)) == 1)
  ###
  # specify maximum lengths of each interruption and total interruption here
  ###
  # test.treated[i, 1, 1] <- all(diff(c(0, which(regime.matrix[i, ] == 1), N_time)) <= 2) &  # max length of one interruption: this first number -1; for example, 2-1 = 1 max each interruption
    # sum(regime.matrix[i, ] == 0) <=max_interruptions  # max total interruption: this second number; for example, max total interruption 2
# }
# colnames(test.treated) <- "if.in.group"
# summary.measures <- summary.measures[test.treated[, 1, 1], , ]

for (j in 1:(n_pool)) {
  colnames(summary.measures[, , j]) <- c("time.on.treatment", "time")
}
colnames(summary.measures) <- c("time.on.treatment", "time")


# ltmleMSM call
res_RR <- ltmleMSM(spec_ltmle$data,
                   Anodes = spec_ltmle$Anodes,
                   Cnodes = spec_ltmle$Cnodes,
                   Lnodes = spec_ltmle$Lnodes,
                   Ynodes = spec_ltmle$Ynodes,
                   survivalOutcome = T,
                   SL.library = SL.library,
                   regimes = regimes,#[, , test.treated[, 1, 1]],  # only the candidate regimes
                   summary.measures = summary.measures,   # corresponding summary measures for the candidate regimes
                   working.msm = "Y~time.on.treatment * time",
                   variance.method = varmethod,  # direct EIC plug in; might be underestimated with positivity and rare outcomes
                   final.Ynodes = paste0("MACE_", c(1:(N_time-1))),   # to pool across these nodes
                   msm.weights = "empirical",  # h weights by obs data
                   # gbounds = c(0.05, 0.95),
                   estimate.time = F  # do not run on a n=50 subsample for predicting computation time
)
return(res_RR)
}