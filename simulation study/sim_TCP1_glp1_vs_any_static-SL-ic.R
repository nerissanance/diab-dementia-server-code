
rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))




gc()
d_wide_list <- readRDS(file=here("data/simulated_data_list.RDS"))

d_wide_list <- d_wide_list[1:50]
gc()




SL.glmnet.robust <- function (Y, X, newX, family, obsWeights, id, alpha = 1, nfolds = 10,
          nlambda = 100, useMin = TRUE, loss = "deviance", ...){
  #.SL.require("glmnet")
  if (!is.matrix(X)) {
    X <- model.matrix(~-1 + ., X)
    newX <- model.matrix(~-1 + ., newX)
  }
  fitCV <- NULL
  try(fitCV <- glmnet::cv.glmnet(x = X, y = Y, weights = obsWeights,
                             lambda = NULL, type.measure = loss, nfolds = nfolds,
                             family = family$family, alpha = alpha, nlambda = nlambda,
                             ...))
  if(is.null(fitCV)){
    meanY <- weighted.mean(Y, w = obsWeights)
    pred <- rep.int(meanY, times = nrow(newX))
    fit <- list(object = meanY)
    out <- list(pred = pred, fit = fit)
    class(out$fit) <- c("SL.mean")
    return(out)
  }else{
    pred <- predict(fitCV, newx = newX, type = "response", s = ifelse(useMin, "lambda.min", "lambda.1se"))
    fit <- list(object = fitCV, useMin = useMin)
    class(fit) <- "SL.glmnet"
    out <- list(pred = pred, fit = fit)
    return(out)
  }

}


SL.EN.robust <- function(..., alpha = 0.5){
  SL.glmnet.robust(...,alpha = 0.5)
}


SL.lib <- c("SL.mean", "SL.glm", "SL.EN.robust", "SL.glmnet.robust")

#test
#res <- run_ltmle(d_wide_list[[1]], varmethod = "ic", resdf=NULL, SL.library = SL.lib, Qint=FALSE)

int.start.time <- Sys.time()
resdf_SL_ic <- foreach(i = 1:length(d_wide_list), .combine = 'bind_rows', .errorhandling = 'remove') %dopar% {
  res <- NULL
  try(res <- run_ltmle(d_wide_list[[i]], varmethod = "ic", resdf=NULL, SL.library = SL.lib, Qint=FALSE))
  return(res)
}
int.SLd.time <- Sys.time()
difftime(int.SLd.time, int.start.time, units="mins")




gc()
saveRDS(resdf_SL_ic, paste0(here::here(),"/data/sim_res_SL_ic.RDS"))



