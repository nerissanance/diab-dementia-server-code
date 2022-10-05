
#------------------------------------------
# glmnet-only function for LTMLE that avoids
# SuperLearner cross-validation
#------------------------------------------


SuperLearner_override <- function(Y, X, newX = NULL, family = gaussian(), SL.library, 
                                  method = "method.NNLS", id = NULL, verbose = FALSE, control = list(),
                                  cvControl = list(), obsWeights = NULL, env = parent.frame()) {
  stopifnot(identical(SL.library, "SL.glmnet"))
  
  res <- NULL
  if(length(unique((Y)))>2){
    try(res <- SL.glmnet(Y, X, newX, family=gaussian(), obsWeights, id,nfolds=5))
  }else{
    try(res <- SL.glmnet(Y, X, newX, family, obsWeights, id,nfolds=5))
    
  }
  if(is.null(res)){
    
    res <- SL.mean(Y, X, newX, family, obsWeights, id)
    cat("glmnet failed!!!")
  }
  
  list(model=res$fit, SL.predict = res$pred)
}


SuperLearner_override_1se<- function(Y, X, newX = NULL, family = gaussian(), SL.library, 
                                     method = "method.NNLS", id = NULL, verbose = FALSE, control = list(),
                                     cvControl = list(), obsWeights = NULL, env = parent.frame()) {
  stopifnot(identical(SL.library, "SL.glmnet"))
  
  res <- NULL
  try(res <- SL.glmnet(Y, X, newX, family, obsWeights, id, useMin=FALSE))
  if(is.null(res)){res <- SL.mean(Y, X, newX, family, obsWeights, id)}
  
  list(model=res$fit, SL.predict = res$pred)
}