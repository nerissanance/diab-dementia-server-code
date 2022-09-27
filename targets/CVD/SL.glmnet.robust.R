SL.glmnet.robust <- function (Y, X, newX, family, obsWeights, id, alpha = 1, nfolds = 10, 
          nlambda = 100, useMin = TRUE, loss = "deviance", ...){

  if(length(unique((Y)))>2){
    out <- try(SL.glmnet(Y, X, newX,  family=gaussian(), obsWeights, id, alpha = alpha, 
                         nfolds = nfolds, 
                       nlambda = nlambda, useMin = TRUE))
  }else{
    out <- try(SL.glmnet(Y, X, newX, family, obsWeights, id,alpha = alpha,
                     nfolds=nfolds,nlambda = nlambda, useMin = TRUE))
  }
    if(is.null(out)){
    out <- SL.mean(Y, X, newX, family, obsWeights, id)
    print("returning SL.mean estimates due to glmnet fail")
  }
  return(out)
}