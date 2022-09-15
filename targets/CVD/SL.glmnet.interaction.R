# new glmnet wrapper that creates interactions

SL.glmnet.interaction <- function (Y, X, newX, family, obsWeights, id, alpha = 1, nfolds = 10, 
          nlambda = 100, useMin = TRUE, loss = "deviance", ...) 
{
  .SL.require("glmnet")
  if (!is.matrix(X)) {
    X <- model.matrix(~-1 + .^2, X)#^2 adds in interaction terms
    newX <- model.matrix(~-1 + .^2, newX)
  }
  if(length(unique((Y)))>2){
    fitCV <- glmnet::cv.glmnet(x = X, y = Y, weights = obsWeights, 
                               lambda = NULL, type.measure = loss, nfolds = nfolds, 
                               family = gaussian(), alpha = alpha, nlambda = nlambda, 
                               ...)
    pred <- predict(fitCV, newx = newX, type = "response", 
                    s = ifelse(useMin, "lambda.min", "lambda.1se"))
    fit <- list(object = fitCV, useMin = useMin)
    class(fit) <- "SL.glmnet"
    out <- list(pred = pred, fit = fit)
  }
  else{
  fitCV <- glmnet::cv.glmnet(x = X, y = Y, weights = obsWeights, 
                             lambda = NULL, type.measure = loss, nfolds = nfolds, 
                             family = family$family, alpha = alpha, nlambda = nlambda, 
                             ...)
  pred <- predict(fitCV, newx = newX, type = "response", 
                  s = ifelse(useMin, "lambda.min", "lambda.1se"))
  fit <- list(object = fitCV, useMin = useMin)
  class(fit) <- "SL.glmnet"
  out <- list(pred = pred, fit = fit)
  }
  
  if(is.null(out)){
    out <- SL.mean(Y, X, newX, family, obsWeights, id)
    print("returning SL.mean estimates due to glmnet fail")
  }
  return(out)
}