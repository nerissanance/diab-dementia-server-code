
rm(list=ls())
source(paste0(here::here(),"/analysis/ltmle_Estimate_update.R"))

# if (!require(testthatsomemore)) {
#   if (!require(devtools)) install.packages('devtools'); require(devtools)
#   install_github('robertzk/testthatsomemore')
# }

library(ltmle)
library(SuperLearner)

rexpit <- function(x) rbinom(n=length(x), size=1, prob=plogis(x))
n <- 1000
W1 <- rnorm(n)
W2 <- rnorm(n)
W3 <- rnorm(n)
A <- rexpit(-1 + 2 * W1 - 3*W2 + W3)
Y <- rexpit(W1 + W2 + W3 + A)
data <- data.frame(W1, W2, W3, A, Y)

# result1 <- ltmle(data, Anodes="A", Ynodes="Y", abar=list(1,0), estimate.time = F, SL.library = "SL.glmnet")
# summary(result1)

SuperLearner_override <- function(Y, X, newX = NULL, family = gaussian(), SL.library,
                                   method = "method.NNLS", id = NULL, verbose = FALSE, control = list(),
                                   cvControl = list(), obsWeights = NULL, env = parent.frame()) {
  stopifnot(identical(SL.library, "SL.glmnet"))

  res <- SL.glmnet(Y, X, newX, family, obsWeights, id)
  list(model=res, SL.predict = res$pred)
}

# testthatsomemore::package_stub("SuperLearner", "SuperLearner", SuperLearner_override, {
#   testthatsomemore::package_stub("ltmle", "Estimate", Estimate_override, {
#   result2 <- ltmle(data, Anodes="A", Ynodes="Y", abar=1, estimate.time = F, SL.library = "SL.glmnet")
#   })
# })
#
# summary(result2)

testthatsomemore::package_stub("SuperLearner", "SuperLearner", SuperLearner_override, {
  testthatsomemore::package_stub("ltmle", "Estimate", Estimate_override, {
  result3 <- ltmle(data, Anodes="A", Ynodes="Y", abar=list(1,0), estimate.time = F, SL.library = "SL.glmnet")
  }
  )
}
)
summary(result3)

# testthatsomemore::package_stub("SuperLearner", "SuperLearner", SuperLearner_override, {
#   result4 <- ltmle(data, Anodes="A", Ynodes="Y", abar=list(1,0), estimate.time = F, SL.library = "glm")
# }
# )
# summary(result4)


# SL.glmnet
# SuperLearner::SuperLearner


# result_glmnet <- cv.glmnet(x=as.matrix(data[,-6]), y=data$Y)
# result5 <- predict(result_glmnet, newx = as.matrix(data[,-6]), type = "response",  "lambda.min")
# summary(result5)



#this is what is returned by SL

# out <- list(call = call, libraryNames = libraryNames, SL.library = library,
#             SL.predict = getPred, coef = coef, library.predict = predY,
#             Z = Z, cvRisk = getCoef$cvRisk, family = family, fitLibrary = get("fitLibrary",
#                                                                               envir = fitLibEnv), cvFitLibrary = cvFitLibrary,
#             varNames = varNames, validRows = validRows, method = method,
#             whichScreen = whichScreen, control = control, cvControl = cvControl,
#             errorsInCVLibrary = errorsInCVLibrary, errorsInLibrary = errorsInLibrary,
#             metaOptimizer = getCoef$optimizer, env = env, times = times)

#Do I need other output? Look at LTMLE code





#it looks like this function within ltmle requires model to predict from
#get the actual function from LTMLE

# PredictOnly <- function(newdata1){
#   if (class(m)[1] == "no.Y.variation")
#     return(rep(m$Y.value, nrow(newdata1)))
#   if (use.glm) {
#
#     #print(class(m))
#     if(class(m)[1]=="glm"){
#       print("glm pred:")
#       SuppressGivenWarnings(pred <- predict(m, newdata1,
#                                             type), "prediction from a rank-deficient fit may be misleading")
#       print(pred[1:10])
#     }else{
#       print("glmnet pred:")
#       print(summary(m))
#       print(head(newdata1))
#       # print( class(newdata1))
#       # newdata1<-as.data.frame(newdata1)
#       # newdata1$Intercept <- 1
#       pred <- glmnetUtils:::predict.cv.glmnet.formula(m, newdata = newdata1, s="lambda.min")
#       print(pred[1:10])
#     }
#
#   }
#   else {
#     newX.list <- GetNewX(newdata1)
#     pred <- ProcessSLPrediction(predict(m, newX.list$newX,
#                                         X.subset, Y.subset, onlySL = TRUE)$pred, newX.list$new.subs,
#                                 try.result = NULL)
#   }
#   return(pred)
# }








# binary outcome
set.seed(1)
N <- 200
X <- matrix(rnorm(N*10), N, 10)
X <- as.data.frame(X)
Y <- rbinom(N, 1, plogis(.2*X[, 1] + .1*X[, 2] - .2*X[, 3] +
                           .1*X[, 3]*X[, 4] - .2*abs(X[, 4])))

SL.library <- c("SL.glmnet", "SL.glm", "SL.mean")

newX <- X

# least squares loss function
test <- SuperLearner(Y = Y, X = X, SL.library = SL.library,
                          verbose = TRUE, method = "method.NNLS", family = binomial())
SL.pred <- predict(test, newdata = newX, X=X, Y=Y, onlySL = T)$pred


#See if I can get predictions from glmnet of the same form
test.glmnet <- glmnet::cv.glmnet(y = Y, x = as.matrix(X), family = "binomial")

# predict.SuperLearner
# function (object, newdata, X = NULL, Y = NULL, onlySL = FALSE,

#Should I be using the response type?
glmnet.pred <- predict(test.glmnet, newx = as.matrix(newX), s="lambda.min", type="response")

summary(SL.pred)
summary(glmnet.pred)

m <- SuperLearner_override(Y = Y, X = as.matrix(X), newX = newX, family = binomial(), SL.library = "SL.glmnet")


# Y = Y
# newX = X = as.matrix(X)
# family = binomial()
# SL.library = "SL.glmnet"
# method = "method.NNLS"
# id = NULL
# verbose = FALSE
# control = list()
# cvControl = list()
# obsWeights = NULL
# env = parent.frame()
#
#   res <- SL.glmnet(Y, X, newX=newX, family, obsWeights, id)
#
#   alpha = 1
#   nfolds = 10
#   nlambda = 100
#   useMin = TRUE
#   loss = "deviance"
#
#   #list(model=res, SL.predict = res$pred)
#
#   fitCV <- glmnet::cv.glmnet(x = X, y = Y, weights = obsWeights,
#                              lambda = NULL, type.measure = loss, nfolds = nfolds,
#                              family = family$family, alpha = alpha, nlambda = nlambda)
#   pred <- predict(fitCV, newx = newX, type = "response",
#                   s = ifelse(useMin, "lambda.min", "lambda.1se"))
#   fit <- list(object = fitCV, useMin = useMin)
#   class(fit) <- "SL.glmnet"
#   out <- list(pred = pred, fit = fit)


# predict.glmnet
# function (object, newx, s = NULL, type = c("link", "response",
#                                            "coefficients", "nonzero", "class"), exact = FALSE,
#           newoffset, ...)

library(glmnet)
predict.glmnet


head(SL.pred)
head(glmnet.pred)

class(SL.pred)
class(glmnet.pred)




# Need to try to get this to work outside of the function

# res <- SL.glmnet(Y, X, newX, family, obsWeights, id)
# list(m=res, SL.predict = res$pred)

pred = predict(test.glmnet,  as.matrix(newX), s="lambda.min")
new.subs = c(1:nrow(newX))
try.result = NULL


#Need o set newdata to get function to work
newdata=as.matrix(newX)

ProcessSLPrediction <- function(pred, new.subs, try.result) {
  if (inherits(try.result, "try-error")) {
    stop(paste("\n\nError occured during call to SuperLearner:\n",
               form, GetSLStopMsg(Y.subset), "\n The error reported is:\n",
               try.result))
  }
  if (all(is.na(pred))) {
    stop(paste("\n\n Unexpected error: SuperLearner returned all NAs during regression:\n",
               form, GetSLStopMsg(Y.subset)))
  }
  predicted.values <- rep(NA, nrow(newdata))
  predicted.values[new.subs] <- pred
  if (max(predicted.values, na.rm = T) > 1 || min(predicted.values,
                                                  na.rm = T) < 0) {
    msg <- paste("SuperLearner returned predicted.values > 1 or < 0: [min, max] = [",
                 min(predicted.values, na.rm = T), ",",
                 max(predicted.values, na.rm = T), "]. Bounding to [0,1]")
    warning(msg)
    predicted.values <- Bound(predicted.values, bounds = c(0,
                                                           1))
  }

  #return((predicted.values))
  return(ValuesByType(predicted.values))
}

ValuesByType <- function(x) {
  if (type == "link") {
    stopifnot(family$family %in% c("binomial",
                                   "quasibinomial"))
    qlogis(Bound(x, bounds = c(1e-04, 0.9999)))
  }
  else {
    x
  }
}

PredictOnly <- function(newdata1) {
  if (class(m)[1] == "no.Y.variation")
    return(rep(m$Y.value, nrow(newdata1)))
  if (use.glm) {
    SuppressGivenWarnings(pred <- predict(m, newdata1,
                                          type), "prediction from a rank-deficient fit may be misleading")
  }
  else {
    newX.list <- GetNewX(newdata1)
    # pred <- ProcessSLPrediction(predict(m, newX.list$newX,
    #                                     X.subset, Y.subset, onlySL = TRUE)$pred, newX.list$new.subs,
    #                             try.result = NULL)
    cat(class(m))
    cat(class(m$model))
    cat(class(newX.list$newX))
    cat(class(newX.list$new.subs))
    cat(dim(newX.list$newX))
    cat(dim(X.subset))

    pred <- ProcessSLPrediction(predict(m$model, as.matrix(newX.list$newX), s="lambda.min", type="response"),
                                newX.list$new.subs,
                                try.result = NULL)


    cat(pred)


  }
  return(pred)
}


GetNewX <- function(newdata1) {
  new.mod.frame <- model.frame(f, data = newdata1, drop.unused.levels = TRUE,
                               na.action = na.pass)
  newX.temp <- model.matrix(terms(f), new.mod.frame)
  if (!use.glm) {
    colnames(newX.temp) <- paste0("Xx.", 1:ncol(newX.temp))
  }
  new.subs <- !rowAnyMissings(newX.temp)
  newX <- as.data.frame(newX.temp[new.subs, , drop = FALSE])
  if (ncol(X) == 1) {
    X.subset <<- cbind(X.subset, ltmle.added.constant = 1)
    newX <- cbind(newX, ltmle.added.constant = 1)
  }
  return(list(newX = newX, new.subs = new.subs))
}


#SuperLearner_override

# ProcessSLPrediction(glmnet::predict.glmnet(glmnet.pred, newX.list$newX, s="lambda.min")$pred,
#                     newX.list$new.subs,
#                     try.result = NULL)

#keep debugging here
#glmnet::predict.glmnet(test.glmnet, newx = as.matrix(newX), s="lambda.min")

#predict(test.glmnet, newx = as.matrix(newX), s="lambda.min")



#Need to set newdata to get function to work

ProcessSLPrediction(predict(test.glmnet,  as.matrix(newX), s="lambda.min"),
                    new.subs = c(1:nrow(newX)),
                    #newX.list$new.subs,
                    try.result = NULL)

# Find function ValuesByType

use.glm=F
f=as.formula("Y~V1+V2+V3")
X.subset=X
Y.subset=Y
PredictOnly(as.data.frame(newdata))



newX.list <- GetNewX(as.data.frame(newdata))
# pred <- ProcessSLPrediction(predict(m, newX.list$newX,
#                                     X.subset, Y.subset, onlySL = TRUE)$pred, newX.list$new.subs,
#                             try.result = NULL)
cat(class(m))
cat(class(m$model))
cat(class(newX.list$newX))
cat(class(newX.list$new.subs))
cat(dim(newX.list$newX))
cat(dim(X.subset))

predict(m$model$fit, as.matrix(newX.list$newX), s="lambda.min", type="response")

pred <- ProcessSLPrediction(predict(m$model, as.matrix(newX.list$newX), s="lambda.min", type="response"),
                            newX.list$new.subs,
                            try.result = NULL)
