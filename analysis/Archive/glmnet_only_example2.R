if (!require(testthatsomemore)) {
  if (!require(devtools)) install.packages('devtools'); require(devtools)
  install_github('robertzk/testthatsomemore')
}

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

result1 <- ltmle(data, Anodes="A", Ynodes="Y", abar=list(1,0), estimate.time = F, SL.library = "SL.glmnet")
summary(result1)

SuperLearner_override <- function(Y, X, newX = NULL, family = gaussian(), SL.library,
                                   method = "method.NNLS", id = NULL, verbose = FALSE, control = list(),
                                   cvControl = list(), obsWeights = NULL, env = parent.frame()) {
  stopifnot(identical(SL.library, "SL.glmnet"))

  res <- SL.glmnet(Y, X, newX, family, obsWeights, id)
  list(m=res, SL.predict = res$pred)
}

testthatsomemore::package_stub("SuperLearner", "SuperLearner", SuperLearner_override, {
  result2 <- ltmle(data, Anodes="A", Ynodes="Y", abar=1, estimate.time = F, SL.library = "SL.glmnet")
}
)
summary(result2)

testthatsomemore::package_stub("SuperLearner", "SuperLearner", SuperLearner_override, {
  result3 <- ltmle(data, Anodes="A", Ynodes="Y", abar=list(1,0), estimate.time = F, SL.library = "SL.glmnet")
}
)
summary(result3)

testthatsomemore::package_stub("SuperLearner", "SuperLearner", SuperLearner_override, {
  result4 <- ltmle(data, Anodes="A", Ynodes="Y", abar=list(1,0), estimate.time = F, SL.library = "glm")
}
)
summary(result4)


SL.glmnet
SuperLearner::SuperLearner


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

PredictOnly <- function(newdata1){
  if (class(m)[1] == "no.Y.variation")
    return(rep(m$Y.value, nrow(newdata1)))
  if (use.glm) {

    #print(class(m))
    if(class(m)[1]=="glm"){
      print("glm pred:")
      SuppressGivenWarnings(pred <- predict(m, newdata1,
                                            type), "prediction from a rank-deficient fit may be misleading")
      print(pred[1:10])
    }else{
      print("glmnet pred:")
      print(summary(m))
      print(head(newdata1))
      # print( class(newdata1))
      # newdata1<-as.data.frame(newdata1)
      # newdata1$Intercept <- 1
      pred <- glmnetUtils:::predict.cv.glmnet.formula(m, newdata = newdata1, s="lambda.min")
      print(pred[1:10])
    }

  }
  else {
    newX.list <- GetNewX(newdata1)
    pred <- ProcessSLPrediction(predict(m, newX.list$newX,
                                        X.subset, Y.subset, onlySL = TRUE)$pred, newX.list$new.subs,
                                try.result = NULL)
  }
  return(pred)
}








# Not run:
## simulate data
set.seed(23432)
## training set
n <- 500
p <- 50
X <- matrix(rnorm(n*p), nrow = n, ncol = p)
colnames(X) <- paste("X", 1:p, sep="")
X <- data.frame(X)
Y <- X[, 1] + sqrt(abs(X[, 2] * X[, 3])) + X[, 2] - X[, 3] + rnorm(n)

## test set
m <- 1000
newX <- matrix(rnorm(m*p), nrow = m, ncol = p)
colnames(newX) <- paste("X", 1:p, sep="")
newX <- data.frame(newX)
newY <- newX[, 1] + sqrt(abs(newX[, 2] * newX[, 3])) + newX[, 2] -
  newX[, 3] + rnorm(m)

# generate Library and run Super Learner
SL.library <- c("SL.glm", "SL.mean")
test <- SuperLearner(Y = Y, X = X, newX = newX, SL.library = SL.library,
                     verbose = TRUE, method = "method.NNLS")
test

SL.pred <- predict(test, newX = newX, X, Y, onlySL = T)

#See if I can get predictions from glmnet of the same form
test.glmnet <- glmnet::cv.glmnet(y = Y, x = as.matrix(X))

# predict.SuperLearner
# function (object, newdata, X = NULL, Y = NULL, onlySL = FALSE,

glmnet.pred <- predict(test.glmnet, newx = as.matrix(newX), s="lambda.min")


# predict.glmnet
# function (object, newx, s = NULL, type = c("link", "response",
#                                            "coefficients", "nonzero", "class"), exact = FALSE,
#           newoffset, ...)

library(glmnet)
predict.glmnet


head(SL.pred$pred)
head(glmnet.pred)

class(SL.pred$pred)
class(glmnet.pred)
