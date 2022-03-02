

rm(list=ls())

# if (!require(testthatsomemore)) {
#   if (!require(devtools)) install.packages('devtools'); require(devtools)
#   install_github('robertzk/testthatsomemore')
# }

package_stub<-function (package_name, function_name, stubbed_value, expr){
  if (!is.element(package_name, utils::installed.packages()[,
                                                            1]) && !is.element(package_name, loadedNamespaces())) {
    stop(gettextf("Could not find package %s for stubbing %s",
                  sQuote(package_name), dQuote(function_name)))
  }
  stopifnot(is.character(function_name))
  if (!is.function(stubbed_value))
    warning(gettextf("Stubbing %s::%s with a %s instead of a function",
                     package_name, function_name, sQuote(class(stubbed_value)[1])))
  namespaces <- list(as.environment(paste0("package:",
                                           package_name)), getNamespace(package_name))
  if (!exists(function_name, envir = namespaces[[1]], inherits = FALSE))
    namespaces <- namespaces[-1]
  if (!exists(function_name, envir = utils::tail(namespaces,
                                                 1)[[1]], inherits = FALSE))
    stop(gettextf("Cannot stub %s::%s because it must exist in the package",
                  package_name, function_name))
  lapply(namespaces, unlockBinding, sym = function_name)
  previous_object <- get(function_name, envir = utils::tail(namespaces,
                                                            1)[[1]])
  on.exit({
    lapply(namespaces, function(ns) {
      tryCatch(error = function(.) NULL, assign(function_name,
                                                previous_object, envir = ns))
      lockBinding(function_name, ns)
    })
  })
  lapply(namespaces, function(ns) assign(function_name, stubbed_value,
                                         envir = ns))
  eval.parent(substitute(expr))
}


# if (!require(testthatsomemore)) {
#   if (!require(devtools)) install.packages('devtools'); require(devtools)
#   install_github('robertzk/testthatsomemore')
# }

library(testthatsomemore)
library(devtools)

library(ltmle)
library(SuperLearner)

rexpit <- function(x) rbinom(n=length(x), size=1, prob=plogis(x))
n <- 1000
W1 <- rnorm(n)
W2 <- rnorm(n)
W3 <- rnorm(n)
W4 <- W3 + rnorm(n, sd=0.1)
A <- rexpit(-1 + 2 * W1 - 3*W2 + W3)
Y <- rexpit(W1 + W2 + W3 + A)
data <- data.frame(W1, W2, W3, W4, A, Y)

Lvars = c("W1", "W2", "W3", "W4")

result1 <- ltmle(data, Anodes="A", Lnodes=Lvars, Ynodes="Y", abar=list(1,0), estimate.time = F, SL.library = "SL.glmnet")
summary(result1)


SuperLearner_override <- function (Y, X, newX = NULL, family = gaussian(), SL.library,
                                   method = "method.NNLS", id = NULL, verbose = FALSE, control = list(),
                                   cvControl = list(), obsWeights = NULL, env = parent.frame()) {
  stopifnot(identical(SL.library, "SL.glmnet"))
  list(SL.predict = SL.glmnet(Y, X, newX, family, obsWeights, id)$pred)
}

package_stub("SuperLearner", "SuperLearner", SuperLearner_override, {
  result2 <- ltmle(data, Anodes="A", Lnodes=Lvars, Ynodes="Y", abar=list(1,0), estimate.time = F, SL.library = "SL.glmnet")
}
)
summary(result2)



SuperLearner_override <- function (Y, X, newX = NULL, family = "binomial", SL.library,
                                   method = "method.NNLS", id = NULL, verbose = FALSE, control = list(),
                                   cvControl = list(), obsWeights = NULL, env = parent.frame()) {
  stopifnot(identical(SL.library, "SL.glmnet"))
  list(SL.predict = SL.glmnet(Y, X, newX, family, obsWeights, id)$pred)
}


#NOTE! THIS REPRODUCES THE ERROR
package_stub("SuperLearner", "SuperLearner", SuperLearner_override, {
  result_temp <- ltmle(data, Anodes="A", Lnodes=Lvars, Ynodes="Y", abar=list(1,0), estimate.time = F, SL.library = "SL.glmnet")
}
)
summary(result_temp)



package_stub("SuperLearner", "SuperLearner", SuperLearner_override, {
  result3 <- ltmle(data, Anodes="A", Lnodes=Lvars, Ynodes="Y", abar=list(1,0), estimate.time = F, SL.library = "glm", family="binomial")
}
)
summary(result3)

result4 <- ltmle(data, Anodes="A", Lnodes=Lvars, Ynodes="Y", abar=list(1,0), estimate.time = F, SL.library = "glm", family="binomial")
summary(result4)


result5 <- ltmle(data, Anodes="A", Lnodes=Lvars, Ynodes="Y", abar=list(1,0), estimate.time = F, SL.library = c("SL.glm","SL.glmnet"), family="binomial")
summary(result5)


# result_glmnet <- cv.glmnet(x=as.matrix(data[,-6]), y=data$Y)
# result5 <- predict(result_glmnet, newx = as.matrix(data[,-6]), type = "response",  "lambda.min")
# summary(result5)


summary(result1)$effect.measures$RR$estimate
summary(result2)$effect.measures$RR$estimate
summary(result3)$effect.measures$RR$estimate
summary(result4)$effect.measures$RR$estimate
summary(result5)$effect.measures$RR$estimate
temp<- summary(result5)
temp$effect.measures$RR$estimate








#Debugging notes:
#Error occurs when contrasting Alevels versus trying to get a mean Y1
#The function just replaces the SuperLearner call in LTMLE with a glmnet call. Make sure the output matches SuperLearner
