FitAndPredict <- function() {
  if (length(Y.subset) < 2)
    stop("Estimation failed because there are fewer than 2 observations to fit")
  Y.subset.range <- range(Y.subset)
  if (anyNA(Y.subset.range))
    stop("Internal error - NA in Y during Estimate")
  if (Y.subset.range[1] < -1e-04 || Y.subset.range[2] >
      1.0001)
    stop("Internal error - Y negative or greater than 1 in Estimate")
  if (Y.subset.range[2] - Y.subset.range[1] < 1e-04) {
    Y.value <- Y.subset.range[2]
    m <- list("no estimation occured because all Y values are the same",
              Y.value = Y.value)
    predicted.values <- ValuesByType(rep(Y.value, nrow(newdata)))
    class(m) <- "no.Y.variation"
  }
  else {
    if (use.glm) {
      SuppressGivenWarnings({
        m <- ltmle.glm.fit(y = Y.subset, x = X.subset,
                           family = family, weights = observation.weights.subset,
                           offset = offst, intercept = intercept)
        m$terms <- tf
        predicted.values <- predict(m, newdata = newdata,
                                    type = type)
      }, GetWarningsToSuppress())
    }
    else {
      newX.list <- GetNewX(newdata)
      SetSeedIfRegressionTesting()
      try.result <- try({

        #parallelized version:
        # SuppressGivenWarnings(m <- SuperLearner::mcSuperLearner(Y = Y.subset,
        #                                                       X = X.subset, SL.library = SL.library, cvControl = inputs$SL.cvControl,
        #                                                       verbose = FALSE, family = family, newX = newX.list$newX,
        #                                                       obsWeights = observation.weights.subset,
        #                                                       id = id.subset, env = environment(SuperLearner::mcSuperLearner)),
        #                       c("non-integer #successes in a binomial glm!",
        #                         "prediction from a rank-deficient fit may be misleading"))


        SuppressGivenWarnings(m <- SuperLearner::SuperLearner(Y = Y.subset,
                                                              X = X.subset, SL.library = SL.library, cvControl = inputs$SL.cvControl,
                                                              verbose = FALSE, family = family, newX = newX.list$newX,
                                                              obsWeights = observation.weights.subset,
                                                              id = id.subset, env = environment(SuperLearner::SuperLearner)),
                              c("non-integer #successes in a binomial glm!",
                                "prediction from a rank-deficient fit may be misleading"))
      })
      predicted.values <- ProcessSLPrediction(m$SL.predict,
                                              newX.list$new.subs, try.result)
    }
  }
  return(list(m = m, predicted.values = predicted.values))
}
GetSLStopMsg <- function(Y) {
  ifelse(all(Y %in% c(0, 1, NA)), "", "\n Note that some SuperLeaner libraries crash when called with continuous dependent variables, as in the case of initial Q regressions with continuous Y or subsequent Q regressions even if Y is binary.")
}
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
  return(ValuesByType(predicted.values))
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

    if(identical(SL.library, "SL.randomForest")){
      pred <- ProcessSLPrediction(predict.SL.randomForest(m$model, as.matrix(newX.list$newX), family=family),
                                  newX.list$new.subs,
                                  try.result = NULL)
    }else{
      if(identical(SL.library, "SL.glmnet")){
        pred <- ProcessSLPrediction(predict(m$model, as.matrix(newX.list$newX), s="lambda.min", type="response"),
                                    newX.list$new.subs,
                                    try.result = NULL)
      }else{
        pred <- ProcessSLPrediction(predict(m$model, as.matrix(newX.list$newX), type="response"),
                                    newX.list$new.subs,
                                    try.result = NULL)
      }
    }


  }
  return(pred)
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
PredictProbAMeanL <- function() {
  probAis1.meanL <- matrix(NaN, nrow(inputs$data), length(nodes$LY) -
                             1)
  if (ncol(probAis1.meanL) == 0)
    return(probAis1.meanL)
  all.LY.nodes <- sort(union(nodes$L, nodes$Y))
  LYindex <- length(nodes$LY)
  for (i in length(all.LY.nodes):1) {
    regression.node <- all.LY.nodes[i]
    L <- data[single.subs, regression.node]
    if (is.numeric(L) && !IsBinary(L)) {
      meanL <- mean(L, na.rm = TRUE)
    }
    else {
      meanL <- Mode(L, na.rm = TRUE)
    }
    newdata.meanL[, regression.node] <- meanL
    if (regression.node %in% nodes$LY[1:length(nodes$LY) -
                                      1]) {
      LYindex <- LYindex - 1
      probAis1.meanL[, LYindex] <- PredictOnly(newdata = newdata.meanL)
    }
  }
  if (anyNA(probAis1.meanL[, 1]))
    stop("NA in probAis1.meanL[, 1]")
  return(probAis1.meanL)
}
