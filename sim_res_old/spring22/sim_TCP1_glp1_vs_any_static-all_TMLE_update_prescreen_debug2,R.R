

rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))

load(here("data/debug_workspace.RData"))
#fit.and.predict <- FitAndPredict()

#function() {
  # if (length(Y.subset) < 2)
  #   stop("Estimation failed because there are fewer than 2 observations to fit")
  # Y.subset.range <- range(Y.subset)
  # if (anyNA(Y.subset.range))
  #   stop("Internal error - NA in Y during Estimate")
  # if (Y.subset.range[1] < -1e-04 || Y.subset.range[2] >
  #     1.0001)
  #   stop("Internal error - Y negative or greater than 1 in Estimate")
  # if (Y.subset.range[2] - Y.subset.range[1] < 1e-04) {
  #   Y.value <- Y.subset.range[2]
  #   m <- list("no estimation occured because all Y values are the same",
  #             Y.value = Y.value)
  #   predicted.values <- ValuesByType(rep(Y.value, nrow(newdata)))
  #   class(m) <- "no.Y.variation"
  # }
  # else{
    # if (use.glm) {
    #   SuppressGivenWarnings({
    #     m <- ltmle.glm.fit(y = Y.subset, x = X.subset,
    #                        family = family, weights = observation.weights.subset,
    #                        offset = offst, intercept = intercept)
    #     m$terms <- tf
    #     predicted.values <- predict(m, newdata = newdata,
    #                                 type = type)
    #   }, GetWarningsToSuppress())
    # }
    # else {
      newX.list <- GetNewX(newdata)
      SetSeedIfRegressionTesting()



      #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      #SuperLearner_override_lasso_prescreen

      Y = Y.subset
      X = X.subset
      SL.library = SL.library
      cvControl = inputs$SL.cvControl
      verbose = FALSE
      newX = newX.list$newX
      obsWeights = observation.weights.subset
      id = id.subset
newX = NULL
      method = "method.NNLS"
      verbose = FALSE
      control = list()
      obsWeights = NULL
      env = parent.frame()
      alpha=1
      loss  = "auc"

      SL.library="SL.glm"
      stopifnot(identical(SL.library, "SL.glm"))

      minscreen=2

      #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      #NEED TO REMOVE A vars FROM PRESCREENING - feed it in
      #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

      #XXXX tf object is terms
      tf


      #.SL.require("glmnet")
      X_Avar <- as.data.frame(X) %>% select(starts_with("glp1_"))
      X_Lvars <- as.data.frame(X) %>% select(!starts_with("glp1_"))
      if (!is.matrix(X_Lvars)) {
        X_Lvars <- model.matrix(~-1 + ., X_Lvars)
      }
      fitCV <- glmnet::cv.glmnet(x = X_Lvars, y = Y, lambda = NULL, type.measure = "deviance",
                                 nfolds = 5, family = family$family, alpha = alpha,
                                 nlambda = 100)
      whichVariable <- (as.numeric(coef(fitCV$glmnet.fit, s = fitCV$lambda.min))[-1] !=0)
      if (sum(whichVariable) < minscreen) {
        warning("fewer than minscreen variables passed the glmnet screen, increased lambda to allow minscreen variables")
        sumCoef <- apply(as.matrix(fitCV$glmnet.fit$beta), 2,
                         function(x) sum((x != 0)))
        newCut <- which.max(sumCoef >= minscreen)
        whichVariable <- (as.matrix(fitCV$glmnet.fit$beta)[,
                                                           newCut] != 0)
      }

      # cat(whichVariable)
      # X <- X %>% subset(., select==!!(whichVariable))
      X_Lvars<-as.data.frame(X_Lvars)
      X_Lvars<-X_Lvars[,whichVariable]
      #print(class(X))
      X <- cbind(X_Avar,X_Lvars)

      # res <- NULL
      # try(res <- SL.glm(Y, X, newX, family, obsWeights, id))
      fit.glm <- glm(Y ~ ., data = data.frame(Y,X), family = family, weights = obsWeights, model = TRUE)
      if (is.matrix(newX)) {
        newX = as.data.frame(newX)
      }
      pred <- predict(fit.glm, newdata = newX, type = "response")
      fit <- list(object = fit.glm)
      class(fit) <- "SL.glm"
      res <- list(pred = pred, fit = fit)

      if(is.null(res)){res <- SL.mean(Y, X, newX, family, obsWeights, id)}

      #print( res$fit)

      list(model=res$fit, SL.predict = res$pred)
