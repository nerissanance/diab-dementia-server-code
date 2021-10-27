
#load all LTMLE functions then edit as needed
library(ltmle)
attach(loadNamespace("ltmle"), name="ltmle_all")
#attach(loadNamespace("glmnetUtils"), name="glmnetUtils_all")

#packages LTMLE relies on
#packrat:::recursivePackageDependencies("ltmle", lib.loc = .libPaths())
library(Matrix)
library(matrixStats)
library(speedglm)


is.glm <- function (SL.library){
  is.equal(SL.library, "glm", check.attributes = FALSE)
}

# data <- data.frame(Y=c(0,1,0,1), X1=c(1243,125,2135,3214), X2=c(0,0,1,1))
# formula <- as.formula("Y~X1+X2")
# weights <- c(1,1,1,1)

ltmle.glm <- function (formula, family, data, weights){

  # #need to update to convert formula
  # #formula = as.character(formula)
  # Yvar = as.character(as.formula(formula))[2]
  # Xvars = as.character(as.formula(formula))[3]
  # Xvars = as.vector(str_split(Xvars, "\\+", simplify = T))
  # Xvars = as.vector(gsub(" ","",  Xvars))
  # #Need to get matrices of x and y form for glmnet
  #   m <- glmnet::glmnet(x = as.matrix(data %>% subset(., select = c(Xvars))), y = as.matrix(data[[Yvar]]),
  #                       family = "binomial", weights =weights, data = data)
  cat("\nltmle.glm:\n")
  # print(class(data))
  #  print(head(data))
  #  try(print(table(data$S1)))
  # formula <- update.formula(formula, ~ .  + dummy)
  # data$dummy <- 1
  #  print(formula)
  # print(weights)
  # df <- stats::model.frame(formula, data)
  # head(df)

  m <- glmnetUtils:::cv.glmnet.formula(formula = formula, family = "binomial", weights =weights, data = data.frame(data,weights))
  class(m) <- "glmnet.formula"
  print("\nSuccess!\n")
  #print(m)

  return(m)
}



#ltmle:::ltmle.glm.fit
ltmle.glm.fit <- function (
  #x, y, 
  formula, data,
  weights, family, offset, intercept){
  
  cat("\nltmle.glm.fit:\n")
  # print(class(data))
  # print(head(data))
  # print(formula)
  
  m <- glmnetUtils:::cv.glmnet.formula(formula = formula, family = "binomial", 
                                    weights =weights, data = data,
                                    offset=offset, intercept=intercept
                                    )
  
  #m <- glmnet(x = x, y = y, family = "binomial", weights =weights, offset=offset, intercept=intercept)
  #class(m) <- c("glm", "lm")
  #cat("\nYAY!\n")
  #print(m)
  #class(m) <- "glmnet.formula"
  
  return(m)
}




# #NOTES: Debug from here on stepthrough
Qstar.kplus1 = NULL
family = quasibinomial()
type = "response"
called.from.estimate.g = TRUE
calc.meanL = inputs$variance.method != "ic"
regimes.with.positive.weight = 1:num.regimes


Estimate<-function(inputs, form, subs, family, type, nodes, Qstar.kplus1, 
                   cur.node, calc.meanL, called.from.estimate.g, regimes.meanL, 
                   regimes.with.positive.weight){
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

        
        #Make formula for lmtle.glmnet call
        X.subset<-as.data.frame(X.subset) %>% dplyr::select(!contains("Intercept"))
        #colnames(X.subset)[1] <- "Intercept"
        #colnames(X.subset)[1] <- "(Intercept)"
        #colnames(X.subset) <- gsub("X.Intercept.","(Intercept)", colnames(X.subset))
        #X.subset<-X.subset[,-1]
        glmnet_form <- as.formula(paste0("Y ~",paste(colnames(X.subset), collapse="+")))
        glmnet_form <- formulaic::create.formula(outcome.name ="Y",
                                                 input.names = colnames(X.subset))$formula
        
        
        glmnet_data <- data.frame(Y=Y.subset, X.subset)  %>% dplyr::select(!contains("Intercept"))
        # glmnet_data <- X.subset
        # glmnet_data$Y= Y.subset
        # 
        #colnames(glmnet_data) <- gsub("X.Intercept.","(Intercept)", colnames(glmnet_data))
        
        #colnames(glmnet_data)[2] <- "Intercept"
         # print(glmnet_form)
         # print(head(glmnet_data))
        # print(head(X.subset))
        # print(glmnet_form)
    
        #print(table(glmnet_data$`(Intercept)`))
        print(dim(glmnet_data))
        SuppressGivenWarnings({
          m <- ltmle.glm.fit(formula = glmnet_form, family = family, 
                             weights = observation.weights.subset, data=glmnet_data, 
                                             offset = offst, intercept = intercept
                             )
          #colnames(glmnet_data)[2] <- "(Intercept)"
          
           # m$terms <- tf
          # print(tf)
          cat("Hi\n")
          #print(class(m))
          # print(glmnet_form)
          #  newdata$Intercept<-1       
          # print(head(newdata))
          print(dim(newdata))
          
          predicted.values <- glmnetUtils:::predict.cv.glmnet.formula(m, newdata = newdata, s="lambda.min")
          #print(predicted.values[1:10])
          cat("bye\n")
          cat("Glmnet run, first 10 preds: ",predicted.values[1:10],"\n")
          
        # colnames(glmnet_data)[2] <- "(Intercept)"
        # colnames(newdata)[1] <- "(Intercept)"
          
          
        }, GetWarningsToSuppress())
      }
      else {
        newX.list <- GetNewX(newdata)
        SetSeedIfRegressionTesting()
        try.result <- try({
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
    #print("Heyyy")
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
  stopifnot(type %in% c("link", "response"))
  num.regimes <- dim(inputs$regimes)[3]
  if (form == "IDENTITY") {
    stopifnot(is.vector(Qstar.kplus1) == 1)
    predicted.values <- ValuesByType(matrix(Qstar.kplus1, 
                                            nrow = nrow(inputs$data), ncol = num.regimes))
    fit <- as.list(rep("no fit because form == IDENTITY", 
                       num.regimes))
    deterministic.list.olddata <- IsDeterministic(inputs$data, 
                                                  cur.node, inputs$deterministic.Q.function, nodes, 
                                                  called.from.estimate.g, inputs$survivalOutcome)
    is.deterministic <- matrix(deterministic.list.olddata$is.deterministic, 
                               nrow = nrow(inputs$data), ncol = num.regimes)
    deterministic.Q <- matrix(NA, nrow(inputs$data), num.regimes)
    deterministic.Q[is.deterministic, ] <- deterministic.list.olddata$Q
    return(list(predicted.values = predicted.values, fit = fit, 
                is.deterministic = is.deterministic, deterministic.Q = deterministic.Q, 
                prob.A.is.1.meanL = NULL))
  }
  data <- inputs$data
  if (cur.node %in% nodes$C) {
    data[, cur.node] <- ConvertCensoringNodeToBinary(data[, 
                                                          cur.node])
  }
  f <- as.formula(form)
  SL.library <- if (called.from.estimate.g) 
    inputs$SL.library.g
  else inputs$SL.library.Q
  use.glm <- (is.glm(SL.library) || length(RhsVars(f)) == 0)
  first.regime <- min(regimes.with.positive.weight)
  if (is.null(Qstar.kplus1)) {
    data.with.Qstar <- data
  }
  else {
    if (is.matrix(Qstar.kplus1)) {
      data.with.Qstar <- cbind(data, Q.kplus1 = Qstar.kplus1[, 
                                                             first.regime])
    }
    else {
      data.with.Qstar <- cbind(data, Q.kplus1 = Qstar.kplus1)
    }
  }
  mod.frame <- model.frame(f, data = data.with.Qstar, drop.unused.levels = TRUE, 
                           na.action = na.pass)
  Y <- mod.frame[[1]]
  tf <- terms(f)
  X <- model.matrix(tf, mod.frame)
  offst <- model.offset(mod.frame)
  intercept <- attributes(tf)$intercept
  if (!use.glm) {
    if (is.equal(family, quasibinomial())) 
      family <- binomial()
    if (!is.null(offst)) 
      stop("offset in formula not supported with SuperLearner")
    colnames(X) <- paste0("Xx.", 1:ncol(X))
    X <- as.data.frame(X)
  }
  fit <- vector("list", num.regimes)
  predicted.values <- deterministic.Q <- matrix(NA, nrow(data), 
                                                num.regimes)
  is.deterministic <- matrix(FALSE, nrow(data), num.regimes)
  fit.and.predict <- NULL
  multiple.subs <- is.matrix(subs)
  multiple.Qstar <- is.matrix(Qstar.kplus1)
  if (calc.meanL) {
    prob.A.is.1.meanL <- array(NaN, dim = c(nrow(inputs$data), 
                                            num.regimes, length(nodes$LY) - 1))
    Anode.index <- which(nodes$A < cur.node)
  }
  else {
    prob.A.is.1.meanL <- NULL
  }
  
  for (regime.index in regimes.with.positive.weight) {
    cat("regime index:",regime.index,"\n")
    newdata <- SetA(data = data.with.Qstar, regimes = inputs$regimes, 
                    Anodes = nodes$A, regime.index = regime.index, cur.node = cur.node)
    cat("newdata size:\n")
    print(dim(newdata))
    if (calc.meanL) {
      if (!is.null(regimes.meanL)) {
        newdata.meanL <- SetA(data = data.with.Qstar, 
                              regimes = regimes.meanL, Anodes = nodes$A, 
                              regime.index = regime.index, cur.node = cur.node)
      }
      else {
        newdata.meanL <- newdata
      }
    }
    deterministic.list.newdata <- IsDeterministic(newdata, 
                                                  cur.node, inputs$deterministic.Q.function, nodes, 
                                                  called.from.estimate.g, inputs$survivalOutcome)
    if (called.from.estimate.g && !is.null(inputs$deterministic.g.function)) {
      newdata.with.current <- newdata
      stopifnot(cur.node %in% nodes$AC)
      if (cur.node %in% nodes$A) {
        newdata.with.current[, cur.node] <- inputs$regimes[, 
                                                           which(nodes$A == cur.node), regime.index]
      }
      else {
        newdata.with.current <- newdata
      }
      deterministic.g.list.newdata <- IsDeterministicG(newdata.with.current, 
                                                       cur.node, inputs$deterministic.g.function, nodes, 
                                                       using.newdata = T)
    }
    else {
      deterministic.g.list.newdata <- list(is.deterministic = rep(FALSE, 
                                                                  nrow(data)), prob1 = NULL)
    }
    if (regime.index > first.regime && multiple.Qstar) {
      Y <- Qstar.kplus1[, regime.index]
    }
    if (regime.index == first.regime || multiple.subs) {
      single.subs <- if (multiple.subs) 
        subs[, regime.index]
      else subs
      X.subset <- X[single.subs, , drop = FALSE]
      id.subset <- inputs$id[single.subs]
      if (any(single.subs)) 
        X.subset[, colAlls(X.subset == 0)] <- 1
      observation.weights.subset <- inputs$observation.weights[single.subs]
      offst.subset <- offst[single.subs]
    }
    if (regime.index == first.regime || multiple.subs || 
        multiple.Qstar) {
      Y.subset <- Y[single.subs]
      if (anyNA(Y.subset)) 
        stop("NA in Estimate")
    }
    if (!all(deterministic.list.newdata$is.deterministic | 
             deterministic.g.list.newdata$is.deterministic)) {
      if (is.null(fit.and.predict) || multiple.Qstar || 
          multiple.subs) {
        fit.and.predict <- FitAndPredict()
        m <- fit.and.predict$m
        
        # print(regime.index)
        cat("regim:\n")
        print(regime.index)
        print(dim(predicted.values[, regime.index]))
        print(length(predicted.values[, regime.index]))
        print(head(predicted.values[, regime.index]))
        cat("Predictions:\n")

        print(dim(fit.and.predict$predicted.values))
        print(length(fit.and.predict$predicted.values))
         print(head(fit.and.predict$predicted.values))
        
         #Error here
        predicted.values[, regime.index] <- fit.and.predict$predicted.values
      }
      else {
        #Maybe here:
        predicted.values[, regime.index] <- PredictOnly(newdata)
      }
      if (calc.meanL) 
        prob.A.is.1.meanL[, regime.index, ] <- PredictProbAMeanL()
    }
    else {
      m <- "all rows are deterministic, no estimation took place"
    }
    predicted.values[deterministic.g.list.newdata$is.deterministic, 
                     regime.index] <- deterministic.g.list.newdata$prob1
    if (calc.meanL) 
      prob.A.is.1.meanL[deterministic.g.list.newdata$is.deterministic, 
                        regime.index, ] <- deterministic.g.list.newdata$prob1
    is.deterministic[, regime.index] <- deterministic.list.newdata$is.deterministic
    if (!called.from.estimate.g) 
      deterministic.Q[deterministic.list.newdata$is.deterministic, 
                      regime.index] <- deterministic.list.newdata$Q
    if (isTRUE(attr(SL.library, "return.fit", exact = TRUE))) {
      fit[[regime.index]] <- m
    }
    else {
      if (use.glm) {
        if (class(m)[1] %in% c("speedglm", "glm","cv.glmnet.formula","cv.glmnet")) {
          #fit[[regime.index]] <- summary(m)$coefficients
          glmnet.sel = data.frame(coef.name = dimnames(coef(m, s="lambda.min"))[[1]], val = matrix(coef(m, s="lambda.min")))
          fit[[regime.index]] <- glmnet.sel
        }
        else {
          stopifnot(class(m)[1] %in% c("no.Y.variation", 
                                       "character"))
          fit[[regime.index]] <- m
        }
      }
      else {
        capture.output(print.m <- print(m))
        fit[[regime.index]] <- print.m
      }
    }
  }
  return(list(predicted.values = predicted.values, fit = fit, 
              is.deterministic = is.deterministic, deterministic.Q = deterministic.Q, 
              prob.A.is.1.meanL = prob.A.is.1.meanL))
}

EstimateVariance <- function(inputs, nodes, combined.summary.measures, regimes.with.positive.weight, 
                             uncensored, alive, Qstar, Qstar.kplus1, cur.node, msm.weights, 
                             LYnode.index, ACnode.index, cum.g, prob.A.is.1, cum.g.meanL, 
                             cum.g.unbounded, cum.g.meanL.unbounded, observation.weights, 
                             is.last.LYnode, intervention.match){
  
  if (inputs$variance.method == "ic") 
    return(NA)
  est.var.iptw <- inputs$variance.method == "iptw"
  TmleOfVariance <- function(Z, Z.meanL) {
    if (all(is.na(Z))) 
      stop("all Z are NA in EstimateVariance")
    if (length(Z.meanL) == 0 || all(Z == 0 | is.na(Z))) {
      Qstar <- Scale(Z, 0, 1)
      return(list(EZd1 = mean(Z, na.rm = T), Qstar = Qstar))
    }
    if (est.var.iptw) {
      index <- uncensored & intervention.match[, d1]
      g <- cum.g[index, ACnode.index, d1]
      Y <- Scale(Z, 0, 1)[index]
      iptw.estimate <- sum(Y/g)/sum(1/g)
      return(list(EZd1 = iptw.estimate * diff(range(Z, 
                                                    na.rm = T)) + min(Z, na.rm = T), Qstar = rep(NA, 
                                                                                                 length(Z))))
    }
    sparsity.data <- inputs$data[, 1:cur.node]
    sparsity.data[, cur.node] <- Scale(Z, 0, 1)
    temp.nodes <- lapply(nodes, function(x) x[x <= cur.node])
    if (cur.node %in% temp.nodes$L) {
      temp.nodes$L <- setdiff(temp.nodes$L, cur.node)
      temp.nodes$Y <- c(temp.nodes$Y, cur.node)
    }
    stratify <- FALSE
    Qform <- paste(GetDefaultForm(sparsity.data[, 1:cur.node], 
                                  nodes = temp.nodes, is.Qform = TRUE, stratify = stratify, 
                                  survivalOutcome = FALSE, showMessage = FALSE), paste0("+ sparityAdj_Z.meanL_", 
                                                                                        1:length(temp.nodes$LY)))
    Qform[length(Qform)] <- "IDENTITY"
    Z.meanL <- apply(AsMatrix(Z.meanL), 2, LogitScale)
    sparsity.data <- cbind(Z.meanL, sparsity.data)
    names(sparsity.data)[sseq(1, ncol(Z.meanL))] <- paste0("sparityAdj_Z.meanL_", 
                                                           sseq(1, ncol(Z.meanL)))
    temp.nodes <- lapply(temp.nodes, function(x) x + ncol(Z.meanL))
    names(Qform) <- names(sparsity.data)[temp.nodes$LY]
    attr(sparsity.data, "called.from.estimate.variance") <- TRUE
    var.tmle <- ltmle(sparsity.data, Anodes = temp.nodes$A, 
                      Cnodes = temp.nodes$C, Lnodes = temp.nodes$L, Ynodes = temp.nodes$Y, 
                      survivalOutcome = FALSE, Qform = Qform, gform = drop3(prob.A.is.1[, 
                                                                                        1:ACnode.index, d1, drop = FALSE]), abar = GetABar(inputs$regimes, 
                                                                                                                                           d1, temp.nodes$A), gbounds = inputs$gbounds, 
                      stratify = stratify, estimate.time = FALSE, deterministic.Q.function = det.q.function, 
                      variance.method = "ic", observation.weights = observation.weights)
    EZd1 <- var.tmle$estimates["tmle"] * diff(range(Z, 
                                                    na.rm = T)) + min(Z, na.rm = T)
    return(list(EZd1 = EZd1, Qstar = var.tmle$Qstar))
  }
  EqualRegimesIndex <- function(dd1, dd2) {
    if (!any(nodes$A <= cur.node)) 
      return(rep(TRUE, n))
    eq <- rowAlls(AsMatrix(inputs$regimes[, which(nodes$A <= 
                                                    cur.node), dd1]) == AsMatrix(inputs$regimes[, which(nodes$A <= 
                                                                                                          cur.node), dd2]))
    eq[is.na(eq)] <- FALSE
    return(eq)
  }
  IsStaticTreatment <- function() {
    for (dd1 in regimes.with.positive.weight) {
      for (dd2 in regimes.with.positive.weight[regimes.with.positive.weight > 
                                               dd1]) {
        if (any(EqualRegimesIndex(dd1, dd2))) 
          return(FALSE)
      }
    }
    return(TRUE)
  }
  num.regimes <- dim(inputs$regimes)[3]
  num.betas <- ncol(combined.summary.measures)
  n <- nrow(inputs$data)
  if (inputs$survivalOutcome) {
    det.q.function <- function(data, current.node, nodes, 
                               called.from.estimate.g) {
      if (!any(nodes$Y < current.node)) 
        return(NULL)
      prev.Y <- data[, nodes$Y[nodes$Y < current.node], 
                     drop = F]
      prev.Y[is.na(prev.Y)] <- 0
      is.deterministic <- rowAnys(prev.Y == 1)
      Q.value <- data[is.deterministic, max(nodes$Y)]
      return(list(is.deterministic = is.deterministic, 
                  Q.value = Q.value))
    }
  }
  else {
    det.q.function <- NULL
  }
  static.treatment <- IsStaticTreatment()
  variance.estimate <- matrix(0, num.betas, num.betas)
  Sigma <- array(dim = c(n, num.regimes, num.regimes))
  if (!is.last.LYnode) 
    Q.data <- inputs$data[alive, 1:cur.node, drop = F]
  for (d1 in regimes.with.positive.weight) {
    if (static.treatment) {
      d2.regimes <- d1
    }
    else {
      d2.regimes <- regimes.with.positive.weight
    }
    for (d2 in d2.regimes) {
      if (is.last.LYnode) {
        Sigma[, d1, d2] <- Qstar[, d1] * (1 - Qstar[, 
                                                    d1])
      }
      else {
        if (any(alive)) {
          resid.sq <- (Qstar.kplus1[alive, d1] - Qstar[alive, 
                                                       d1]) * (Qstar.kplus1[alive, d2] - Qstar[alive, 
                                                                                               d2])
          resid.sq.range <- range(resid.sq, na.rm = T)
          if (diff(resid.sq.range) > 1e-04) {
            Q.data[, cur.node] <- (resid.sq - resid.sq.range[1])/diff(resid.sq.range)
            names(Q.data)[cur.node] <- "Q.kplus1"
            cat("A\n")
            m <- ltmle.glm(formula = formula(inputs$Qform[LYnode.index]), 
                           family = quasibinomial(), data = Q.data, 
                           weights = NULL)
            Q.newdata <- SetA(data = Q.data, regimes = inputs$regimes[alive, 
                                                                      , d1, drop = F], Anodes = nodes$A, cur.node = cur.node)
            SuppressGivenWarnings(Q.resid.sq.pred <- glmnetUtils:::predict.cv.glmnet.formula(m, newdata = Q.newdata, #type = "response", 
                                                                                             s="lambda.min"),
                                  "prediction from a rank-deficient fit may be misleading")
            Sigma[alive, d1, d2] <- Q.resid.sq.pred * 
              diff(resid.sq.range) + resid.sq.range[1]
          }
          else {
            resid.sq.value <- min(resid.sq, na.rm = T)
            Sigma[alive, d1, d2] <- resid.sq.value
          }
        }
        Sigma[!alive, d1, d2] <- 0
      }
    }
  }
  if (est.var.iptw) 
    Z.without.sum.meas.meanL <- Z.meanL <- NA
  no.V <- length(inputs$baseline.column.names) == 0
  if ((!est.var.iptw && static.treatment) || (est.var.iptw && 
                                              static.treatment && no.V)) {
    for (d1 in regimes.with.positive.weight) {
      Z.without.sum.meas <- Sigma[, d1, d1]/cum.g[, ACnode.index, 
                                                  d1] * cum.g.unbounded[, ACnode.index, d1]/cum.g[, 
                                                                                                  ACnode.index, d1] * msm.weights[, d1]^2 * observation.weights^2
      if (!est.var.iptw) 
        Z.without.sum.meas.meanL <- 1/cum.g.meanL[, ACnode.index, 
                                                  d1, ] * cum.g.meanL.unbounded[, ACnode.index, 
                                                                                d1, ]/cum.g.meanL[, ACnode.index, d1, ] * msm.weights[, 
                                                                                                                                      d1]^2 * observation.weights^2
      var.tmle <- TmleOfVariance(Z.without.sum.meas, Z.without.sum.meas.meanL)
      if (no.V) {
        variance.estimate <- variance.estimate + (combined.summary.measures[1, 
                                                                            , d1] %*% t(combined.summary.measures[1, , 
                                                                                                                  d1])) * var.tmle$EZd1
      }
      else {
        baseline.msm <- paste("Qstar ~", paste(inputs$baseline.column.names, 
                                               collapse = " + "), "+", paste0("I(", 
                                                                              inputs$baseline.column.names, "^2)", 
                                                                              collapse = " + "))
        V.data <- data.frame(Qstar = var.tmle$Qstar, 
                             inputs$data[, inputs$baseline.column.names, 
                                         drop = FALSE])
        cat("B\n")
        m <- ltmle.glm(formula(baseline.msm), family = quasibinomial(), 
                       data = V.data, weights = NULL)
        SuppressGivenWarnings(pred.Qstar <- glmnetUtils:::predict.cv.glmnet.formula(m, 
                                                                                 #type = "response", 
                                                                                 newdata = V.data, s="lambda.min") * 
                                diff(range(Z.without.sum.meas, na.rm = T)) + 
                                min(Z.without.sum.meas, na.rm = T), "prediction from a rank-deficient fit may be misleading")
        variance.estimate.sum <- crossprod(combined.summary.measures[, 
                                                                     , d1], combined.summary.measures[, , d1] * 
                                             pred.Qstar)
        variance.estimate <- variance.estimate + variance.estimate.sum/n
      }
    }
  }
  else {
    for (beta.index2 in 1:num.betas) {
      for (d1 in regimes.with.positive.weight) {
        Z.base <- rep(0, n)
        if (!est.var.iptw) 
          Z.base.meanL <- matrix(0, n, dim(cum.g.meanL)[4])
        for (d2 in regimes.with.positive.weight) {
          equal.regimes.index <- EqualRegimesIndex(d1, 
                                                   d2)
          h1 <- combined.summary.measures[, beta.index2, 
                                          d2] * msm.weights[, d2]
          Z.base[equal.regimes.index] <- Z.base[equal.regimes.index] + 
            h1[equal.regimes.index] * Sigma[equal.regimes.index, 
                                            d1, d2]/cum.g[equal.regimes.index, ACnode.index, 
                                                          d1] * observation.weights[equal.regimes.index]
          if (!est.var.iptw) 
            Z.base.meanL[equal.regimes.index, ] <- Z.base.meanL[equal.regimes.index, 
            ] + h1[equal.regimes.index] * 1/cum.g.meanL[equal.regimes.index, 
                                                        ACnode.index, d1, ] * observation.weights[equal.regimes.index]
        }
        for (beta.index1 in 1:num.betas) {
          if (beta.index1 >= beta.index2) {
            Z <- combined.summary.measures[, beta.index1, 
                                           d1] * msm.weights[, d1] * cum.g.unbounded[, 
                                                                                     ACnode.index, d1]/cum.g[, ACnode.index, 
                                                                                                             d1] * observation.weights * Z.base
            if (!est.var.iptw) 
              Z.meanL <- combined.summary.measures[, 
                                                   beta.index1, d1] * msm.weights[, d1] * 
                cum.g.meanL.unbounded[, ACnode.index, 
                                      d1, ]/cum.g.meanL[, ACnode.index, d1, 
                                      ] * observation.weights * Z.base.meanL
            var.tmle <- TmleOfVariance(Z, Z.meanL)
            variance.estimate[beta.index1, beta.index2] <- variance.estimate[beta.index1, 
                                                                             beta.index2] + var.tmle$EZd1
          }
          else {
            variance.estimate[beta.index1, beta.index2] <- variance.estimate[beta.index2, 
                                                                             beta.index1]
          }
        }
      }
    }
  }
  if (max(abs(variance.estimate - t(variance.estimate))) > 
      1e-05) 
    stop("not symmetric")
  if (any(eigen(variance.estimate, only.values = TRUE)$values < 
          -1e-08)) {
    variance.estimate <- MakePosDef(variance.estimate)
  }
  return(variance.estimate)
}




UpdateQ<-function (Qstar.kplus1, logitQ, combined.summary.measures, cum.g, 
                   working.msm, uncensored, intervention.match, is.deterministic, 
                   msm.weights, gcomp, observation.weights) 
{
  n <- nrow(logitQ)
  num.regimes <- ncol(logitQ)
  off <- as.vector(logitQ)
  Y <- as.vector(Qstar.kplus1)
  if (length(cum.g) == 0) 
    cum.g <- 1
  stacked.summary.measures <- apply(combined.summary.measures, 
                                    2, rbind)
  subs.vec <- uncensored & !is.deterministic & as.vector(intervention.match)
  weight.vec <- numeric(n * num.regimes)
  weight.vec[subs.vec] <- (observation.weights * as.vector(msm.weights)/as.vector(cum.g))[subs.vec]
  if (anyNA(weight.vec)) 
    stop("NA in weight.vec")
  f <- as.formula(paste(working.msm, "+ offset(off)"))
  data.temp <- data.frame(Y, stacked.summary.measures, off)
  if (gcomp) {
    Qstar <- plogis(logitQ)
    m <- "no Qstar fit because gcomp=TRUE (so no updating step)"
  }
  else {
    if (any(weight.vec > 0)) {
      #cat("C\n")
      # m <- ltmle.glm(f, data = data.temp[weight.vec > 0, 
      # ], family = quasibinomial(), weights = as.vector(scale(weight.vec[weight.vec > 
      #                                                                     0], center = FALSE)))
      # 
      m <- glm(formula=formula(f),  family = quasibinomial(), 
               data = data.temp[weight.vec > 0,], 
               weights = as.vector(scale(weight.vec[weight.vec >0], center = FALSE)))

      # Qstar <- matrix(glmnetUtils:::predict.cv.glmnet.formula(m, newdata = data.temp, type = "response", s="lambda.min"), 
      #                 nrow = nrow(logitQ))
      Qstar <- matrix(predict(m, newdata = data.temp, type = "response"), 
                      nrow = nrow(logitQ))
    }
    else {
      Qstar <- plogis(logitQ)
      m <- "no Qstar fit because no subjects alive, uncensored, following intervention"
    }
  }
  indicator <- matrix(uncensored * observation.weights, nrow = nrow(stacked.summary.measures), 
                      ncol = ncol(stacked.summary.measures)) * matrix(intervention.match, 
                                                                      nrow = nrow(stacked.summary.measures), ncol = ncol(stacked.summary.measures))
  h.g.ratio <- stacked.summary.measures/matrix(cum.g, nrow = nrow(stacked.summary.measures), 
                                               ncol = ncol(stacked.summary.measures)) * indicator
  dim(h.g.ratio) <- c(n, num.regimes, ncol(h.g.ratio))
  for (i in 1:num.regimes) {
    h.g.ratio[, i, ] <- h.g.ratio[, i, ] * msm.weights[, 
                                                       i]
    weight.zero.index <- msm.weights[, i] == 0
    h.g.ratio[weight.zero.index, i, ] <- 0
  }
  cum.g.used <- weight.vec > 0 & msm.weights > 0
  dim(cum.g.used) <- c(n, num.regimes)
  return(list(Qstar = Qstar, h.g.ratio = h.g.ratio, X = stacked.summary.measures, 
              off = off, fit = m, cum.g.used = cum.g.used))
}

FitPooledMSM <- function (working.msm, Qstar, combined.summary.measures, msm.weights){
  n <- dim(Qstar)[1]
  num.regimes <- dim(Qstar)[2]
  num.final.Ynodes <- dim(Qstar)[3]
  num.summary.measures <- dim(combined.summary.measures)[2]
  X <- apply(combined.summary.measures, 2, rbind)
  Y <- as.vector(Qstar)
  weight.vec <- as.vector(msm.weights)
  data.pooled <- data.frame(Y, X)
  positive.weight <- weight.vec > 0
  #m <- ltmle.glm(formula(working.msm), data = data.pooled[positive.weight, 
   cat("D\n")
  # print(formula(working.msm))
  m <- glm(formula=formula(working.msm),  family = quasibinomial(), 
           data = data.pooled[positive.weight,], 
           weights = weight.vec[positive.weight])
  SuppressGivenWarnings(m.beta <- predict(m, newdata = data.pooled, type = "response"), "prediction from a rank-deficient fit may be misleading")
  
  # m <- ltmle.glm(formula(working.msm), data = data.pooled[positive.weight, 
  # ], family = quasibinomial(), weights = weight.vec[positive.weight])
  # SuppressGivenWarnings(m.beta <- glmnetUtils:::predict.cv.glmnet.formula(m, newdata = data.pooled,
  #                                                                      type = "response", s="lambda.min"), "prediction from a rank-deficient fit may be misleading")
   dim(m.beta) <- dim(Qstar)
  return(list(m = m, m.beta = m.beta))
}



CalcIPTW <- function (inputs, cum.g, msm.weights){
  if (isTRUE(attr(inputs$data, "called.from.estimate.variance", 
                  exact = TRUE))) {
    return(list(beta = NA, IC = matrix(NA, 1, 1)))
  }
  nodes <- inputs$all.nodes
  n <- nrow(inputs$data)
  num.regimes <- dim(inputs$regimes)[3]
  num.final.Ynodes <- length(inputs$final.Ynodes)
  Y.vec <- X.mat <- weight.vec <- NULL
  save.xy <- list()
  for (j in 1:num.final.Ynodes) {
    final.Ynode <- inputs$final.Ynodes[j]
    intervention.match <- InterventionMatch(inputs$intervention.match, 
                                            nodes$A, cur.node = final.Ynode)
    uncensored <- IsUncensored(inputs$uncensored, nodes$C, 
                               final.Ynode)
    for (i in 1:num.regimes) {
      cat("num.regime: ",i,"\n")
      index <- uncensored & intervention.match[, i]
      col.index <- which.max(nodes$AC[nodes$AC < final.Ynode])
      Y <- inputs$data[index, final.Ynode]
      if (length(col.index > 0)) {
        g <- cum.g[index, col.index, i]
      }
      else {
        g <- 1
      }
      X <- inputs$combined.summary.measures[index, , i, 
                                            j]
      if (is.vector(X)) {
        dim(X) <- c(sum(index), ncol(inputs$combined.summary.measures))
      }
      weight <- msm.weights[index, i, j] * inputs$observation.weights[index]/g
      weight[msm.weights[index, i, j] == 0 | inputs$observation.weights[index] == 
               0] <- 0
      save.xy[[length(save.xy) + 1]] <- list(X = X, Y = Y, 
                                             weight = weight, index = index)
      Y.vec <- c(Y.vec, Y)
      X.mat <- rbind(X.mat, X)
      weight.vec <- c(weight.vec, weight)
    }
  }
  colnames(X.mat) <- colnames(inputs$combined.summary.measures)
  if (nrow(X.mat) == 0) {
    warning("no rows uncensored and matching regimes/abar - IPTW returns NA")
    num.beta <- ncol(inputs$combined.summary.measures)
    return(list(beta = rep(NA, num.beta), IC = matrix(nrow = n, 
                                                      ncol = num.beta)))
  }
  # m.glm <- ltmle.glm(formula(inputs$working.msm), family = quasibinomial(), 
  #                    data = data.frame(Y = Y.vec, X.mat, weight.vec), weights = as.vector(scale(weight.vec, 
  #                                                                                               center = FALSE)))
  m.glm <- glm(formula=formula(inputs$working.msm),  family = quasibinomial(), 
               data = data.frame(Y = Y.vec, X.mat, weight.vec), 
               weights = as.vector(scale(weight.vec, center = FALSE)))

  beta <- coef(m.glm)
  IC <- matrix(0, nrow = n, ncol = length(beta))
  m.beta <- array(dim = c(n, num.regimes, num.final.Ynodes))
  cnt <- 0
  for (j in 1:num.final.Ynodes) {
    cat("num.final.Ynodes:",j,"\n")
    
    final.Ynode <- inputs$final.Ynodes[j]
    for (i in 1:num.regimes) {
      cat("regime index:",i,"\n")
      
      newdata <- data.frame(inputs$combined.summary.measures[, 
                                                             , i, j])
      colnames(newdata) <- colnames(inputs$combined.summary.measures)
      SuppressGivenWarnings(m.beta[, i, j] <- predict(m.glm, 
                                                      newdata = newdata, type = "response"), 
                            "prediction from a rank-deficient fit may be misleading")
      cnt <- cnt + 1
      XY.list <- save.xy[[cnt]]
      IC[XY.list$index, ] <- IC[XY.list$index, ] + XY.list$weight * 
        XY.list$X * (XY.list$Y - m.beta[XY.list$index, 
                                        i, j])
    }
  }
  C <- NormalizeIC(IC, inputs$combined.summary.measures, m.beta, 
                   msm.weights, observation.weights = inputs$observation.weights, 
                   g.ratio = NULL)
  normalized.IC <- t(safe.solve(C, t(IC)))
  household.IC <- HouseholdIC(normalized.IC, inputs$id)
  names(beta) <- inputs$beta.names
  return(list(beta = beta, IC = household.IC))
}



assignInNamespace("EstimateVariance",EstimateVariance, ns="ltmle")
assignInNamespace("Estimate",Estimate, ns="ltmle")
assignInNamespace("UpdateQ",UpdateQ, ns="ltmle")
assignInNamespace("FitPooledMSM",FitPooledMSM, ns="ltmle")
assignInNamespace("ltmle.glm",ltmle.glm, ns="ltmle")
assignInNamespace("ltmle.glm.fit",ltmle.glm.fit, ns="ltmle")
assignInNamespace("CalcIPTW",CalcIPTW, ns="ltmle")

# predict.cv.glmnet.formula <- function(object, newdata, na.action = na.pass, ...){
#   if (!inherits(object, "cv.glmnet.formula")) 
#     stop("invalid cv.glmnet.formula object")
#   cl <- match.call(expand.dots = FALSE)
# 
#   cl$formula <- delete.response(object$terms)
#   cat("in prediction function\n")
#   # cat(head(cl$newdata))
#   # cat(object$xlev)
#   # cat(cl$formula)
#   cl$data <- cl$newdata
#   cl$newdata <- NULL
#   cl$xlev <- object$xlev
#   cl[[1]] <- if (object$use.model.frame) 
#     makeModelComponentsMF
#   else makeModelComponents
#   xy <- eval.parent(cl)
#   
#   #print(head(xy))
#   
#   x <- xy$x
#   offset <- xy$offset
#   class(object) <- class(object)[-1]
#   predict(object, x, ...)
# }
# assignInNamespace("predict.cv.glmnet.formula",predict.cv.glmnet.formula, ns="glmnetUtils")
# 
# 
