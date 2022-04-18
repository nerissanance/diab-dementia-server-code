

data=spec_ltmle$data
Anodes = spec_ltmle$Anodes
Cnodes = spec_ltmle$Cnodes
Lnodes = spec_ltmle$Lnodes
Ynodes = spec_ltmle$Ynodes
survivalOutcome = T
abar = abar_spec
gcomp=gcomp
Qform = NULL
estimate.time=T
deterministic.Q.function = det.Q.function
SL.library = "SL.glmnet"
variance.method = varmethod



          rule = NULL
          gbounds = c(0.01, 1)
          Yrange = NULL
          deterministic.g.function = NULL
          stratify = FALSE
           SL.cvControl = list()
          estimate.time = TRUE
          iptw.only = FALSE
          deterministic.Q.function = NULL
          variance.method = "tmle"
          observation.weights = NULL
          id = NULL
          gform = NULL

  data <- CheckData(data)
  msm.inputs <- GetMSMInputsForLtmle(data, abar, rule, gform)
  inputs <- CreateInputs(data = data, Anodes = Anodes, Cnodes = Cnodes,
                         Lnodes = Lnodes, Ynodes = Ynodes, survivalOutcome = survivalOutcome,
                         Qform = Qform, gform = msm.inputs$gform, Yrange = Yrange,
                         gbounds = gbounds, deterministic.g.function = deterministic.g.function,
                         SL.library = SL.library, SL.cvControl = SL.cvControl,
                         regimes = msm.inputs$regimes, working.msm = msm.inputs$working.msm,
                         summary.measures = msm.inputs$summary.measures, final.Ynodes = msm.inputs$final.Ynodes,
                         stratify = stratify, msm.weights = msm.inputs$msm.weights,
                         estimate.time = estimate.time, gcomp = gcomp, iptw.only = iptw.only,
                         deterministic.Q.function = deterministic.Q.function,
                         variance.method = variance.method, observation.weights = observation.weights,
                         id = id)

  num.final.Ynodes <- length(inputs$final.Ynodes)
  num.betas <- dim(inputs$combined.summary.measures)[2]
  #combined.summary.measures: n x num.measures x num.regimes x num.final.Ynodes       note: num.measures is summary measures and baseline covariates, converted to main terms
  n <- nrow(inputs$data)
  num.regimes <- dim(inputs$regimes)[3]
  Qstar <- array(dim=c(n, num.regimes, num.final.Ynodes))
  all.msm.weights <- GetMsmWeights(inputs) #n x num.regimes x num.final.Ynodes
  new.var.y <- array(dim=c(num.betas, num.betas, num.final.Ynodes))
  IC <- matrix(0, n, num.betas)
  #store IC for each final Ynode, compare var(IC) to sum(var(IC.ynode))
  IC.y <- array(dim=c(n, num.betas, num.final.Ynodes))




  n <- nrow(inputs$data)
  num.regimes <- dim(inputs$regimes)[3]
  nodes <- inputs$all.nodes

  g <- cum.g <- cum.g.unbounded <- prob.A.is.1 <- array(NaN, dim=c(n, length(nodes$AC), num.regimes))
  if(inputs$variance.method == "ic"){
    cum.g.meanL <- cum.g.meanL.unbounded <- NULL
  }else{
    g.meanL <- cum.g.meanL <- cum.g.meanL.unbounded <- array(NaN, dim=c(n, length(nodes$AC), num.regimes, length(nodes$LY)-1))
  }
  fit <- vector("list", length(nodes$AC))
  names(fit) <- names(inputs$data)[nodes$AC]

  if (inputs$variance.method != "ic" && anyNA(inputs$regimes)){
    regimes.meanL <- inputs$regimes
    for (i in seq_along(nodes$A)) {
      for (regime.index in 1:num.regimes) {
        regimes.meanL[is.na(regimes.meanL[, i, regime.index]), i, regime.index] <- Mode(inputs$regimes[, i, regime.index], na.rm = TRUE)
      }
    }
  }else {
    regimes.meanL <- NULL
  }

i=1
cur.node <- nodes$AC[i]
    uncensored <- IsUncensored(inputs$uncensored, nodes$C, cur.node)
    deterministic.origdata <- IsDeterministic(inputs$data, cur.node, inputs$deterministic.Q.function, nodes, called.from.estimate.g=TRUE, inputs$survivalOutcome)$is.deterministic #deterministic due to death or Q.function



      form <- inputs$gform[i]
      deterministic.g.list.origdata <- IsDeterministicG(inputs$data, cur.node, inputs$deterministic.g.function, nodes, using.newdata=F) #deterministic due to deterministic.g.function - using original data
      deterministic.g.origdata <- deterministic.g.list.origdata$is.deterministic
        subs <- uncensored & !deterministic.origdata & !deterministic.g.origdata


#g.est <- ltmle:::Estimate(inputs, form=form, Qstar.kplus1=NULL, subs=subs, family=quasibinomial(), type="response", nodes=nodes, called.from.estimate.g=TRUE, calc.meanL=inputs$variance.method != "ic", cur.node=cur.node, regimes.meanL=regimes.meanL, regimes.with.positive.weight=1:num.regimes) #assume all regimes have positive weight for some final.Ynode

 Qstar.kplus1=NULL
 family=quasibinomial()
 type="response"
 nodes=nodes
 called.from.estimate.g=TRUE
 calc.meanL=inputs$variance.method != "ic"
 cur.node=cur.node
 regimes.meanL=regimes.meanL
 regimes.with.positive.weight=1:num.regimes


source(paste0(here::here(),"/temp_functions.R"))



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
   SL.library <- if(called.from.estimate.g){
     inputs$SL.library.g
   }else{
     inputs$SL.library.Q
   }

   use.glm <- (is.glm(SL.library) || length(RhsVars(f)) == 0)

   first.regime <- min(regimes.with.positive.weight)

   if(is.null(Qstar.kplus1)){
     data.with.Qstar <- data
   }else{
     if(is.matrix(Qstar.kplus1)){
       data.with.Qstar <- cbind(data, Q.kplus1 = Qstar.kplus1[, first.regime])
     }else{
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
   predicted.values <- deterministic.Q <- matrix(NA, nrow(data),num.regimes)
   is.deterministic <- matrix(FALSE, nrow(data), num.regimes)
   fit.and.predict <- NULL
   multiple.subs <- is.matrix(subs)
   multiple.Qstar <- is.matrix(Qstar.kplus1)
   if(calc.meanL){
     prob.A.is.1.meanL <- array(NaN, dim = c(nrow(inputs$data),
                                             num.regimes, length(nodes$LY) - 1))
     Anode.index <- which(nodes$A < cur.node)
   }else{
     prob.A.is.1.meanL <- NULL
   }

   regime.index=1
   #for(regime.index in regimes.with.positive.weight){
     newdata <- SetA(data = data.with.Qstar, regimes = inputs$regimes,
                     Anodes = nodes$A, regime.index = regime.index, cur.node = cur.node)
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
     }else{
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

     # if (!all(deterministic.list.newdata$is.deterministic |
     #          deterministic.g.list.newdata$is.deterministic)) {
     #   if (is.null(fit.and.predict) || multiple.Qstar ||
     #       multiple.subs) {

     #DEBUG HERE:
         #fit.and.predict <- FitAndPredict()

     if (length(Y.subset) < 2)
       stop("Estimation failed because there are fewer than 2 observations to fit")
     Y.subset.range <- range(Y.subset)
     if (anyNA(Y.subset.range))
       stop("Internal error - NA in Y during Estimate")
     if (Y.subset.range[1] < -1e-04 || Y.subset.range[2] >
         1.0001)
       stop("Internal error - Y negative or greater than 1 in Estimate")
     # if (Y.subset.range[2] - Y.subset.range[1] < 1e-04) {
     #   Y.value <- Y.subset.range[2]
     #   m <- list("no estimation occured because all Y values are the same",
     #             Y.value = Y.value)
     #   predicted.values <- ValuesByType(rep(Y.value, nrow(newdata)))
     #   class(m) <- "no.Y.variation"
     # }
     # else {
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
         try.result <- try({
           SuppressGivenWarnings(m <- SuperLearner::SuperLearner(Y = Y.subset,
                                                                 X = X.subset, SL.library = SL.library, cvControl = inputs$SL.cvControl,
                                                                 verbose = FALSE, family = family, newX = newX.list$newX,
                                                                 obsWeights = observation.weights.subset,
                                                                 id = id.subset, env = environment(SuperLearner::SuperLearner)),
                                 c("non-integer #successes in a binomial glm!",
                                   "prediction from a rank-deficient fit may be misleading"))
         })


         #predicted.values <- ProcessSLPrediction(m$SL.predict,newX.list$new.subs, try.result)

         pred=m$SL.predict
         new.subs=newX.list$new.subs



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


     #   }
     # }

      #----------------------------------------------------------
         m <- fit.and.predict$m
         predicted.values[, regime.index] <- fit.and.predict$predicted.values
     #   }
     #   else {
     #     predicted.values[, regime.index] <- PredictOnly(newdata)
     #   }


         newdata1 = newdata
         newX.list <- GetNewX(newdata1)
         pred <- ProcessSLPrediction(predict(m, newX.list$newX,
                                             X.subset, Y.subset, onlySL = TRUE)$pred, newX.list$new.subs,
                                     try.result = NULL)
         summary(pred)

         pred2 <- ProcessSLPrediction(predict(m, newX.list$newX,
                                             onlySL = TRUE)$pred, newX.list$new.subs,
                                     try.result = NULL)
         summary(pred2)
         methods(predict)

         predict.SL.glmnet
         predict.SuperLearner

     #   if (calc.meanL)
     #     prob.A.is.1.meanL[, regime.index, ] <- PredictProbAMeanL()
     # }else{
     #   m <- "all rows are deterministic, no estimation took place"
     # }
     #
     # predicted.values[deterministic.g.list.newdata$is.deterministic,
     #                  regime.index] <- deterministic.g.list.newdata$prob1
     # if (calc.meanL)
     #   prob.A.is.1.meanL[deterministic.g.list.newdata$is.deterministic,
     #                     regime.index, ] <- deterministic.g.list.newdata$prob1
     # is.deterministic[, regime.index] <- deterministic.list.newdata$is.deterministic
     # if (!called.from.estimate.g)
     #   deterministic.Q[deterministic.list.newdata$is.deterministic,
     #                   regime.index] <- deterministic.list.newdata$Q
     # if (isTRUE(attr(SL.library, "return.fit", exact = TRUE))) {
     #   fit[[regime.index]] <- m
     # }else{
     #   if (use.glm) {
     #     if (class(m)[1] %in% c("speedglm", "glm")) {
     #       fit[[regime.index]] <- summary(m)$coefficients
     #     }
     #     else {
     #       stopifnot(class(m)[1] %in% c("no.Y.variation",
     #                                    "character"))
     #       fit[[regime.index]] <- m
     #     }
     #   }
     #   else {
     #     capture.output(print.m <- print(m))
     #     fit[[regime.index]] <- print.m
     #   }
     # }















