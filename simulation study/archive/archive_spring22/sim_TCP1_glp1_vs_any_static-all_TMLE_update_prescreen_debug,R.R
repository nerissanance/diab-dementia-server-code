
rm(list=ls())
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))




gc()
d_wide_list <- readRDS(file=here("data/simulated_data_list.RDS"))
d_wide_list <- d_wide_list[1:2]
gc()


d<- d_wide_list[[1]][1:10000,]

# res <- run_ltmle_glmnet(d, resdf=NULL, Qint=TRUE, det.Q=FALSE, varmethod = "tmle", override_function=SuperLearner_override_lasso_prescreen)
# res



         N_time = 11 #number of time points you want to look at
         SL.library = c("SL.glmnet")
         resdf=NULL
         Qint=TRUE
         gcomp=F
         det.Q=FALSE
         varmethod = "tmle"
         override_function=SuperLearner_override_lasso_prescreen
         alt=FALSE
         label=""

  warn = getOption("warn")
  options(warn=-1)

  #clean competing events
  d <-clean_sim_data(d, N_time=N_time)

  #Use only first N time points
  d <- d %>% dplyr::select(!!(baseline_vars),matches(paste0("_(",paste0(0:(N_time-1),collapse="|"),")$")))


  spec_ltmle <- spec_analysis(data=d, c(long_covariates,"event_death_"),
                              baseline_vars, N_time,
                              Avars=c("glp1_"),
                              Yvars=c("event_dementia_"),
                              alt=alt)
  abar_spec = list(rep(1,N_time),rep(0,N_time))

  #Drop the baseline events
  spec_ltmle$data <- spec_ltmle$data %>% subset(., select = -c(event_death_0, censor_0, event_dementia_0))
  spec_ltmle$Cnodes = spec_ltmle$Cnodes[spec_ltmle$Cnodes!="censor_0"]
  spec_ltmle$Lnodes = spec_ltmle$Lnodes[spec_ltmle$Lnodes!="event_death_0"]
  spec_ltmle$Ynodes = spec_ltmle$Ynodes[spec_ltmle$Ynodes!="event_dementia_0"]

  set.seed(12345)
  res = NULL


  if(Qint){

    if(N_time==11){
      qform = c(
        insulin_0="Q.kplus1 ~ 1",
        insulin_1="Q.kplus1 ~ 1",
        event_dementia_1="Q.kplus1 ~ 1",
        insulin_2="Q.kplus1 ~ 1",
        event_dementia_2="Q.kplus1 ~ 1",
        insulin_3="Q.kplus1 ~ 1",
        event_dementia_3="Q.kplus1 ~ 1",
        insulin_4="Q.kplus1 ~ 1",
        event_dementia_4="Q.kplus1 ~ 1",
        insulin_5="Q.kplus1 ~ 1",
        event_dementia_5="Q.kplus1 ~ 1",
        insulin_6="Q.kplus1 ~ 1",
        event_dementia_6="Q.kplus1 ~ 1",
        insulin_7="Q.kplus1 ~ 1",
        event_dementia_7="Q.kplus1 ~ 1",
        insulin_8="Q.kplus1 ~ 1",
        event_dementia_8="Q.kplus1 ~ 1",
        insulin_9="Q.kplus1 ~ 1",
        event_dementia_9="Q.kplus1 ~ 1",
        insulin_10="Q.kplus1 ~ 1",
        event_dementia_10="Q.kplus1 ~ 1"
      )
    }

    if(N_time==2){
      qform = c(
        insulin_0="Q.kplus1 ~ 1",
        insulin_1="Q.kplus1 ~ 1",
        event_dementia_1="Q.kplus1 ~ 1")
    }
  }else{
    qform=NULL
  }


  if(det.Q){
    det.q.fun = det.Q.function
  }else{
    det.q.fun = NULL
  }


  environment(override_function) <- asNamespace('SuperLearner')
  assignInNamespace("SuperLearner", override_function, ns = "SuperLearner")

  environment(Estimate_override) <- asNamespace('ltmle')
  assignInNamespace("Estimate", Estimate_override, ns = "ltmle")


      # res <- ltmle(data=spec_ltmle$data,
      #                  Anodes = spec_ltmle$Anodes,
      #                  Cnodes = spec_ltmle$Cnodes,
      #                  Lnodes = spec_ltmle$Lnodes,
      #                  Ynodes = spec_ltmle$Ynodes,
      #                  survivalOutcome = T,
      #                  abar = abar_spec,
      #                  gcomp=gcomp,
      #                  Qform = qform,
      #                  estimate.time=T,
      #                  deterministic.Q.function = det.q.fun,
      #                  SL.library = SL.library,
      #                  variance.method = varmethod)

      data=spec_ltmle$data
      Anodes = spec_ltmle$Anodes
      Cnodes = spec_ltmle$Cnodes
      Lnodes = spec_ltmle$Lnodes
      Ynodes = spec_ltmle$Ynodes
      survivalOutcome = T
      abar = abar_spec
      gcomp=gcomp
      Qform = qform
      estimate.time=T
      deterministic.Q.function = det.q.fun
      SL.library = SL.library
      variance.method = varmethod

gform = NULL
rule = NULL
gbounds = c(0.01, 1)
Yrange = NULL
deterministic.g.function = NULL
stratify = FALSE
SL.cvControl = list()
estimate.time = TRUE
iptw.only = FALSE
observation.weights = NULL
id = NULL


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

        #---------------------------------------------------------------------------------------
         # LtmleMSMFromInputs(inputs)
        #---------------------------------------------------------------------------------------
          #---------------------------------------------------------------------------------------
          # MainCalcs(inputs)
          #---------------------------------------------------------------------------------------

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

        #---------------------------------------------------------------------------------------
        # g.list <- EstimateG(inputs)
        #---------------------------------------------------------------------------------------

        n <- nrow(inputs$data)
        num.regimes <- dim(inputs$regimes)[3]
        nodes <- inputs$all.nodes

        g <- cum.g <- cum.g.unbounded <- prob.A.is.1 <- array(NaN, dim=c(n, length(nodes$AC), num.regimes))
        if (inputs$variance.method == "ic") {
          cum.g.meanL <- cum.g.meanL.unbounded <- NULL
        } else {
          g.meanL <- cum.g.meanL <- cum.g.meanL.unbounded <- array(NaN, dim=c(n, length(nodes$AC), num.regimes, length(nodes$LY)-1))
        }

        fit <- vector("list", length(nodes$AC))
        names(fit) <- names(inputs$data)[nodes$AC]

        if (inputs$variance.method != "ic" && anyNA(inputs$regimes)) {
          regimes.meanL <- inputs$regimes
          for (i in seq_along(nodes$A)) {
            for (regime.index in 1:num.regimes) {
              regimes.meanL[is.na(regimes.meanL[, i, regime.index]), i, regime.index] <- Mode(inputs$regimes[, i, regime.index], na.rm = TRUE)
            }
          }
        } else {
          regimes.meanL <- NULL
        }

        # for (i in seq_along(nodes$AC)) {
        #   cur.node <- nodes$AC[i]
        #   uncensored <- IsUncensored(inputs$uncensored, nodes$C, cur.node)
        #   deterministic.origdata <- IsDeterministic(inputs$data, cur.node, inputs$deterministic.Q.function, nodes, called.from.estimate.g=TRUE, inputs$survivalOutcome)$is.deterministic #deterministic due to death or Q.function
        #   if (is.numeric(inputs$gform)) {
        #     if (!is.null(inputs$deterministic.g.function)) stop("deterministic.g.function is not compatible with numeric gform")
        #     prob.A.is.1[, i, ] <- inputs$gform[, i, ]
        #     g.est <- list(is.deterministic = deterministic.origdata)
        #     fit[[i]] <- "no fit due to numeric gform"
        #   } else {
        #     form <- inputs$gform[i]
        #     deterministic.g.list.origdata <- IsDeterministicG(inputs$data, cur.node, inputs$deterministic.g.function, nodes, using.newdata=F) #deterministic due to deterministic.g.function - using original data
        #     deterministic.g.origdata <- deterministic.g.list.origdata$is.deterministic
        #     if (inputs$stratify) {
        #       intervention.match <- InterventionMatch(inputs$intervention.match, nodes$A, cur.node=nodes$AC[i])
        #       subs <- uncensored & intervention.match & !deterministic.origdata & !deterministic.g.origdata
        #     } else {
        #       subs <- uncensored & !deterministic.origdata & !deterministic.g.origdata
        #     }
        #     g.est <- Estimate(inputs, form=form, Qstar.kplus1=NULL, subs=subs, family=quasibinomial(), type="response", nodes=nodes, called.from.estimate.g=TRUE, calc.meanL=inputs$variance.method != "ic", cur.node=cur.node, regimes.meanL=regimes.meanL, regimes.with.positive.weight=1:num.regimes) #assume all regimes have positive weight for some final.Ynode
        #     prob.A.is.1[, i, ] <- g.est$predicted.values
        #     fit[[i]] <- g.est$fit
        #   }
        #   #prob.A.is.1 is prob(a=1), gmat is prob(a=abar)
        #   #cur.abar can be NA after censoring/death if treatment is dynamic
        #   if (cur.node %in% nodes$A) {
        #     cur.abar <- AsMatrix(inputs$regimes[, nodes$A == cur.node, ])
        #     if (is.null(regimes.meanL)) {
        #       cur.abar.meanL <- cur.abar
        #     } else {
        #       cur.abar.meanL <- AsMatrix(regimes.meanL[, nodes$A == cur.node, ])
        #     }
        #   } else {
        #     cur.abar <- cur.abar.meanL <- matrix(1, nrow(inputs$data), num.regimes)  #if this is a cnode, abar is always 1 (uncensored)
        #   }
        #   g[, i, ] <- CalcG(AsMatrix(prob.A.is.1[, i, ]), cur.abar, g.est$is.deterministic)
        #   if (inputs$variance.method != "ic") {
        #     if (is.numeric(inputs$gform)) {
        #       if (anyNA(g[, i, ])) stop("Error - NA in numeric gform. There may not be NA values in gform (including after censoring if variance.method is 'tmle' or 'iptw'.")
        #       g.meanL[, i, , ] <- g[, i, ] #recyles
        #     } else {
        #       for (j in sseq(1, dim(g.meanL)[4])) {
        #         g.meanL[, i, , j] <- CalcG(AsMatrix(g.est$prob.A.is.1.meanL[, , j]), cur.abar.meanL, g.est$is.deterministic)
        #       }
        #     }
        #   }
        #   if (anyNA(g[uncensored, i, ])) stop("Error - NA in g. g should only be NA after censoring. If you passed numeric gform, make sure there are no NA values except after censoring. Otherwise something has gone wrong.")
        # }
        #
        #
        # for (regime.index in 1:num.regimes) {
        #   cum.g.list <- CalcCumG(AsMatrix(g[, , regime.index]), inputs$gbounds)
        #   cum.g[, , regime.index] <- cum.g.list$bounded
        #   cum.g.unbounded[, , regime.index] <- cum.g.list$unbounded
        #   if (inputs$variance.method != "ic") {
        #     for (j in sseq(1, dim(g.meanL)[4])) {
        #       cum.g.list <- CalcCumG(AsMatrix(g.meanL[, , regime.index, j]), inputs$gbounds)
        #       cum.g.meanL[, , regime.index, j] <- cum.g.list$bounded
        #       cum.g.meanL.unbounded[, , regime.index, j] <- cum.g.list$unbounded
        #     }
        #   }
        # }

        i=seq_along(nodes$AC)[1]
          cur.node <- nodes$AC[i]
          uncensored <- IsUncensored(inputs$uncensored, nodes$C, cur.node)
          deterministic.origdata <- IsDeterministic(inputs$data, cur.node, inputs$deterministic.Q.function, nodes, called.from.estimate.g=TRUE, inputs$survivalOutcome)$is.deterministic #deterministic due to death or Q.function
          # if (is.numeric(inputs$gform)) {
          #   if (!is.null(inputs$deterministic.g.function)) stop("deterministic.g.function is not compatible with numeric gform")
          #   prob.A.is.1[, i, ] <- inputs$gform[, i, ]
          #   g.est <- list(is.deterministic = deterministic.origdata)
          #   fit[[i]] <- "no fit due to numeric gform"
          # } else {
            form <- inputs$gform[i]
            deterministic.g.list.origdata <- IsDeterministicG(inputs$data, cur.node, inputs$deterministic.g.function, nodes, using.newdata=F) #deterministic due to deterministic.g.function - using original data
            deterministic.g.origdata <- deterministic.g.list.origdata$is.deterministic
            if (inputs$stratify) {
              intervention.match <- InterventionMatch(inputs$intervention.match, nodes$A, cur.node=nodes$AC[i])
              subs <- uncensored & intervention.match & !deterministic.origdata & !deterministic.g.origdata
            } else {
              subs <- uncensored & !deterministic.origdata & !deterministic.g.origdata
            }

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
            #g.est <- Estimate(inputs, form=form, Qstar.kplus1=NULL, subs=subs, family=quasibinomial(), type="response", nodes=nodes, called.from.estimate.g=TRUE, calc.meanL=inputs$variance.method != "ic", cur.node=cur.node, regimes.meanL=regimes.meanL, regimes.with.positive.weight=1:num.regimes) #assume all regimes have positive weight for some final.Ynode
            #g.est <- Estimate_override(inputs, form=form, Qstar.kplus1=NULL, subs=subs, family=quasibinomial(), type="response", nodes=nodes, called.from.estimate.g=TRUE, calc.meanL=inputs$variance.method != "ic", cur.node=cur.node, regimes.meanL=regimes.meanL, regimes.with.positive.weight=1:num.regimes) #assume all regimes have positive weight for some final.Ynode

      Qstar.kplus1=NULL
      family=quasibinomial()
      type="response"
      called.from.estimate.g=TRUE
      calc.meanL=inputs$variance.method != "ic"
      regimes.with.positive.weight=1:num.regimes
      source(paste0(here(),"/simulation study/temp_ltmle_functions.R"))

              stopifnot(type %in% c("link", "response"))
              num.regimes <- dim(inputs$regimes)[3]

              # if(form == "IDENTITY"){
              #   stopifnot(is.vector(Qstar.kplus1) == 1)
              #   predicted.values <- ValuesByType(matrix(Qstar.kplus1,
              #                                           nrow = nrow(inputs$data), ncol = num.regimes))
              #   fit <- as.list(rep("no fit because form == IDENTITY",
              #                      num.regimes))
              #   deterministic.list.olddata <- IsDeterministic(inputs$data,
              #                                                 cur.node, inputs$deterministic.Q.function, nodes,
              #                                                 called.from.estimate.g, inputs$survivalOutcome)
              #   is.deterministic <- matrix(deterministic.list.olddata$is.deterministic,
              #                              nrow = nrow(inputs$data), ncol = num.regimes)
              #   deterministic.Q <- matrix(NA, nrow(inputs$data), num.regimes)
              #   deterministic.Q[is.deterministic, ] <- deterministic.list.olddata$Q
              #   return(list(predicted.values = predicted.values, fit = fit,
              #               is.deterministic = is.deterministic, deterministic.Q = deterministic.Q,
              #               prob.A.is.1.meanL = NULL))
              # }
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
              use.glm <- (ltmle:::is.glm(SL.library) || length(RhsVars(f)) == 0)
              first.regime <- min(regimes.with.positive.weight)
              if(is.null(Qstar.kplus1)) {
                data.with.Qstar <- data
              }else{
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
               #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
               # colnames(X) <- paste0("Xx.", 1:ncol(X))
               #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
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
              }else{
                prob.A.is.1.meanL <- NULL
              }






              #OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

              # for (regime.index in regimes.with.positive.weight) {
              #   newdata <- SetA(data = data.with.Qstar, regimes = inputs$regimes,
              #                   Anodes = nodes$A, regime.index = regime.index, cur.node = cur.node)
              #   if (calc.meanL) {
              #     if (!is.null(regimes.meanL)) {
              #       newdata.meanL <- SetA(data = data.with.Qstar,
              #                             regimes = regimes.meanL, Anodes = nodes$A,
              #                             regime.index = regime.index, cur.node = cur.node)
              #     }
              #     else {
              #       newdata.meanL <- newdata
              #     }
              #   }
              #   deterministic.list.newdata <- IsDeterministic(newdata,
              #                                                 cur.node, inputs$deterministic.Q.function, nodes,
              #                                                 called.from.estimate.g, inputs$survivalOutcome)
              #   if (called.from.estimate.g && !is.null(inputs$deterministic.g.function)) {
              #     newdata.with.current <- newdata
              #     stopifnot(cur.node %in% nodes$AC)
              #     if (cur.node %in% nodes$A) {
              #       newdata.with.current[, cur.node] <- inputs$regimes[,
              #                                                          which(nodes$A == cur.node), regime.index]
              #     }
              #     else {
              #       newdata.with.current <- newdata
              #     }
              #     deterministic.g.list.newdata <- IsDeterministicG(newdata.with.current,
              #                                                      cur.node, inputs$deterministic.g.function, nodes,
              #                                                      using.newdata = T)
              #   }
              #   else {
              #     deterministic.g.list.newdata <- list(is.deterministic = rep(FALSE,
              #                                                                 nrow(data)), prob1 = NULL)
              #   }
              #   if (regime.index > first.regime && multiple.Qstar) {
              #     Y <- Qstar.kplus1[, regime.index]
              #   }
              #   if (regime.index == first.regime || multiple.subs) {
              #     single.subs <- if (multiple.subs)
              #       subs[, regime.index]
              #     else subs
              #     X.subset <- X[single.subs, , drop = FALSE]
              #     id.subset <- inputs$id[single.subs]
              #     if (any(single.subs))
              #       X.subset[, matrixStats::colAlls(X.subset == 0)] <- 1
              #     observation.weights.subset <- inputs$observation.weights[single.subs]
              #     offst.subset <- offst[single.subs]
              #   }
              #   if (regime.index == first.regime || multiple.subs ||
              #       multiple.Qstar) {
              #     Y.subset <- Y[single.subs]
              #     if (anyNA(Y.subset))
              #       stop("NA in Estimate")
              #   }
              #   if (!all(deterministic.list.newdata$is.deterministic |
              #            deterministic.g.list.newdata$is.deterministic)) {
              #     if (is.null(fit.and.predict) || multiple.Qstar ||
              #         multiple.subs) {
              #       fit.and.predict <- FitAndPredict()
              #       m <- fit.and.predict$m
              #       predicted.values[, regime.index] <- fit.and.predict$predicted.values
              #     }
              #     else {
              #       predicted.values[, regime.index] <- PredictOnly(newdata)
              #     }
              #     if (calc.meanL)
              #       prob.A.is.1.meanL[, regime.index, ] <- PredictProbAMeanL()
              #   }
              #   else {
              #     m <- "all rows are deterministic, no estimation took place"
              #   }
              #   predicted.values[deterministic.g.list.newdata$is.deterministic,
              #                    regime.index] <- deterministic.g.list.newdata$prob1
              #   if (calc.meanL)
              #     prob.A.is.1.meanL[deterministic.g.list.newdata$is.deterministic,
              #                       regime.index, ] <- deterministic.g.list.newdata$prob1
              #   is.deterministic[, regime.index] <- deterministic.list.newdata$is.deterministic
              #   if (!called.from.estimate.g)
              #     deterministic.Q[deterministic.list.newdata$is.deterministic,
              #                     regime.index] <- deterministic.list.newdata$Q
              #   if (isTRUE(attr(SL.library, "return.fit", exact = TRUE))) {
              #     fit[[regime.index]] <- m
              #   }
              #   else {
              #     if (use.glm) {
              #       if (class(m)[1] %in% c("speedglm", "glm")) {
              #         fit[[regime.index]] <- summary(m)$coefficients
              #       }
              #       else {
              #         stopifnot(class(m)[1] %in% c("no.Y.variation",
              #                                      "character"))
              #         fit[[regime.index]] <- m
              #       }
              #     }
              #     else {
              #       capture.output(print.m <- print(m))
              #       fit[[regime.index]] <- print.m
              #     }
              #   }
              # }

              #OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

             # for (regime.index in regimes.with.positive.weight) {
              regime.index = regimes.with.positive.weight[1]

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
                }else {
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
                    X.subset[, matrixStats::colAlls(X.subset == 0)] <- 1
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
                #          deterministic.g.list.newdata$is.deterministic)){
                  # if (is.null(fit.and.predict) || multiple.Qstar ||
                  #     multiple.subs) {
                    #fit.and.predict <- FitAndPredict()

                save.image(file=here("data/debug_workspace.RData"))







#
#                     #------------------------------------------------------------
#
#                     m <- fit.and.predict$m
#                     predicted.values[, regime.index] <- fit.and.predict$predicted.values
#                   }
#                   else {
#                     predicted.values[, regime.index] <- PredictOnly(newdata)
#                   }
#                   if (calc.meanL)
#                     prob.A.is.1.meanL[, regime.index, ] <- PredictProbAMeanL()
#                 # }else{
#                 #   m <- "all rows are deterministic, no estimation took place"
#                 # }
#
#                 predicted.values[deterministic.g.list.newdata$is.deterministic,
#                                  regime.index] <- deterministic.g.list.newdata$prob1
#                 if (calc.meanL)
#                   prob.A.is.1.meanL[deterministic.g.list.newdata$is.deterministic,
#                                     regime.index, ] <- deterministic.g.list.newdata$prob1
#                 is.deterministic[, regime.index] <- deterministic.list.newdata$is.deterministic
#                 if (!called.from.estimate.g)
#                   deterministic.Q[deterministic.list.newdata$is.deterministic,
#                                   regime.index] <- deterministic.list.newdata$Q
#                 if (isTRUE(attr(SL.library, "return.fit", exact = TRUE))) {
#                   fit[[regime.index]] <- m
#                   }else{
#                   if(use.glm){
#                     if(class(m)[1] %in% c("speedglm", "glm")) {
#                       fit[[regime.index]] <- summary(m)$coefficients
#                     }else{
#                       stopifnot(class(m)[1] %in% c("no.Y.variation",
#                                                    "character"))
#                       fit[[regime.index]] <- m
#                     }
#                   }
#                   else{
#                     capture.output(print.m <- print(m))
#                     fit[[regime.index]] <- print.m
#                   }
#                 }
#               #}
#
#               g.est <- (list(predicted.values = predicted.values, fit = fit,
#                           is.deterministic = is.deterministic, deterministic.Q = deterministic.Q,
#                           prob.A.is.1.meanL = prob.A.is.1.meanL))
#
#
# #XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#
#
#                         prob.A.is.1[, i, ] <- g.est$predicted.values
#             fit[[i]] <- g.est$fit
#
#
#
#
#
#             #}
#           #prob.A.is.1 is prob(a=1), gmat is prob(a=abar)
#           #cur.abar can be NA after censoring/death if treatment is dynamic
#           if (cur.node %in% nodes$A) {
#             cur.abar <- AsMatrix(inputs$regimes[, nodes$A == cur.node, ])
#             if (is.null(regimes.meanL)) {
#               cur.abar.meanL <- cur.abar
#             } else {
#               cur.abar.meanL <- AsMatrix(regimes.meanL[, nodes$A == cur.node, ])
#             }
#           } else {
#             cur.abar <- cur.abar.meanL <- matrix(1, nrow(inputs$data), num.regimes)  #if this is a cnode, abar is always 1 (uncensored)
#           }
#           g[, i, ] <- CalcG(AsMatrix(prob.A.is.1[, i, ]), cur.abar, g.est$is.deterministic)
#           if (inputs$variance.method != "ic") {
#             if (is.numeric(inputs$gform)) {
#               if (anyNA(g[, i, ])) stop("Error - NA in numeric gform. There may not be NA values in gform (including after censoring if variance.method is 'tmle' or 'iptw'.")
#               g.meanL[, i, , ] <- g[, i, ] #recyles
#             } else {
#               for (j in sseq(1, dim(g.meanL)[4])) {
#                 g.meanL[, i, , j] <- CalcG(AsMatrix(g.est$prob.A.is.1.meanL[, , j]), cur.abar.meanL, g.est$is.deterministic)
#               }
#             }
#           }
#           if (anyNA(g[uncensored, i, ])) stop("Error - NA in g. g should only be NA after censoring. If you passed numeric gform, make sure there are no NA values except after censoring. Otherwise something has gone wrong.")
#         }
#
#
#         for (regime.index in 1:num.regimes) {
#           cum.g.list <- CalcCumG(AsMatrix(g[, , regime.index]), inputs$gbounds)
#           cum.g[, , regime.index] <- cum.g.list$bounded
#           cum.g.unbounded[, , regime.index] <- cum.g.list$unbounded
#           if (inputs$variance.method != "ic") {
#             for (j in sseq(1, dim(g.meanL)[4])) {
#               cum.g.list <- CalcCumG(AsMatrix(g.meanL[, , regime.index, j]), inputs$gbounds)
#               cum.g.meanL[, , regime.index, j] <- cum.g.list$bounded
#               cum.g.meanL.unbounded[, , regime.index, j] <- cum.g.list$unbounded
#             }
#           }
#         }
#
#
#
#
#
#
#
#
#
#
#          g.list <- list(cum.g=cum.g, cum.g.unbounded=cum.g.unbounded, cum.g.meanL=cum.g.meanL, fit=ReorderFits(fit), prob.A.is.1=prob.A.is.1, cum.g.meanL.unbounded=cum.g.meanL.unbounded)
#
#
#         #---------------------------------------------------------------------------------------
#         iptw <- CalcIPTW(inputs, g.list$cum.g, all.msm.weights)
#         fit <- list(g=g.list$fit)
#         if (inputs$iptw.only) {
#           beta <- rep(NA, length(iptw$beta))
#           fitted.msm <- NULL
#           variance.estimate <- NULL
#           fixed.tmle <- list(cum.g.used=array(NA, dim=dim(g.list$cum.g)))
#         } else {
#           for (j in 1:num.final.Ynodes) {
#             fixed.tmle <- FixedTimeTMLE(inputs, nodes = SubsetNodes(inputs$all.nodes, final.Ynode=inputs$final.Ynodes[j]), msm.weights = drop3(all.msm.weights[, , j, drop=FALSE]), combined.summary.measures = dropn(inputs$combined.summary.measures[, , , j, drop=FALSE], n=4), g.list = g.list)
#             IC <- IC + fixed.tmle$IC
#             IC.y[, , j] <- fixed.tmle$IC
#             Qstar[, , j] <- fixed.tmle$Qstar # n x num.regimes
#             new.var.y[, , j] <- fixed.tmle$est.var
#           }
#           fit <- c(fit, fixed.tmle$fit)
#           if (isTRUE(attr(inputs$data, "called.from.estimate.variance", exact=TRUE))) {
#             return(list(IC=matrix(NA, 1, 1), msm=NULL, beta=qlogis(mean(Qstar)), cum.g=g.list$cum.g, cum.g.unbounded=g.list$cum.g.unbounded, fit=fit, variance.estimate=NULL, beta.iptw=iptw$beta, IC.iptw=iptw$IC, Qstar=Qstar, cum.g.used=fixed.tmle$cum.g.used))
#           }
#           fitted.msm <- FitPooledMSM(inputs$working.msm, Qstar, inputs$combined.summary.measures, all.msm.weights * inputs$observation.weights)
#           IC <- FinalizeIC(IC, inputs$combined.summary.measures, Qstar, fitted.msm$m.beta, all.msm.weights, inputs$observation.weights, inputs$id) #n x num.betas
#           C.old <- NormalizeIC(IC, inputs$combined.summary.measures, fitted.msm$m.beta, all.msm.weights, inputs$observation.weights, g.ratio = NULL) #C without using g.ratio
#           g.ratio <- CalcGUnboundedToBoundedRatio(g.list, inputs$all.nodes, inputs$final.Ynodes)
#           CheckForVarianceWarning(inputs, g.ratio)
#           if (inputs$variance.method == "ic") {
#             variance.estimate <- NULL
#           } else {
#             new.var <- matrix(NA, num.betas, num.betas)
#             for (i in 1:num.betas) {
#               for (j in 1:num.betas) {
#                 if (num.final.Ynodes > 1) {
#                   cov.IC <- cov(IC.y[, i, ], IC.y[, j, ])
#                   diag(cov.IC) <- new.var.y[i, j, ]
#                   new.var[i, j] <- sum(cov.IC)
#                 } else {
#                   new.var[i, j] <- new.var.y[i, j, 1]
#                 }
#               }
#             }
#
#             C <- NormalizeIC(IC, inputs$combined.summary.measures, fitted.msm$m.beta, all.msm.weights, inputs$observation.weights, g.ratio)
#             variance.estimate <- safe.solve(C) %*% new.var %*% t(safe.solve(C))
#           }
#           IC <- t(safe.solve(C.old, t(IC))) #IC %*% solve(C)
#           beta <- coef(fitted.msm$m)
#           names(beta) <- inputs$beta.names
#         }
#
#         results<-(list(IC=IC, msm=fitted.msm$m, beta=beta, cum.g=g.list$cum.g, cum.g.unbounded=g.list$cum.g.unbounded, fit=fit, variance.estimate=variance.estimate, beta.iptw=iptw$beta, IC.iptw=iptw$IC, Qstar=Qstar, cum.g.used=fixed.tmle$cum.g.used)) #note: only returns cum.g and fit and cum.g.used for the last final.Ynode
#
#
#
#         #---------------------------------------------------------------------------------------
#
#           result$gcomp <- inputs$gcomp
#           result$formulas <- list(Qform=inputs$Qform, gform=inputs$gform)
#           result$binaryOutcome <- inputs$binaryOutcome
#           result$transformOutcome <- inputs$transformOutcome
#           result$survivalOutcome <- inputs$survivalOutcome
#           msm.result <- (result)
#
#
#           # num.regimes <- dim(inputs$regimes)[3]
#           # stopifnot(num.regimes %in% 1:2)
#           # if (num.regimes == 2) {
#           #   class(msm.result) <- "ltmleEffectMeasures"
#           #   return(msm.result)
#           # }
#           # names(msm.result$beta.iptw) <- names(msm.result$beta) <- NULL
#           # iptw <- plogis(msm.result$beta.iptw)
#           # iptw.list <- list(iptw.estimate=iptw, iptw.IC=iptw*(1-iptw)*msm.result$IC.iptw[, 1])
#           #
#           # r <- list()
#           # if (inputs$iptw.only) {
#           #   tmle <- NA
#           #   tmle.IC <- rep(NA, nrow(inputs$data))
#           # } else {
#           #   tmle <- plogis(msm.result$beta)
#           #   tmle.IC <- msm.result$IC[, 1] #only one regime
#           # }
#           # r$estimates <- c(tmle=tmle, iptw=iptw.list$iptw.estimate)
#           # r$IC <- list(tmle=tmle.IC * tmle * (1 - tmle), iptw=iptw.list$iptw.IC)
#           # if (!is.null(msm.result$variance.estimate)) {
#           #   stopifnot(length(msm.result$variance.estimate) == 1)
#           #   r$variance.estimate <- msm.result$variance.estimate[1] * (tmle * (1 - tmle))^2
#           # }
#           #
#           # if (inputs$gcomp) {
#           #   names(r$estimates)[1] <- names(r$IC)[1] <- "gcomp"
#           # }
#           #
#           # r$cum.g <- AsMatrix(msm.result$cum.g[, , 1]) #only one regime
#           # r$cum.g.unbounded <- AsMatrix(msm.result$cum.g.unbounded[, , 1]) #only one regime
#           # r$cum.g.used <- AsMatrix(msm.result$cum.g.used[, , 1]) #only one regime
#           # r$gcomp <- inputs$gcomp
#           # r$fit <- msm.result$fit
#           # r$fit$g <- r$fit$g[[1]]  #only one regime
#           # r$fit$Q <- r$fit$Q[[1]]  #only one regime
#           # r$Qstar <- msm.result$Qstar[, 1, 1] #1 regime, 1 final.Ynode
#           #
#           # r$formulas <- msm.result$formulas
#           # r$binaryOutcome <- msm.result$binaryOutcome
#           # r$transformOutcome <- msm.result$transformOutcome==TRUE #Want to store transformOutcome flag without attributes
#           #
#           # if (msm.result$transformOutcome) {
#           #   Yrange <- attr(msm.result$transformOutcome, "Yrange")
#           #   #back transform estimate and IC
#           #   r$estimates <- r$estimates*diff(Yrange) + min(Yrange)
#           #   r$IC <- lapply(r$IC, function (IC) IC * diff(Yrange))
#           #   r$variance.estimate <- r$variance.estimate * (diff(Yrange))^2
#           # }
#           # class(r) <- "ltmle"
#           # return(r)
