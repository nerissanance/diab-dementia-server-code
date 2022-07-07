# # need the devtools package to install from GitHub
# library(devtools)
# 
# # install survtmle from github
# install_github("benkeser/survtmle",ref="b7b3ccf0e01adc1cc958166ce5ccc0cc8aa9b5e9")
# 
# # load survtmle
# library(survtmle)

# load the rv144 data
data(rv144)

# look at the first 3 rows
head(rv144)

# empty result holders
est <- NULL
rslt <- vector(mode="list",length=6)

# get unbounded estimates at each time
for(time in 1:6){
  tmp.fit <- survtmle(ftime=rv144$ftime,
                      ftype=rv144$ftype,
                      adjustVars=rv144[,c("highRisk","male","medRisk","medAge","highAge")],
                      trt=rv144$vax,
                      t0=time,
                      method="mean",
                      glm.ftime="highRisk + medRisk + highAge + medAge",
                      glm.ctime="highRisk + medRisk + highAge + medAge + t",
                      returnIC = FALSE, returnModels = FALSE)
  # store just the point estimates in est
  est <- cbind(est, tmp.fit$est)
  # store the whole fit in rslt
  rslt[[time]][[1]] <- tmp.fit
}


#' convertBounds
#' 
#' This function takes as input a list of length two with each entry a data.frame
#' with named columns t and u1 (first entry) or u2 (second entry). The latter columns
#' are the bounds specified on the conditional cumulative incidence scale. The function
#' returns a list with two entries that contain the bounds on the iterated mean scale
#' for the two endpoints of interest. 

convertBounds <- function(bounds,t0){
  u1.tild <- (bounds[[1]]$u1[bounds[[1]]$t==t0])/(1 - c(0,bounds[[2]]$u2[1:t0]))
  u2.tild <- (bounds[[2]]$u2[bounds[[1]]$t==t0])/(1 - c(0,bounds[[1]]$u1[1:t0]))
  return(list(data.frame(t=1:t0,l1=0,u1=u1.tild[1:t0]),
              data.frame(t=1:t0,l2=0,u2=u2.tild[1:t0])))
}


# loop over time
for(time in 1:6){
  # each time create an empty list of 6 entries -- one for each bound multiplier
  rslt[[time]][[2]] <- vector(mode="list",length=6)
  # dummy counter
  ct <- 0
  # loop over bound multipliers
  for(m in c(1.25,1.5,1.75,2,2.5,3)){
    # add to counter
    ct <- ct + 1
    # make a list of bounds for this multiplier to be input into convertBounds
    # notice that these bounds are based on the cumulative incidence 
    # in the placebo arm from the unbounded tmles fit earlier
    bounds0 <- list(data.frame(t=1:6, l1=0, u1=est[1,]*m),
                    data.frame(t=1:6, l2=0, u2=est[3,]*m))
    # call convertBounds to put bounds on cumulative incidence scale
    boundsTime <- convertBounds(bounds=bounds0, t0=time)
    
    # call survtmle with this set of bounds for this time point
    tmp.fit <- survtmle(ftime=rv144$ftime,
                        ftype=rv144$ftype,
                        adjustVars=rv144[,c("highRisk","male","medRisk","medAge","highAge")],
                        trt=rv144$vax,
                        t0=time,
                        method="mean",
                        glm.ftime="highRisk + medRisk + highAge + medAge",
                        glm.ctime="highRisk + medRisk + highAge + medAge + t",
                        bounds=boundsTime,
                        returnIC = FALSE, returnModels = FALSE)
    # store results
    rslt[[time]][[2]][[ct]] <- tmp.fit
  } # end multiplier loop
} # end time loop 



survtmle

function (ftime, ftype, trt, adjustVars, t0 = max(ftime[ftype > 
                                                          0]), SL.ftime = NULL, SL.ctime = NULL, SL.trt = NULL, glm.ftime = NULL, 
          glm.ctime = NULL, glm.trt = NULL, returnIC = TRUE, returnModels = TRUE, 
          ftypeOfInterest = unique(ftype[ftype != 0]), trtOfInterest = unique(trt), 
          method = "hazard", bounds = NULL, verbose = FALSE, 
          tol = 1/(sqrt(length(ftime))), maxIter = 10, Gcomp = FALSE, 
          gtol = 0.001) 
{
  call <- match.call(expand.dots = TRUE)
  clean <- checkInputs(ftime = ftime, ftype = ftype, trt = trt, 
                       t0 = t0, adjustVars = adjustVars, SL.ftime = SL.ftime, 
                       SL.ctime = SL.ctime, SL.trt = SL.trt, glm.ftime = glm.ftime, 
                       glm.ctime = glm.ctime, glm.trt = glm.trt, returnIC = returnIC, 
                       returnModels = returnModels, ftypeOfInterest = ftypeOfInterest, 
                       trtOfInterest = trtOfInterest, bounds = bounds, verbose = verbose, 
                       tol = tol, Gcomp = Gcomp, method = method)
  if (method == "hazard") {
    tmle.fit <- hazard_tmle(ftime = clean$ftime, ftype = clean$ftype, 
                            trt = clean$trt, t0 = t0, adjustVars = clean$adjustVars, 
                            SL.ftime = clean$SL.ftime, SL.ctime = clean$SL.ctime, 
                            SL.trt = clean$SL.trt, glm.ftime = clean$glm.ftime, 
                            glm.ctime = clean$glm.ctime, glm.trt = clean$glm.trt, 
                            returnIC = returnIC, returnModels = returnModels, 
                            ftypeOfInterest = ftypeOfInterest, trtOfInterest = trtOfInterest, 
                            bounds = bounds, verbose = verbose, tol = tol, maxIter = maxIter, 
                            gtol = gtol)
  }
  else if (method == "mean") {
    tmle.fit <- mean_tmle(ftime = clean$ftime, ftype = clean$ftype, 
                          trt = clean$trt, t0 = t0, adjustVars = clean$adjustVars, 
                          SL.ftime = clean$SL.ftime, SL.ctime = clean$SL.ctime, 
                          SL.trt = clean$SL.trt, glm.ftime = clean$glm.ftime, 
                          glm.ctime = clean$glm.ctime, glm.trt = clean$glm.trt, 
                          returnIC = returnIC, returnModels = returnModels, 
                          ftypeOfInterest = ftypeOfInterest, trtOfInterest = trtOfInterest, 
                          bounds = bounds, verbose = verbose, tol = tol, Gcomp = Gcomp, 
                          gtol = gtol)
  }
  out <- list(call = call, est = tmle.fit$est, var = tmle.fit$var, 
              meanIC = tmle.fit$meanIC, ic = tmle.fit$ic, ftimeMod = tmle.fit$ftimeMod, 
              ctimeMod = tmle.fit$ctimeMod, trtMod = tmle.fit$trtMod, 
              t0 = t0, ftime = tmle.fit$ftime, ftype = tmle.fit$ftype, 
              trt = tmle.fit$trt, adjustVars = tmle.fit$adjustVars)
  class(out) <- "survtmle"
  return(out)
}


mean_tmle
function (ftime, ftype, trt, t0 = max(ftime[ftype > 0]), adjustVars = NULL, 
          SL.ftime = NULL, SL.ctime = NULL, SL.trt = NULL, glm.ftime = NULL, 
          glm.ctime = NULL, glm.trt = "1", glm.family = "binomial", 
          returnIC = TRUE, returnModels = FALSE, ftypeOfInterest = unique(ftype[ftype != 
                                                                                  0]), trtOfInterest = unique(trt), bounds = NULL, verbose = FALSE, 
          Gcomp = FALSE, gtol = 0.001, ...) 
{
  n <- length(ftime)
  id <- seq_len(n)
  dat <- data.frame(id = id, ftime = ftime, ftype = ftype, 
                    trt = trt)
  if (!is.null(adjustVars)) {
    dat <- cbind(dat, adjustVars)
  }
  nJ <- length(ftypeOfInterest)
  allJ <- sort(unique(ftype[ftype != 0]))
  ofInterestJ <- sort(ftypeOfInterest)
  ntrt <- length(trtOfInterest)
  uniqtrt <- sort(trtOfInterest)
  trtOut <- estimateTreatment(dat = dat, ntrt = ntrt, uniqtrt = uniqtrt, 
                              adjustVars = adjustVars, SL.trt = SL.trt, glm.trt = glm.trt, 
                              returnModels = returnModels, gtol = gtol)
  dat <- trtOut$dat
  trtMod <- trtOut$trtMod
  dataList <- makeDataList(dat = dat, J = allJ, ntrt = ntrt, 
                           uniqtrt = uniqtrt, t0 = t0, bounds = bounds)
  censOut <- estimateCensoring(dataList = dataList, ntrt = ntrt, 
                               uniqtrt = uniqtrt, t0 = t0, verbose = verbose, adjustVars = adjustVars, 
                               SL.ctime = SL.ctime, glm.ctime = glm.ctime, glm.family = glm.family, 
                               returnModels = returnModels, gtol = gtol)
  dataList <- censOut$dataList
  ctimeMod <- censOut$ctimeMod
  wideDataList <- makeWideDataList(dat = dat, dataList = dataList, 
                                   adjustVars = adjustVars, t0 = t0, allJ = allJ, ntrt = ntrt, 
                                   uniqtrt = uniqtrt)
  timeAndType <- expand.grid(rev(seq_len(t0)), ofInterestJ)
  ftimeMod <- vector(mode = "list", length = length(ofInterestJ))
  names(ftimeMod) <- paste0("J", ofInterestJ)
  for (j in seq_along(ofInterestJ)) {
    ftimeMod[[j]] <- vector(mode = "list", length = t0)
    names(ftimeMod[[j]]) <- paste0("t", seq_len(t0))
  }
  for (i in seq_len(nrow(timeAndType))) {
    estOut <- estimateIteratedMean(wideDataList = wideDataList, 
                                   t = timeAndType[i, 1], whichJ = timeAndType[i, 2], 
                                   ntrt = ntrt, uniqtrt = uniqtrt, allJ = allJ, t0 = t0, 
                                   SL.ftime = SL.ftime, adjustVars = adjustVars, glm.ftime = glm.ftime, 
                                   verbose = verbose, returnModels = returnModels, bounds = bounds)
    wideDataList <- estOut$wideDataList
    eval(parse(text = paste0("ftimeMod$J", timeAndType[i, 
                                                       2], "$t", timeAndType[i, 1], "<-estOut$ftimeMod")))
    wideDataList <- fluctuateIteratedMean(wideDataList = wideDataList, 
                                          t = timeAndType[i, 1], whichJ = timeAndType[i, 2], 
                                          ntrt = ntrt, uniqtrt = uniqtrt, allJ = allJ, t0 = t0, 
                                          SL.ftime = SL.ftime, glm.ftime = glm.ftime, returnModels = returnModels, 
                                          bounds = bounds, Gcomp = Gcomp)
  }
  est <- rowNames <- NULL
  for (j in ofInterestJ) {
    for (z in seq_along(uniqtrt)) {
      thisEst <- eval(parse(text = paste("mean(wideDataList[[", 
                                         z + 1, "]]$Q", j, "star.1)", sep = "")))
      est <- rbind(est, thisEst)
      rowNames <- c(rowNames, paste(c(uniqtrt[z], j), collapse = " "))
      eval(parse(text = paste("wideDataList[[1]]$Q", 
                              j, "star.0.Z", uniqtrt[z], " <- rep(thisEst,n)", 
                              sep = "")))
      eval(parse(text = paste("wideDataList[[1]]$Q", 
                              j, "star.1.Z", uniqtrt[z], " <- wideDataList[[(z+1)]]$Q", 
                              j, "star.1", sep = "")))
    }
  }
  row.names(est) <- rowNames
  for (j in ofInterestJ) {
    for (z in seq_along(uniqtrt)) {
      for (t in rev(seq_len(t0))) {
        outcomeName <- ifelse(t == t0, paste("N", 
                                             j, ".", t0, sep = ""), paste("Q", 
                                                                          j, "star.", t + 1, sep = ""))
        eval(parse(text = paste("wideDataList[[1]]$D.Z", 
                                uniqtrt[z], ".", j, "star.", t, 
                                " <- wideDataList[[1]]$H", uniqtrt[z], 
                                ".", t, "*(wideDataList[[1]][,outcomeName] - wideDataList[[1]]$Q", 
                                j, "star.", t, ")", sep = "")))
      }
      eval(parse(text = paste("wideDataList[[1]]$D.Z", 
                              uniqtrt[z], ".", j, "star.0 <- wideDataList[[1]]$Q", 
                              j, "star.1.Z", uniqtrt[z], " - wideDataList[[1]]$Q", 
                              j, "star.0.Z", uniqtrt[z], sep = "")))
      ind <- eval(parse(text = paste("grep('D.Z", 
                                     uniqtrt[z], ".", j, "star', names(wideDataList[[1]]))", 
                                     sep = "")))
      eval(parse(text = paste("wideDataList[[1]]$IC", 
                              j, "star.Z", uniqtrt[z], " <- rowSums(cbind(rep(0, nrow(wideDataList[[1]])),wideDataList[[1]][,ind]))", 
                              sep = "")))
    }
  }
  infCurves <- wideDataList[[1]][, grep("IC", names(wideDataList[[1]])), 
                                 drop = FALSE]
  meanIC <- apply(infCurves, MARGIN = 2, FUN = mean)
  var <- t(as.matrix(infCurves)) %*% as.matrix(infCurves)/(n^2)
  row.names(var) <- colnames(var) <- rowNames
  out <- list(est = est, var = var, meanIC = meanIC, ic = infCurves, 
              trtMod = trtMod, ftimeMod = ftimeMod, ctimeMod = ctimeMod, 
              ftime = ftime, ftype = ftype, trt = trt, adjustVars = adjustVars)
  class(out) <- "survtmle"
  return(out)
}




survtmle:::estimateIteratedMean
function (wideDataList, t, whichJ, allJ, t0, adjustVars, SL.ftime = NULL, 
          glm.ftime = NULL, verbose, returnModels = FALSE, bounds = NULL, 
          ...) 
{
  include <- rep(TRUE, nrow(wideDataList[[1]]))
  if (t != 1) {
    for (j in allJ) {
      include[wideDataList[[1]][[paste0("N", j, ".", 
                                        t - 1)]] == 1] <- FALSE
    }
    include[wideDataList[[1]][[paste0("C.", t - 1)]] == 
              1] <- FALSE
  }
  outcomeName <- ifelse(t == t0, paste("N", whichJ, ".", 
                                       t0, sep = ""), paste("Q", whichJ, "star.", 
                                                            t + 1, sep = ""))
  wideDataList <- lapply(wideDataList, function(x, t) {
    if (length(allJ) > 1) {
      x[[paste0("NnotJ.", t - 1)]] <- rowSums(cbind(rep(0, 
                                                        nrow(x)), x[, paste0("N", allJ[allJ != 
                                                                                         whichJ], ".", t - 1)]))
    }
    else {
      x[[paste0("NnotJ.", t - 1)]] <- 0
    }
    x
  }, t = t)
  lj.t <- paste0("l", whichJ, ".", t)
  uj.t <- paste0("u", whichJ, ".", t)
  Qtildej.t <- paste0("Qtilde", whichJ, ".", t)
  Nj.tm1 <- paste0("N", whichJ, ".", t - 1)
  Qj.t <- paste0("Q", whichJ, ".", t)
  NnotJ.tm1 <- paste0("NnotJ.", t - 1)
  Qform <- paste(outcomeName, "~", glm.ftime, sep = " ")
  if (is.null(SL.ftime)) {
    if (is.null(bounds)) {
      suppressWarnings({
        Qmod <- fast_glm(reg_form = stats::as.formula(Qform), 
                         data = wideDataList[[1]][include, ], family = stats::binomial())
        if (unique(class(Qmod) %in% c("glm", "lm"))) {
          Qmod <- cleanglm(Qmod)
        }
        wideDataList <- lapply(wideDataList, function(x, 
                                                      whichJ, t) {
          suppressWarnings(x[[Qj.t]] <- x[[Nj.tm1]] + 
                             (1 - x[[NnotJ.tm1]] - x[[Nj.tm1]]) * predict(Qmod, 
                                                                          newdata = x, type = "response"))
          x
        }, t = t, whichJ = whichJ)
      })
    }
    else {
      X <- stats::model.matrix(stats::as.formula(Qform), 
                               data = wideDataList[[1]][include, ])
      Ytilde <- (wideDataList[[1]][include, outcomeName] - 
                   wideDataList[[1]][[lj.t]][include])/(wideDataList[[1]][[uj.t]][include] - 
                                                          wideDataList[[1]][[lj.t]][include])
      Qmod <- stats::optim(par = rep(0, ncol(X)), fn = LogLikelihood, 
                           Y = Ytilde, X = X, method = "BFGS", gr = grad, 
                           control = list(reltol = 1e-07, maxit = 50000))
      beta <- Qmod$par
      wideDataList <- lapply(wideDataList, function(x, 
                                                    j, t) {
        newX <- stats::model.matrix(stats::as.formula(Qform), 
                                    data = x)
        x[[Qj.t]] <- x[[Nj.tm1]] + (1 - x[[NnotJ.tm1]] - 
                                      x[[Nj.tm1]]) * (plogis(newX %*% beta) * (x[[uj.t]] - 
                                                                                 x[[lj.t]]) + x[[lj.t]])
        x
      }, j = whichJ, t = t)
    }
  }
  else if (is.null(glm.ftime)) {
    if (is.null(bounds)) {
      nUniq <- length(unique(wideDataList[[1]][include, 
                                               outcomeName]))
      cvControl <- SuperLearner::SuperLearner.CV.control()
      if (t == t0) {
        nE <- sum(wideDataList[[1]][include, outcomeName])
        ignoreSL <- nE <= 2
        if (ignoreSL) {
          suppressWarnings({
            Qform_trt <- paste(outcomeName, "~", 
                               "trt", sep = " ")
            Qmod <- fast_glm(reg_form = stats::as.formula(Qform_trt), 
                             data = wideDataList[[1]][include, ], family = stats::gaussian())
            wideDataList <- lapply(wideDataList, function(x, 
                                                          whichJ, t) {
              suppressWarnings(x[[Qj.t]] <- x[[Nj.tm1]] + 
                                 (1 - x[[NnotJ.tm1]] - x[[Nj.tm1]]) * 
                                 predict(Qmod, newdata = data.frame(trt = x$trt)))
              x
            }, t = t, whichJ = whichJ)
          })
        }
        else {
          simplify <- nE <= cvControl$V
          if (simplify) 
            cvControl <- list(V = nE - 1, stratifyCV = TRUE)
          suppressWarnings(Qmod <- SuperLearner::SuperLearner(Y = wideDataList[[1]][include, 
                                                                                    outcomeName], X = wideDataList[[1]][include, 
                                                                                                                        c("trt", names(adjustVars))], SL.library = SL.ftime, 
                                                              cvControl = cvControl, family = "binomial", 
                                                              verbose = verbose))
          wideDataList <- lapply(wideDataList, function(x, 
                                                        whichJ, t) {
            x[[Qj.t]] <- x[[Nj.tm1]] + (1 - x[[NnotJ.tm1]] - 
                                          x[[Nj.tm1]]) * predict(Qmod, newdata = x[, 
                                                                                   c("trt", names(adjustVars))], onlySL = TRUE)$pred
            x
          }, t = t, whichJ = whichJ)
        }
      }
      else {
        suppressWarnings(Qmod <- SuperLearner::SuperLearner(Y = wideDataList[[1]][include, 
                                                                                  outcomeName], X = wideDataList[[1]][include, 
                                                                                                                      c("trt", names(adjustVars))], SL.library = SL.ftime, 
                                                            cvControl = cvControl, family = "binomial", 
                                                            verbose = verbose))
        wideDataList <- lapply(wideDataList, function(x, 
                                                      whichJ, t) {
          suppressWarnings(x[[Qj.t]] <- x[[Nj.tm1]] + 
                             (1 - x[[Nj.tm1]] - x[[NnotJ.tm1]]) * predict(Qmod, 
                                                                          newdata = x[, c("trt", names(adjustVars))], 
                                                                          onlySL = TRUE)$pred)
          x
        }, t = t, whichJ = whichJ)
      }
    }
    else {
      stop("Super Learner code with bounds not written yet")
    }
  }
  out <- list(wideDataList = wideDataList, ftimeMod = if (returnModels == 
                                                          TRUE) {
    Qmod
  } else {
    NULL
  })
  return(out)
}