

run_ltmle_simple <- function(d,
                             SL.library = c("SL.glm"),
                             varmethod = "ic", #variance method
                             override_function=SuperLearner_override,
                             id=NULL){
  abar_spec = list(rep(1,2),rep(0,2))
  set.seed(12345)
  fit <- res <- NULL


  package_stub("SuperLearner", "SuperLearner", override_function, {
    testthatsomemore::package_stub("ltmle", "Estimate", Estimate_override, {
      try(fit <- ltmle(data=d,
                       Anodes = c("A1","A2"),
                       Lnodes = c("L2"),
                       Ynodes = c("Y2","Y3"),
                       survivalOutcome = T,
                       abar = abar_spec,
                       estimate.time=F,
                       SL.library = SL.library,
                       variance.method = varmethod,
                       id=id
      ))
    })})


  if(!is.null(fit)){
    res <- summary(fit)
    res.iptw <- summary(fit, estimator="iptw")
    res.RR <- as.data.frame(res$effect.measures$RR)
    res.ate <- as.data.frame(res$effect.measures$ATE) %>% rename(ate.long.name=long.name,ate=estimate, ate.sd=std.dev , ate.pval=pvalue, ate.ci.lb=CI.2.5., ate.ci.ub=  CI.97.5., ate.log.std.err=log.std.err)

    res.RR.iptw <- as.data.frame(res.iptw$effect.measures$RR) %>% rename(iptw.long.name=long.name, iptw.estimate=estimate, iptw.sd=std.dev , iptw.pval=pvalue, iptw.ci.lb=CI.2.5., iptw.ci.ub=  CI.97.5., iptw.log.std.err=log.std.err)
    res.ate.iptw <- as.data.frame(res$effect.measures$ATE) %>% rename(iptw.ate.long.name=long.name, iptw.ate=estimate, iptw.ate.sd=std.dev , iptw.ate.pval=pvalue, iptw.ate.ci.lb=CI.2.5., iptw.ate.ci.ub=  CI.97.5., iptw.ate.log.std.err=log.std.err)

    res <- cbind(res.RR, res.ate, res.RR.iptw, res.ate.iptw)
  }
  return(res)
}
