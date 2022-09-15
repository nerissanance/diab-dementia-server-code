# if(FALSE){
#   
#   spec_ltmle=tar_read(tmle_spec_TCP1)
#     abar_spec=list(rep(1,N_time-1),rep(0,N_time-1))
#   SL.library="glm" 
# }
boot_fun <- function(spec_ltmle, 
                     abar_spec, 
                     SL.library,
                     det.Q.function
                     ,package_stub
                     ,SuperLearner_override
                     ,Estimate_override){
  
  data_sub <- as.data.frame(spec_ltmle$data)
  #sampling with replacement:
  data_sub <- data_sub[sample(nrow(data_sub), replace=TRUE),]
  #get only distinct observations (dropping repeated observations)
  data_sub <- distinct(data_sub) %>% droplevels()
  
  package_stub("SuperLearner", "SuperLearner", SuperLearner_override, {
    package_stub("ltmle", "Estimate", Estimate_override, {
      fit <- ltmle(data=data_sub,
                   Anodes = spec_ltmle$Anodes,
                   Cnodes = spec_ltmle$Cnodes,
                   Lnodes = spec_ltmle$Lnodes,
                   Ynodes = spec_ltmle$Ynodes,
                   survivalOutcome = TRUE,
                   abar = abar_spec,
                   estimate.time=FALSE,
                   # deterministic.Q.function = NULL,
                   deterministic.Q.function = det.Q.function,
                   SL.library = SL.library,
                   variance.method = "ic") #ic for speed
    })})
  
  
  res <- summary(fit)
  res.iptw <- summary(fit, estimator="iptw")
  
  res_RR=data.frame(
    estimator="tmle",
    parameter="RR",
    estimate=res$effect.measures$RR$estimate,
    std.dev=res$effect.measures$RR$std.dev,
    ci.lb=res$effect.measures$RR$CI[1],
    ci.ub=res$effect.measures$RR$CI[2],
    pvalue=res$effect.measures$RR$pvalue)
  res_RR_iptw=data.frame(
    estimator="iptw",
    parameter="RR",
    estimate=res.iptw$effect.measures$RR$estimate,
    std.dev=res.iptw$effect.measures$RR$std.dev,
    ci.lb=res.iptw$effect.measures$RR$CI[1],
    ci.ub=res.iptw$effect.measures$RR$CI[2],
    pvalue=res.iptw$effect.measures$RR$pvalue)
  
  res_ATE=data.frame(
    estimator="tmle",
    parameter="ATE",
    estimate=res$effect.measures$ATE$estimate,
    std.dev=res$effect.measures$ATE$std.dev,
    ci.lb=res$effect.measures$ATE$CI[1],
    ci.ub=res$effect.measures$ATE$CI[2],
    pvalue=res$effect.measures$ATE$pvalue)
  res_ATE_iptw=data.frame(
    estimator="iptw",
    parameter="ATE",
    estimate=res.iptw$effect.measures$ATE$estimate,
    std.dev=res.iptw$effect.measures$ATE$std.dev,
    ci.lb=res.iptw$effect.measures$ATE$CI[1],
    ci.ub=res.iptw$effect.measures$ATE$CI[2],
    pvalue=res.iptw$effect.measures$ATE$pvalue)
  
  results <- bind_rows(res_RR, res_RR_iptw,
                       res_ATE, res_ATE_iptw)
  return(results)
}
