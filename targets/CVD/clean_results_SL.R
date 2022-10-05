# produce table with estimated parameter effects  
clean_results_SL <- function(fit, TCP="", analysis="", outcome="dementia"){
    results <- foreach(e=c("tmle","iptw"),.combine="rbind")%do%{
        res <- summary(fit, estimator=e)
        foreach(p=c("ATE","RR"),.combine="rbind")%do%{
            data.table(
                estimator=e,
                parameter=p,
                estimate=res$effect.measures[[p]]$estimate,
                std.dev=res$effect.measures[[p]]$std.dev,
                ci.lb=res$effect.measures[[p]]$CI[1],
                ci.ub=res$effect.measures[[p]]$CI[2],
                pvalue=res$effect.measures[[p]]$pvalue)
        }
    }
    results$TCP <- TCP  
    results$analysis <- analysis
    results$outcome <- outcome
    results$SL_lib <- paste0(SL.library,collapse = "/")
    return(results)
}
