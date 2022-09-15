# produce table with estimated parameter effects  
clean_results <- function(fit, TCP="", analysis="", outcome="dementia"){
    results <- foreach(e=c("tmle","iptw"),.combine="rbind")%do%{
        res <- summary(fit, estimator=e)
        foreach(p=c("ATE","RR"),.combine="rbind")%do%{
            data.table(
                estimator=e,
                parameter=p,
                estimate=round(res$effect.measures[[p]]$estimate,6),
                std.dev=round(res$effect.measures[[p]]$std.dev,6),
                ci.lb=round(res$effect.measures[[p]]$CI[1],6),
                ci.ub=round(res$effect.measures[[p]]$CI[2],6),
                pvalue=round(res$effect.measures[[p]]$pvalue),6)
        }
    }
    results$TCP <- TCP  
    results$analysis <- analysis
    results$outcome <- outcome
    return(results)
}
