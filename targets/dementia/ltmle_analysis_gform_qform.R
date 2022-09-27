
#set Nsub to NULL to run full analysis, or it will run a subset of the data
ltmle_analysis <- function(spec_ltmle,  SL.library="SL.glmnet", abar_spec ,
                           det.Q.function=NULL, varmethod, Nsub=NULL){

    if(!is.null(Nsub)&!is.infinite(Nsub)){
        spec_ltmle$data <- spec_ltmle$data[1:Nsub,]
    }
    package_stub("SuperLearner", "SuperLearner", SuperLearner_override, {
        package_stub("ltmle", "Estimate", Estimate_override, {

          #attr(SL.library,"return.fit"==T)

          res_RR <- ltmle(data=spec_ltmle$data,
                            Anodes = spec_ltmle$Anodes,
                            Cnodes = spec_ltmle$Cnodes,
                            Lnodes = spec_ltmle$Lnodes,
                            Ynodes = spec_ltmle$Ynodes,
                            survivalOutcome = TRUE,
                            abar = abar_spec,
                            deterministic.Q.function = det.Q.function,
                            SL.library = SL.library,
                            variance.method = varmethod ,
                          gform=c(
                              glp1_1=""
                          )
            })})

    return(res_RR)
}
