if(FALSE){
  
  spec_ltmle=CVD_spec_TCP1
  SL.library="glm"
  N_time=5
  abar_spec=list(rep(1,N_time),rep(0,N_time))
  det.Q.function=NULL
  varmethod="ic"
  Nsub=Inf
}


#set Nsub to NULL to run full analysis, or it will run a subset of the data
ltmle_analysis_SL <- function(spec_ltmle,  SL.library, abar_spec,
                           det.Q.function=NULL, varmethod, Nsub=NULL){
  
  if(!is.null(Nsub)&!is.infinite(Nsub)){
    spec_ltmle$data <- spec_ltmle$data[1:Nsub,]
  }

      res_RR <- ltmle(data=spec_ltmle$data,
                      Anodes = spec_ltmle$Anodes,
                      Cnodes = spec_ltmle$Cnodes,
                      Lnodes = spec_ltmle$Lnodes,
                      Ynodes = spec_ltmle$Ynodes,
                      survivalOutcome = TRUE,
                      abar = abar_spec,
                      deterministic.Q.function = det.Q.function,
                      SL.library = SL.library,
                      variance.method = varmethod )
    
  
  return(res_RR)
}
