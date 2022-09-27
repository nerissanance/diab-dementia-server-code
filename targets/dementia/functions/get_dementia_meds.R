
#Add hypertension medicine and  dementia medication to charlson covariates


get_dementia_meds <- function(cov, path_corona_update, pnr, nobs){
  
  c1 <- importSAS('x:/Data/Rawdata_Hurtig/706582/lmdb.sas7bdat', 
                  date.vars = "eksd", where="atc=:'C'or atc=:'N06D'", filter=pnr, obs=nobs)
  c2 <- importSAS(paste0(path_corona_update,"/lmdb.sas7bdat"), 
                  date.vars = "eksd", where="atc=:'C' or atc=:'N06D'", filter=pnr, obs=nobs)
  c12 <- rbind(c1,c2,fill=TRUE)
  c12 <- c12[,.(pnr,eksd,atc)]
  hyp <- hypertensionMedication(c12,vars=c("pnr","atc","eksd"))
  hyp <- hyp[,.(pnr,hypertension)]
  setnames(hyp,"hypertension","hypertension_med")
  
  dementia_med <- c12[grepl("^N06D",atc)]
  setkeyv(dementia_med,c("pnr","eksd"))
  dementia_med <- dementia_med[,.SD[1],by="pnr"]
  dementia_med <- dementia_med[,.(pnr,eksd)]
  setnames(dementia_med,"eksd","dementia_med")
  #adjustment of cov
  cov <- Reduce(function(x,y){merge(x,y,all=TRUE,by="pnr")},list(cov,dementia_med,hyp))
  cov[,hypertension:=pmin(hypertension,hypertension_med,na.rm = TRUE)]
  cov[,dementia:=pmin(dementia,dementia_med,na.rm=TRUE)]
  # new definition considered
  # cov[,dementia:=pmin(dementia,dementia_newdef,na.rm=TRUE)]
  cov[,c("dementia_med","hypertension_med"):=NULL]

return(cov)
}
