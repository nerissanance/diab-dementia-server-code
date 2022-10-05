

get_atc_codes <- function(path_corona_update,nobs){
  
  A10_1 <- importSAS('x:/Data/Rawdata_Hurtig/706582/lmdb.sas7bdat',
                     date.vars = "eksd", where="atc=:'A10'",
                     keep=c("pnr","eksd","atc"),
                     obs=nobs)
  A10_2 <- importSAS(paste0(path_corona_update,"/lmdb.sas7bdat"),
                     date.vars = "eksd", where="atc=:'A10'",
                     keep=c("pnr","eksd","atc"),
                     obs=nobs)
  
  A10 <- rbind(A10_1[eksd<=as.Date("2018-12-31")],A10_2,fill=TRUE)
  return(A10)
  
}