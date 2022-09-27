


# Household size 
get_household <- function(pnr, nobs){

  filelist <- paste0('bef',1994:2020,'12.sas7bdat')
  household <- rbindlist(lapply(filelist,function(x){
    #browser()
    dat <- importSAS(paste0('X:/Data/Rawdata_Hurtig/706582/Data til COVID-19 studier/',x),
                     keep=c("pnr","familie_id"),
                     filter=pnr, obs=nobs)
    setkey(dat,"familie_id")
    dat[,year:=as.numeric(substr(x,4,7))]
    dat[,household:=.N,by="familie_id"]
    dat <- merge(dat,pnr,by="pnr")
    dat[,familie_id:=NULL]
    dat
  }))
  
  return(household)
}
