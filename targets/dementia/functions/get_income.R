


get_income <- function(pnr, nobs){
  
  income=importSAS('X:/Data/Rawdata_Hurtig/706582/husstandsindk.sas7bdat',
                   filter=pnr,obs=nobs, keep=c('pnr','year','indiv_indk_index'))

  return(income)
}
