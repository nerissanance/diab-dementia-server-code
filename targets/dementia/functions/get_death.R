

get_death <- function(path_corona_update, filter, nobs){
  
  doede <- importSAS('X:/Data/Rawdata_Hurtig/706582/doede.sas7bdat',
                     keep=c("pnr","doddato","dod_ami","dod_cv","dod_stroke"),date.vars = "doddato",
                     filter = filter, obs=nobs)
  doede_ny <- importSAS(paste0(path_corona_update,'/doede.sas7bdat'),
                        date.vars = "doddato",
                        filter = filter, obs=nobs)
  doede <- rbind(doede,doede_ny,fill=TRUE)
  for(x in c("dod_ami","dod_stroke","dod_cv"))
    doede[get(x)=='.',(x):=NA]
  
  return(doede)
}
 