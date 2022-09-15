
######################################################################
#
# Diagnoses for outcomes, exclusion and covariates
#
######################################################################

get_diagnoses <- function(path_corona_update,filter,nobs){
  
  diag1 <- importSAS('x:/Data/Rawdata_Hurtig/706582/diag_indl.sas7bdat', 
                     date.vars = "inddto", keep=c("pnr","diag","inddto","pattype"),
                     where="(substr(diag,1,1) in ('0','1','2','4','5')
                           or substr(diag,1,2) in ('DI','DG','DH','DJ','DM','DB','DK','DE','DN','DC'))
                           and pattype=0",filter = filter, obs=nobs)
  diag2 <- importSAS('V:/Data/Workdata/706582/Corona_update/Data/diag_indl_9march2020.sas7bdat',
                     date.vars = "inddto", keep=c("pnr","diag","inddto","pattype"),
                     where="substr(diag,1,2) in ('DI','DG','DH','DJ','DM','DB','DK','DE','DN','DC')
                           and pattype='0'",filter = filter, obs=nobs)
  diag3 <- importSAS(paste0(path_corona_update,'diag_indl_lpr3.sas7bdat'),
                where="substr(diag,1,2) in ('DI','DG','DH','DJ','DM','DB','DK','DE','DN','DC')", 
                filter=filter, obs=nobs,
                keep=c("pnr","diag","inddto","starttidspunkt","sluttidspunkt"))
  #ANDREW: why is this being filtered this way? 10 years of followup time?
  diag3 <- diag3[(sluttidspunkt-starttidspunkt)/(60*60)>12] 
  diag3[,c("starttidspunkt","sluttidspunkt"):=NULL]
  #diag3[,dif:=NULL]
  diag3[,inddto:=as.Date(inddto,format="%Y%m%d")]

    diag <- rbind(diag1,diag2,diag3,fill=TRUE)

  return(diag)
}