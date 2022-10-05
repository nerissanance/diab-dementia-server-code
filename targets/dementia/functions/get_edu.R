

get_edu <- function(filter, nobs){
  ######################################################################
  #
  # Education - available until 2018
  # Missing education assume to be primary
  #
  ######################################################################
  
  edu <- importSAS('X:/Data/Rawdata_Hurtig/706582/uddan.sas7bdat',
                   keep=c('pnr','hfaudd','year'),filter = filter, obs=nobs)
  edu <- merge(edu,edu_code,by="hfaudd")
  edu <- edu[,.(pnr,year,code5txt)]  
  return(edu)
}




