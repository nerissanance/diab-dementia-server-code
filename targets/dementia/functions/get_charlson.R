

get_charlson <- function(diagnoses,pnr){
 
  ######################################################################
  #
  # Charlson index and components at time of entry
  # looking back 5 years - currently from first ever secondary treatment
  #
  ######################################################################
  diag <- merge(diagnoses,pnr[.(pnr,secdate)],by="pnr")

  charlson <- charlsonIndex(diag,ptid='pnr',vars='diag',data.date='inddto',charlson.date='secdate')
  charlsoncode <- charlson[[1]]
  charlsoncomp <- charlson[[2]]
  
  ######################################################################
  #
  # AMI - dementia - and other covariates - defined by first date
  #
  ######################################################################
  covariates <- c(charlson.codes["heart.failure"],
                  charlson.codes["renal.disease"],
                  charlson.codes["chronic.pulmonary.disease"],
                  #c() #add vector of stroke codes
                  charlson.codes["dementia"],
                  charlson.codes["any.malignancy"])
  covariates <- c(covariates,list(
    ischemic.heart.disease=c("411","412","413","414","DI20","DI23","DI24","DI25"),
    myocardial.infarction=c("410","I21"),
    hypertension=c(paste0('40',1:4),'41009','41099','DI10','DI109','DI11','DI110','DI119','DI119A',         
                   'DI12','DI120','DI129','DI129A','DI13','DI130','DI131','DI132','DI139','DI15','DI150','DI151','DI152','DI158','DI159')
    #stroke: using code's from Christian and Thomas's (Lee 2020) paper
    ,stroke=c('433','434','435','436','437','438','DI63','DI64')
    # ,
    # dementia_newdef=c(charlson.codes["dementia"],
    # "2930", "2931", "DG318", "DG319")
  ))
  
  cov <- findCondition(diag,"diag",c("pnr","inddto"),covariates,match="start")
  setkeyv(cov,c("pnr","X","inddto"))
  cov <- cov[,.SD[1],by=c("pnr","X")]
  cov <- dcast(cov,pnr~X,value.var="inddto")
  return(cov)
}
