
if(FALSE){
 base_dem <- tar_read(base_dem)
 comorbidities <- tar_read(comorbidities)
 drugdiab <- tar_read(drugdiab)
 lab <- tar_read(lab)
 final_date="2022-07-20"
 node_length=180
}

merge_longitudinal_raw_data <- function(base_dem,
                                        comorbidities,
                                        drugdiab, 
                                        node_length,
                                        final_date){
    base_dem[,inn:=secdate] # start of first secondary treatment
  
  ##ATTENTION: removing dementia from the calculation of end of follow up currently, 
  ## so that we can use data for other analyses, will need 
  ## to deal with this analytically 
   base_dem[,out:=pmin(doddato,as.Date(final_date),na.rm=TRUE)] # last date
   
  base_dem[,last_date:=out] #keep track of last date for def of censoring
  #moving the below exclusion to earlier in the files
   base_dem <- base_dem[out>inn] # ~10 patients "died" prior to picking up medication
   if(nrow(base_dem[inn>out,])>10)stop("ERROR: should only have n=10 people with this data issue. if more occur, check the data again before moving up thise threshold (this will need to also change in the flowchart)")
  # NEED TO DOUBLE CHECK WHY THIS IS HAPPENING
  
  base_dem[,dummy:=1] #necessary dummy for splitting
  
  # Select part of base_dem that needs splitting
  # Keep only variables defining intervals and dates that are part of splitting - plus dummy! and dementia
  base_dem_split <- base_dem[,.(pnr,inn,out,last_date,dummy,birth_date,doddato,secdate,dementia)]

  
  # Split by time of comorbidities covariates
  time_dem <- lexisTwo(base_dem_split,comorbidities,invars = c("pnr","inn","out","dummy"),
                       splitvars = c('any.malignancy','chronic.pulmonary.disease','heart.failure','hypertension',
                                     'ischemic.heart.disease','myocardial.infarction','renal.disease',"stroke"
                                     ))
  #Split by diabetes treatment
  time_dem <- lexisFromTo(time_dem,drugdiab,invars = c("pnr","inn","out","dummy"),
                          splitvars=c("pnr","start","end","value","drugdiab"))

  # Split in 6 month periods since baseline
  time_dem <- lexisSeq(time_dem,invars = c("pnr","inn","out","dummy"),
                       varname="secdate",splitvector = c(0,25*365,node_length),
                       format="seq",value="splittime")   
  ## table(time_dem$splittime)
  #Note note using nodes beyond 10 for analysis, but keep the others for tabulation
  
  
  # Split in 5 year age intervals
  # time_dem <- lexisSeq(time_dem,invars = c("pnr","inn","out","dummy"),
  #                      varname="birth_date",splitvector=c(0,100*365,5*365),
  #                      format="seq",value="age")
  # time_dem[,age:=age*5]
  
  
  # Outcome node -calculated as occurring at any time in the interval
  time_dem[,event_dementia:=0]
  time_dem[!is.na(dementia) & dementia>=inn & dementia<=out,event_dementia:=1]
  time_dem[,event_dementia:=max(event_dementia),by=c("pnr","splittime")]
  
  # Outcome node - death
  time_dem[,event_death:=0]
  time_dem[!is.na(doddato) & doddato>=inn & doddato<=out,event_death:=1]
  time_dem[,event_death:=max(event_death),by=c("pnr","splittime")]
  
  # Censoring node--dementia
  time_dem[,censor_dem:=0]
  time_dem[out==last_date & event_dementia !=1, censor_dem:=1]
  time_dem[,censor_dem:=max(censor_dem),by=c("pnr","splittime")]

  # outcome node: MACE (stroke, MI, death)
  time_dem[,MACE:=pmax(0,myocardial.infarction,stroke,event_death)]
  time_dem[,MACE2:=pmax(0,myocardial.infarction,stroke,event_death,heart.failure)]
  
  # Censoring node--MACE
  time_dem[,censor_MACE:=0]
  time_dem[out==last_date & MACE!=1, censor_MACE:=1]
  time_dem[,censor_MACE:=max(censor_MACE),by=c("pnr","splittime")]
  
  # Censoring node--MACE2 (including HF)
  time_dem[,censor_MACE2:=0]
  time_dem[out==last_date & MACE2!=1, censor_MACE2:=1]
  time_dem[,censor_MACE2:=max(censor_MACE2),by=c("pnr","splittime")]
  
  #time_dem <- time_dem %>%
   # group_by(pnr, splittime) %>% mutate(visitfreq=n()) %>% as.data.table()
  #equivalent way of doing that in data.table:
  time_dem[,visitfreq:=.N,by=c("pnr","splittime")]
  
  
  
  
  ###############################################################
  #
  # Add empty records so that all individuals complete records
  #
  ###############################################################
  setkeyv(time_dem,c("pnr","splittime"))
  pnr2 <- unique(time_dem[,pnr])
  splittime <- unique(time_dem[,splittime])
  pnr3 <- data.table(pnr=rep(pnr2,each=length(splittime)),splittime=rep(0:(length(splittime)-1),length(pnr2)))
  time_dem <- merge(time_dem,pnr3,by=c("pnr","splittime"),all=TRUE)
  
  
  # NOTE: for the outcome nodes, we need to make sure they stay at 1 through follow up
  # To do this, we can make indicators of when they jump to 1 and carry that 
  # forward for the rest of follow up, then take row max, as below:
  outcomes <- c("event_dementia","MACE","MACE2")
  for(outvar in outcomes){
    time_dem[get(outvar)==1,indicator:=1]
    #table(time_dem$indicator,time_dem$event_dementia,useNA="always")
    time_dem[,indicator:=nafill(indicator,type="locf"),by=pnr]
    #table(time_dem$indicator,time_dem$event_dementia2,useNA="always")
    time_dem[,(paste0(outvar)):=pmax(indicator,get(outvar),na.rm=T)]
  }
  
  
  #make sure data is sorted correctly
  time_dem <- time_dem %>% arrange(pnr, splittime)
  ## head(time_dem)
  
  
  
  return(time_dem)
  
  
}
