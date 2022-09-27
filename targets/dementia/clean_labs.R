if (FALSE){
  tar_load(lab)
  tar_load(base_dem)
  node_length=180
  final_date="2022-07-20"
  breaks_hba1c=c(0,48,53,58,70,150)
}

# 
# VARIABLES CREATED here in the output (wide format data):
#   - hba1c_ : time-varying A1C in the interval length time_node. missing is NA
#   - hba1c365_ : time-varying A1C allowing for 365 day gaps between measures
#                 (probably using hba1c_0 to subset the data)
#   - hba1c_lvcf_: carrying forward hba1c values through the end of each person's follow up time
#   - censor_hba1c_: new censoring variable that turns on when there is more than a
#                     one-node gap in A1C measurement
#                     (to be integrated into censoring node for dynamic tx regime)
#   - missing_a1c_: indicator of missingness of A1C (to be included in models when lvcf is included)
#   - mediator_a1c_: for mediation analysis, product of indicators for 
#                    (1) being measured =1 and (2) being controlled (<=48) =1
# 

clean_labs <- function(lab, 
                       base_dem,
                       node_length,
                       breaks_hba1c=breaks_hba1c,
                       final_date="2022-07-20"
                       ){
  
    #Addition 7-25-2022 adding in A1C values from labs (Nerissa/Thomas)
    #takes the most recent in the interval
    #remove outliers/unreasonable values (cutoffs from Bochra)
    hba1c <- lab[class=="HBA1C"& (res)<150 & (res)>0] 
    #if there are multiple values in the same day (very rare), take the mean:
    hba1c <- hba1c[,.(hba1c=mean(res)),by=c("pnr","start")]
    #summary(hba1c$res)
    hba1c[,end:=start+1]
    lab[,class:=NULL]
    setnames(hba1c,"start","date")
    pop=base_dem[,.(pnr,date=secdate)]
    setkey(hba1c,pnr,date)
    setkey(pop,pnr,date)
    # get latest value of HBA1C (if any) before second line treatment
    
    ## pop=hba1c[pop,roll=Inf]
    # in 6 months long intervals find current HBA1C value
    intervals=seq(0,11*node_length,node_length)
    grid <- pop[,.(date=date+intervals),by=pnr]
    # each interval has a number starting at zero, take the most recent value
    grid[,interval:=0:(length(intervals)-1),by=pnr]
    #restrict to current followup
    grid=grid[date<=as.Date(final_date)]
    setkey(grid,pnr,date)
    #allow for up to a year of carry forward
    grid0 <- hba1c[grid,roll=(365)]
    grid1 <- hba1c[grid,roll=(node_length)]
    grid2 <- hba1c[grid,roll=(Inf)]
    
    #add indicator for missingness to grid1 (true node length)
    grid1[,missing_a1c:=as.numeric(is.na(hba1c))]
    
    #create cutoff points and apply them to these variables
    makebins <- function(x){cut(x, breaks=breaks_hba1c)}
    grid0$hba1c <- makebins(grid0$hba1c)
    grid1$hba1c <- makebins(grid1$hba1c)
    grid2$hba1c <- makebins(grid2$hba1c)
    
    ##Addition Aug 10, 2022
    ## for zeyi mediation analysis we want: 
    # indicator that HBA1C is controlled
    # indicator that HBA1C is measured 
    # product of the two= mediator_a1c
    #make "controlled" indicator for true node length
    grid1[,controlled_a1c:=as.numeric(hba1c=="(0,48]")]
    grid1[,mediator_a1c:= (controlled_a1c)*(!is.na(hba1c))]

    ##Create one lvcf dataset with less categories (in case of positivity issues)
    grid2$hba1c2 <- dplyr::recode(grid2$hba1c,
      "(0,48]"="(0,48]",
      "(48,53]"="(48,53]",
      "(53,58]"="(53,58]",
      "(58,70]"="(58,150]",
      "(70,150]"="(58,150]" 
    )
    
    ##addition 4-8-22: we want to create a new indicator that can be used for 
    ##censoring by HbA1C as part of a new stochastic regime
    ##this indicator will take the value of 1 in the time interval X time nodes 
    ##after the first missing hba1c measurement.  
    grid1[,indicator:=as.numeric(is.na(hba1c))]
    #lag values by (1) time nodes (allows for one 180-day gap), by id
    grid1[,indicator2:=shift(indicator,1L,fill=0,type="lag"),by=pnr]
    ##grid1[50:100, c("pnr","indicator","indicator2"),with=F]
    #Now take the PRODUCT of the lagged value and the missingness indicator
      #will be 0 except if both are 1 (means missing)
    grid1[,prod:=(indicator*indicator2)]
    #carry forward 1 for the rest of follow up, by id
    grid1[,censor_hba1c:=as.numeric(cumsum(prod)>0),by=pnr]
    grid1[,indicator:=NULL]
    grid1[,indicator2:=NULL]
  
    
    
    #reshape all data long to wide
    hba1c_wide <- data.table::dcast(grid0[,c("pnr","hba1c","interval")],
                                    pnr~interval,value.var=c("hba1c"))
    setnames(hba1c_wide,c("pnr",paste0("hba1c365_",0:11)))
    
    hba1c_wide1 <- data.table::dcast(grid1[,c("pnr","hba1c","censor_hba1c","interval","mediator_a1c","missing_a1c")],
                                    pnr~interval,value.var=c("hba1c","censor_hba1c","mediator_a1c","missing_a1c"))
    
    hba1c_wide2 <- data.table::dcast(grid2[,c("pnr","hba1c","hba1c2","interval")],
                                     pnr~interval,value.var=c("hba1c","hba1c2"))
    setnames(hba1c_wide2,c("pnr",paste0("hba1c_lvcf_",0:11),paste0("hba1c_lvcf2_",0:11)))
    
    
    #merge all data together
    hba1c_widecat <- hba1c_wide[hba1c_wide1[hba1c_wide2]]
    
    #add in the number of measurements in the year prior to secondline start
    getbaseline <- merge(base_dem[,c("pnr","secdate")],hba1c[,c("pnr","date")],by="pnr",all=F)
    getbaseline <-getbaseline[date%between%list(secdate-365,secdate),]
    getbaseline[,bl_ct_hba1c:=.N,by="pnr"]
    getbaseline <- unique(getbaseline[,.(pnr,bl_ct_hba1c)])
    
    hba1c_widecat<- merge(hba1c_widecat,getbaseline,by="pnr",all=T)
    
    setkey(hba1c_widecat,pnr)
    
    

    
    
    
     
    return(hba1c_widecat)
}
