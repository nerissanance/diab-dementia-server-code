if(FALSE){
  
  diabetes_pop=tar_read(diabetes_pop)
  year=2011
  danish_pop=tar_read(demographics)
  tar_load(comorbidities)
  tar_load(doede)
  tar_load(wide_df)
  diabetes_pop=tar_read(diabetes_pop)
  
}


make_flowchart <- function(diabetes_pop=diabetes_pop,
                           year, 
                           danish_pop=demographics,  
                           comorbidities=comorbidities,
                           doede=doede,
                           wide_df=wide_df
                           ){
  
  
  #currently, diabetes_pop is all danish people who have a second line treatment. 
  #now, subset to  age over 50, not recent immigration, and year or later
  # get_diabetes_population() has previously subset to previous metformin
  #dim(diabetes_pop)
  #dim(danish_pop)
  
  mat_exclusions <- c("Metformin use+started secondline, ever",nrow(diabetes_pop),"")
    
  baseline <- merge(diabetes_pop,danish_pop[,.(pnr,birth_date, first_ind,last_ind,first_ud,last_ud, origin,sex)], by="pnr")
  #browser()
  
  # Select age >=50 and year>=year
  #dim(baseline)
  baseline[,age_base:=as.numeric((secdate-birth_date)/365.25)]
  
  
  mat_exclusions <- rbind(mat_exclusions,
                          c(paste0("excl started <year: ",year),"",nrow( baseline[data.table::year(secdate)<year])),
                          c("after YR", nrow(baseline[year(secdate)>=year]),""),
                          c("excl age<50","",nrow( baseline[year(secdate)>=year & age_base<50])),
                          c("after YR and >=50yo", nrow(baseline[age_base>=50 & year(secdate)>=year]),""))

  
  baseline <- baseline[age_base>=50 & year(secdate)>=year]
  #dim(baseline)
  
  bl2 <-  baseline[first_other !="insulin" & (is.na(baseline$last_ind) | 
                                                       (baseline$secdate - baseline$last_ind)/365 > 0.5),]
  
  mat_exclusions <- rbind(mat_exclusions,
                          c("excl insulin use at bl","",nrow(baseline[first_other =="insulin",]))
                          ,c("cohort excl insulin","",nrow(baseline[first_other !="insulin",]))
                          #exclucing people who immigrated back within 6 mo of secondline switch
                          ,c("excl not in DK at switch","",nrow(baseline[first_other !="insulin" &!is.na(baseline$last_ind) & 
                                                                           ((baseline$secdate - baseline$last_ind)/365 <= 0.5),])),
                          c("base cohort",nrow(bl2),"")
  )
  
  #add in regime info and censoring info here
  
  
   #Also adding in possible exclusions for baseline HbA1C values
  mat_exclusions <- rbind(mat_exclusions,
                          c("final cohort wide_df: ", nrow(wide_df),"")
                          ,c("excl no hba1c in 6 prior 6mo","",nrow(wide_df[is.na(hba1c_0),]))
                          ,c("excl no hba1c in 6 prior 12mo","",nrow(wide_df[is.na(hba1c365_0),])))
                          
    
    
    
    # Some extra data checks, while we're at it!
    # make sure all these datasets have unique PNRs (no duplicate records)
    if(
      length(unique(diabetes_pop$pnr))!=nrow(diabetes_pop) 
      
    )stop("CHECK FOR DUPLICATE PNRS")
  
  # if(
  #   nrow(wide_df)!= nrow(basecohort)
  # )stop("these exclusions do not match the final N in wide_df. review and revise")
  # 
  
  # #who didn't have a prior dementia diagnosis
  # #dim(baseline)
  # baseline <- merge(baseline, comorbidities[,.(pnr, dementia )],by="pnr", all.x=TRUE,all.y=FALSE)
  # baseline <- baseline %>%  mutate(prior_dementia=ifelse(is.na(dementia)|secdate<dementia,0,1)) %>% 
  #   subset(., select=-c(dementia)) %>%
  #   #filter(prior_dementia==0) %>% #note, not dropping to keep in CVD cohort
  #   as.data.table
  # #dim(baseline)
  # 
  # ##Notes: moving this up from a later file, for 
  # # a few patients died prior to picking up medication
  # setkey(baseline,"pnr")
  # doede2 <- unique(doede)
  # setkey(doede2,"pnr")
  # 

  # 
  df <-data.frame(mat_exclusions,row.names=NULL)
  
 return(df) 
}
