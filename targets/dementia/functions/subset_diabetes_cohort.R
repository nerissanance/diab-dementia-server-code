if(FALSE){
  tar_load(diabetes_pop)
  diabetes_cohort=diabetes_pop 
  tar_load(demographics)
  danish_pop=demographics 
tar_load(comorbidities) 
tar_load(doede)
  
}


subset_diabetes_cohort <- function(danish_pop, diabetes_cohort, 
                                   comorbidities,doede){
  #currently, diabetes_cohort is all danish people who have a second line treatment. 
  #now, subset to  age over 50, not recent immigration, and 2009 or later
  # get_diabetes_population() has previously subset to previous metformin
  #dim(diabetes_cohort)
  #dim(danish_pop)

  baseline <- merge(diabetes_cohort,danish_pop[,.(pnr,birth_date, first_ind,last_ind,first_ud,last_ud, origin,sex)], by="pnr")
  #browser()

  # Select age >=50 and year>=2009
  #dim(baseline)
  baseline[,age_base:=as.numeric((secdate-birth_date)/365.25)]
  baseline <- baseline[age_base>=50 & year(secdate)>=2009]
  #dim(baseline)
  

  
  
  #no insulin use before second line
  #table(baseline$first_other)
  #Remove folks whose first treatment after metformin was insulin
  #uncomment to see n
  #dim(baseline)
  #baseline2 <-baseline
  baseline <- baseline[first_other !="insulin",]
  #nrow(baseline2)-nrow(baseline)
  
  
  #Need to make sure second-line switch was after immigration or return to denmark 
  #to confirm a switch to 2nd line (versus treatment abroad)
  #Drop 5 who immigrated back within 6 months of 2ndline switch
  #dim(baseline)
  baseline <- baseline %>% filter(is.na(baseline$last_ind) | (baseline$secdate - baseline$last_ind)/365 > 0.5) %>% data.table()
  #dim(baseline)
  
  #who didn't have a prior dementia diagnosis
  #dim(baseline)
  baseline <- merge(baseline, comorbidities[,.(pnr, dementia )],by="pnr", all.x=TRUE,all.y=FALSE)
  baseline <- baseline %>%  mutate(prior_dementia=ifelse(is.na(dementia)|secdate<dementia,0,1)) %>% 
    subset(., select=-c(dementia)) %>%
    #filter(prior_dementia==0) %>% #note, not dropping to keep in CVD cohort
    as.data.table
  #dim(baseline)

  ##Notes: moving this up from a later file, for 
  # a few patients died prior to picking up medication
  setkey(baseline,"pnr")
  doede2 <- unique(doede)
  setkey(doede2,"pnr")
  
  baseline <- merge(baseline,doede2, all.x=T)
  # baseline <- baseline[secdate<doddato,]
  
  

  return(baseline)
}