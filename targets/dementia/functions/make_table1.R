
if(FALSE){
    setwd("z:/Workdata/706582/Andrew Mertens/targets_diabetes_dementia/")
    library(targets)
    base_dem=tar_read(base_dem) 
    wide_df=tar_read(wide_df)
    tmle_spec_TCP1=tar_read(tmle_spec_TCP1)
    outcome="event_dementia_"
    year=2009
    cvd_subset=NULL
    u=sapply(c("heaven","ltmle","data.table","tidyverse","SuperLearner","tictoc","glmnet","Matrix","Publish","matrixStats","speedglm","doParallel" ,"parallel" ,"snow" ,"doSNOW" ,"foreach"),function(x)do.call(library,list(x)))
}

make_table1 <- function(base_dem,wide_df, year=2009,outcome,cvd_subset){
    ## apply exclusions
    data1 <- wide_df[year(secdate)>=year,][,grepl("_[1-9]$|_10$|_11$",names(wide_df))==0,with=FALSE]
    if (outcome=="event_dementia_") {
        #drop those with prior dementia
        data1 <- data1[prior_dementia==0,]
    } else if(outcome%in%c("MACE_","MACE2_")){
        data1 <- data1[!is.na(wide_df[[cvd_subset]]),]
    }
    ##note: need to use base_dem to get the n's before imputation
    base <- base_dem[,-c("year.x","year.y","first_ind","last_ind","dod_cv",
                         "dod_stroke","dod_ami","doddato","last_ud","first_ud",
                         "birth_date","secdate")]
    setkey(base,pnr)
    #get baseline longitudinal covariates
    data1 <- data1 %>% dplyr::select(grep("(pnr|_0)",names(data1)))
    setkey(data1,pnr)
    ##merge longitudinal baseline to unimputed demographic characteristics
    data1 <- base[data1]
    #make formula
    ## formula = as.formula(paste0("glp1_0~",paste0(names(fulldf)[-1],collapse="+"),collapse=""))
    data=tmle_spec_TCP1$data[,grepl("_[1-9]$|_10$|_11$",names(tmle_spec_TCP1$data))==0,with=FALSE]
    formula=glp1_0 ~ Q(age_base) + sex + code5txt + quartile_income + Q(metformin_dur) + year_2nd_line_start + code5txt_miss + quartile_income_miss + insulin_0 + chronic.pulmonary.disease_0 + hypertension_0 + myocardial.infarction_0 + ischemic.heart.disease_0 + heart.failure_0 + renal.disease_0 + stroke_0 + any_second_line_0
    data1=data1[,c("glp1_0",intersect(names(data),names(data1))),with=FALSE]
    summ <- summary(utable(formula,data=data1[1:2000]))
    return(summ)
}


