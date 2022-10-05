#---------------------------------------------------------------------
#
# The start of diabetes is defined as the first date of any use of a
# hypoglycemic drug (A10). This is second line populations (not metformin)
#
#---------------------------------------------------------------------
get_diabetes_population <- function(A10, druglist){

    # # Ignore metformin
    # A10 <- A10[!(atc %in% c(druglist$metformin))]
    # # keep first record per patient: this is our definition of second line treatment
    # setkey(A10,pnr,eksd)
    # diabetes_population <- A10[A10[,.I[1],by="pnr"]$V1]
    # setnames(diabetes_population,"eksd","secdate")
    
    
    
    drugdiab <- findCondition(A10,"atc",c("pnr","eksd"), druglist, match="start")
    f_metformin <- drugdiab[X=="metformin"]
    setkeyv(f_metformin,c("pnr","eksd"))
    #selects earliest use of metformin:
    f_metformin <- f_metformin[,.SD[1],by="pnr"]
    setnames(f_metformin,"eksd","metd")
    f_metformin <- f_metformin[,.(pnr,metd)]
    diabetes_population <-drugdiab[X!="metformin"]
    #merge earliest metformin date with dates of first second line use
    diabetes_population <- merge(diabetes_population,f_metformin,by="pnr")
    #subset to those with 
    diabetes_population <- diabetes_population[eksd>=metd]
    
    #calculate time on metformin
    diabetes_population[,metformin_dur := eksd - metd]
    #sort in ascending order (earliest date is first by each pnr) then tame first obs
    setkeyv(diabetes_population,c("pnr","eksd"))
    diabetes_population <- diabetes_population[,.SD[1],by="pnr"]
    diabetes_population <- diabetes_population[,.(pnr,X,eksd,metformin_dur)]
    setnames(diabetes_population,c("X","eksd","metformin_dur"),c("first_other","secdate","metformin_dur"))
    # diabetes_population holds date of first secondary treatment (secdate) and the secondary treatment - 
    # only for those starting secondary treatment.
    
    return(diabetes_population)
}

