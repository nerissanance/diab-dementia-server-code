get_demographics <- function(path_corona_update,filter,nobs){
    pop1 <- importSAS('x:/Data/Rawdata_Hurtig/706582/pop.sas7bdat',
                      date.vars = c("fdato","first_ind","last_ind","first_ud","last_ud"),
                      keep=c("sex","fdato","ie_type"),
                      filter = filter,obs=nobs)
    pop2 <- importSAS('V:/Data/Workdata/706582/Corona_update/Data/pop_9march2020.sas7bdat',
                      date.vars = c("fdato","bop_vfra","seneste_indvandring"),
                      keep=c("sex","fdato","ie_type","bop_vfra","seneste_indvandring"),
                      filter = filter,obs=nobs)
    pop3 <- importSAS(paste0(path_corona_update,'/pop_20jun2021.sas7bdat'),
                      date.vars = c("fdato","bop_vfra","seneste_indvandring"),
                      keep=c("sex","fdato","ie_type","bop_vfra","seneste_indvandring"),
                      filter = filter,obs=nobs)
    
    setnames(pop2,"seneste_indvandring","last_ind")
    setnames(pop3,"seneste_indvandring","last_ind")
    
    pop <- rbind(pop1,pop2,pop3,fill=TRUE)
    setkey(pop,"pnr")
    setnames(pop,"fdato","birth_date")
    pop[,sex:=factor(sex,levels=c("0","1"),labels=c("Female","Male"))]
    pop[,ie_type:=factor(ie_type,levels=c("1","2","3"),labels=c("Dane","Immigrant","Descendent"))]
    setnames(pop,"ie_type","origin")
    # first entry per person
    pop <- pop[pop[,.I[1],by="pnr"]$V1]
    return(pop)
}
