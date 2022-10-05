


get_drug_diabetes <- function(A10, druglist){
  


  drugdiab <- findCondition(A10,"atc",c("pnr","eksd"),druglist,match="start")
  
  # Prepare drugdiab for splitting in time intervals. The splitting program
  # requires that there are no time overlaps within a single drug class. The
  # use currently is to let any prescription identify treatment during the next 
  # six months OR until there is another prescription.
  setnames(drugdiab,c("X","eksd"),c("drugdiab","start"))
  setkeyv(drugdiab,c("pnr","drugdiab","start"))
  drugdiab[,end:=start+180]
  drugdiab[,end2:=shift(start,type="lead"),by=c("pnr","drugdiab")]
  drugdiab[,end:=pmin(end,end2,na.rm=TRUE)]
  drugdiab[,end2:=NULL]
  drugdiab[,value:=1]

  return(drugdiab)
}