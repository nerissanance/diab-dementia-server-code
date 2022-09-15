if(FALSE){
  filter=tar_read(diabetes_pop)[,.(pnr=pnr)]
  nobs=5000
  
}

get_region <- function(path=path_root_x,filter=diabetes_pop[,.(pnr=pnr)],nobs=nobs){
  
  
  kommune <- importSAS(paste0(path_root_x,"kommune.sas7bdat"),filter = filter, obs=nobs)
  kommune$kom <- as.integer(kommune$kom)

  if(!is.numeric(kommune$kom)) stop('Input to kommuneRegion must be integer')
  
  
  make_region <-function(x){
    fifelse(x %in% c(773,787,810,813,820,825,840,846,849,851,860),'Nordjylland',
                   fifelse(x %in% c(101,147,151,153,155,157,159,161,163,165,167,169,173,175,183,185,187,190,
                                          201,210,217,219,223,230,240,250,260,270,400,411),"Hovedstaden",
                           fifelse(x %in% c(253,259,265,269,306,316,320,326,329,330,336,340,350,360,370,376,390)
                                   ,'Sj?lland',
                                   fifelse(x %in% c(615,657,661,665,671,706,707,710,727,730,740,741,746,751,756,760,766,
                                                          779,791),'Midtjylland',
                                           fifelse(x %in% c(410,420,430,440,450,461,479,480,482,492,510,530,540,550,
                                                            561,563,573,575,
                                                                  580,607,621,630),'Sydjylland',
                                                   '')))))}
  
 kommune$region <-make_region(kommune$kom)
 
 region <- kommune[,c("pnr","year","region")]
 region$year <- as.integer(region$year)
  return(region)
  
  }
  