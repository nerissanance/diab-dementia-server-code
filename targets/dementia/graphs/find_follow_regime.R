
find_followup <- function(data,t){
  #subset to risk set (no outcome or censor at time t)
  data <- data.table(data)
  data2 <- data[get(paste0("censor_",t))==0 & get(paste0("event_dementia_",(t-1)))==0,]
  a_sub <- rep(paste0("glp1_",1:t))
  #if sum of subsetted A nodes is = to follw up time, then they followed full regime
  data2$full_regime <-rowSums(data2[,..a_sub])

  full_regime_1 <- data2[full_regime==t,]
  full_regime_0 <- data2[full_regime==0,]


  return(list(full_regime_1,full_regime_0, t))
}

t1 <- find_followup(data=data,t=1)
t1[[1]]%>% select(event_dementia_1, censor_2, glp1_2) %>% gtsummary::tbl_summary()
