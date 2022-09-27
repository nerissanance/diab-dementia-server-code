

table_cont_usage <- function(long_df_full){
  
  tab_list=NULL
  
  d<- long_df_full %>% filter(censor==0, splittime<11)
  d<- d %>% group_by(pnr) %>% arrange(splittime) %>% filter(first(glp1)==1) %>%
    mutate(tot_use=cumsum(glp1))
  #table(d$splittime, d$glp1)
  tab <- table(d$splittime, d$splittime+1== d$tot_use)
  
  tab_list$glp1 <- tab
  
  
  d<- d %>% filter(censor==0, splittime<11)
  d<- d %>% group_by(pnr) %>% arrange(splittime) %>% filter(first(sglt2_inhib)==1) %>%
    mutate(tot_use=cumsum(sglt2_inhib))
  tab <- table(d$splittime, d$splittime+1== d$tot_use)
  
  tab_list$sglt2_inhib <- tab
  
  return(tab_list)
}