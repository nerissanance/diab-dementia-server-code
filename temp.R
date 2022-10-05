
res_vec <- NULL

seed_vec <- c(347347, 123, 22222, 1234, 2454325, 4252345,45238095, 2234, 1234, 123456,45742,3462356,23432,9999)
nsamp <- 115698
nsamp <- 115698


for(i in seed_vec){

  set.seed(i)
  u.always <- synthesizeDD.always(cc)

  d.always <- sim(u.always, nsamp)
  (prop.always <-
      1*(d.always$event_dementia_1 +
           d.always$event_dementia_2 +
           d.always$event_dementia_3 +
           d.always$event_dementia_4 +
           d.always$event_dementia_5 +
           d.always$event_dementia_6 +
           d.always$event_dementia_7 +
           d.always$event_dementia_8 +
           d.always$event_dementia_9 +
           d.always$event_dementia_10 >0)  %>% table %>% prop.table)


  set.seed(i)
  u.never <- synthesizeDD.never(cc)

  d.never <- sim(u.never, nsamp)

  (prop.never <-
      1*(d.never$event_dementia_1 +
           d.never$event_dementia_2 +
           d.never$event_dementia_3 +
           d.never$event_dementia_4 +
           d.never$event_dementia_5 +
           d.never$event_dementia_6 +
           d.never$event_dementia_7 +
           d.never$event_dementia_8 +
           d.never$event_dementia_9 +
           d.never$event_dementia_10 >0) %>% table %>% prop.table)


  (cRD <-  prop.always[2] - prop.never[2])
  (cRR <- (prop.always[2])/prop.never[2])
  cRD
  cRR

  res <- data.frame(seed=i, N=nsamp, cRD=cRD, cRR=cRR)
  res_vec <- bind_rows(res_vec, res)
}

res_vec
