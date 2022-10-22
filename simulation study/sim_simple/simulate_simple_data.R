


generate_data3 = function(n) {

  # exogenous variables
  for(i in 1:2){
    assign(paste0("U.Lt",i), rnorm(n, mean=0, sd=1))
    assign(paste0("U.At",i), runif(n,0,1))
    assign(paste0("U.Yt",i+1), runif(n,0,1))

  }

  # endogenous variables
  L1 = U.Lt1
  A1 = as.numeric( U.At1 < plogis(0.001*L1))
  Y2 = as.numeric(U.Yt2 < plogis(L1-(2*A1)-1))

  #L2 = ifelse(Y2==0, A1 + L1 + U.Lt2, NA) # Note this adds censoring NA's
  L2 =  A1 + L1 + U.Lt2
  A2 = ifelse(Y2==0, as.numeric(U.At2 < plogis(0.001*(L1+A1+L2))),0)
  Y3 = ifelse(Y2==0, as.numeric(U.Yt3 < plogis((L1-A1+L2-A2)/2)), 1)
  # dataframe with endogenous variables
  O = data.frame(L1,A1,Y2,L2,A2,Y3)

  return(O)
}

set.seed(252)

n=1000
simp_sim_data_list <- list()

for(i in 1:100){
  set.seed(i*12)
  simp_sim_data_list[[i]] <- generate_data3(n)
}

saveRDS(simp_sim_data_list, paste0(here::here(),"/data/simulated_data_list_simple.RDS"))


d <- simp_sim_data_list[[1]]

#check data
head(d)
table(d$A1)
table(d$Y2)
table(d$A2)
table(d$Y3)
table(d$A1, d$A2)
table(d$Y2==1, d$Y3)


#Calc truth
generate_counterfactual_data = function(n, A) {
  for(i in 1:2){
    assign(paste0("U.Lt",i), rnorm(n, mean=0, sd=1))
    assign(paste0("U.At",i), runif(n,0,1))
    assign(paste0("U.Yt",i+1), runif(n,0,1))
  }
  L1 = U.Lt1
  A1 = A
  Y2 = as.numeric(U.Yt2 < plogis(L1-(2*A1)-1))
  L2 =  A1 + L1 + U.Lt2
  A2 = A
  Y3 = ifelse(Y2==0, as.numeric(U.Yt3 < plogis((L1-A1+L2-A2)/2)), 1)
  O = data.frame(L1,A1,Y2,L2,A2,Y3)
  return(O)
}

#NOTE: need to remove censoring from simulation

df_A1 <- generate_counterfactual_data(1000000, A=1)
df_A0 <- generate_counterfactual_data(1000000, A=0)

simple_sim_trueRR <- prop.table(table(df_A1$Y3))[2]/prop.table(table(df_A0$Y3))[2]
simple_sim_trueRD <- prop.table(table(df_A1$Y3))[2]-prop.table(table(df_A0$Y3))[2]










