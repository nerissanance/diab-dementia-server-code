library(data.table)
library(tidyverse)
library(doParallel)

boot <- foreach(i=1:9,.combine=rbind) %do% {
  rio::import(paste0("./targets/dementia/export/boot_res",i,".txt"))

}
n <- 113000

#We now want to shrink it back to respect that sample size is n and not
#n_1~ 2/3 n.

boot <- data.table(boot)
boot1 <- boot[parameter=="ATE" &estimator=="tmle",]
hist(boot1$estimate)
#why bimodal? look at iterations over time
hist(boot1$estimate[1:120])
hist(boot1$estimate[1:1080])

summary(boot1$estimate)

(quants <- quantile(boot1$estimate,c(.025,.975)))

results <- data.table(rio::import(paste0("./targets/dementia/export/table_ltmle_main_results_dementia_2022-08-11_ic.txt")))
(Psihat <-results[estimator=="tmle"&parameter=="ATE"&analysis=="primary",])

#from mark:
#We do this by computing x_1=psi_n-q_l and x_2=q_2-psi_n.
(x_1 <-Psihat$estimate-quants[[1]])
(x_2 <- quants[[2]]-Psihat$estimate)
#Thus the interval can be represented as
#psi_n-x_1,psi_n+x_2.

(boot_lb <- Psihat$estimate-x_1)
(boot_ub <-Psihat$estimate+x_2)
Psihat
#you can it is better than IC (more narrow!)

#From mark: Now define new interval as: psi_n-x_1 sqrt(2/3 n), psi_n + x_2 sqrt(2/3 n)
#NN note: I think you need to additionally divide by n so you're accounting for
#this estimate already being a sample sd? like this:
(boot_adj_lb <- Psihat$estimate-(x_1*(sqrt((2/3)*n)/sqrt(n))))
(boot_adj_ub <- Psihat$estimate+(x_2*(sqrt((2/3)*n)/sqrt(n))))

#is there right, though? not sure.

# Easier to visualize:

ggdata <- data.frame(rbind(c("Empirical IC",
                       Psihat$estimate,Psihat$ci.lb,Psihat$ci.ub
), c("Bootstrap",Psihat$estimate,boot_lb,boot_ub

),c("Bootstrap rescaled for sample size",Psihat$estimate,boot_adj_lb,boot_adj_ub
    )))

names(ggdata) <- c("est_type","est","ci_lb","ci_ub")



ggplot2::ggplot(data=ggdata,aes(x=est_type,y=est,ymin=ci_lb,ymax=ci_ub))+
  geom_pointrange()+coord_flip()+geom_hline(yintercept = 0)

