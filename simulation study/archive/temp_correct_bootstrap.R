
rm(list=ls())
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))
source(paste0(here::here(),"/simulation study/0_simulation_cleaning_functions.R"))


#need to load the IC point estimates
files <- dir(path=paste0(here::here(),"/sim_res/"), pattern = "*.RDS")
files <- files[grepl("outcome_blind_",files)]
files <- files[grepl("_T4",files)]

point_estimates <- readRDS(paste0(here::here(),"/sim_res/outcome_blind_cens_competing_risks_sim_res_ic_T4.RDS")) %>% select(estimate) %>% mutate(iteration = 1:n())

#---------------------------------------------------------
# Outcome blind, T4
#---------------------------------------------------------

boot_iter_files <- dir(path=paste0(here::here(),"/data/bootstrap/"), pattern = "*.RDS")
boot_iter_files <- boot_iter_files[grepl("_T4",boot_iter_files)]
boot_iter_files <- boot_iter_files[!grepl("_null",boot_iter_files)]
boot_iter_files_no_ties <- boot_iter_files[grepl("_no_ties",boot_iter_files)]
length(boot_iter_files_no_ties)




boot_iter_files <-bind_rows(
 data.frame(boot_file=boot_iter_files_no_ties, analysis="outcome_blind_bootstrap_no_ties_cens_competing_risks")
)

boot_iter_files$boot_iter <- as.character(1:nrow(boot_iter_files))
setwd(paste0(here::here(),"/data/bootstrap/"))
boot_res <- boot_iter_files$boot_file %>% map(readRDS) %>% map_dfr(~bind_rows(.) , .id="boot_iter")
boot_res <- left_join(boot_res, boot_iter_files, by=c("boot_iter"))
boot_res <- boot_res %>% subset(., select = c(iteration, estimate, std.dev, CI.2.5., CI.97.5.)) %>% rename(boot_est=estimate   )
boot_res$N <- 70000
# load(paste0(here::here(),"/results/truth_blind_T4.Rdata"))
# sim_res_ob_T4 <- calc_sim_performance(files, boot_iter_files, 0.4493307, cRD3)


d<-left_join(point_estimates, boot_res, by="iteration")
head(d)

#correct bootstrap

#from mark:
#We do this by computing x_1=psi_n-q_l and x_2=q_2-psi_n.
d$x_1 <-d$estimate-d$CI.2.5.
d$x_2 <- d$CI.97.5.-d$estimate
head(d)

#Thus the interval can be represented as
#psi_n-x_1,psi_n+x_2.
d$boot_lb <- d$estimate - d$x_1
d$boot_ub <- d$estimate + d$x_2
head(d)

#you can it is better than IC (more narrow!)

#From mark: Now define new interval as: psi_n-x_1 sqrt(2/3 n), psi_n + x_2 sqrt(2/3 n)
#NN note: I think you need to additionally divide by n so you're accounting for
#this estimate already being a sample sd? like this:
d$boot_adj_lb <- d$estimate-(d$x_1*(sqrt((2/3)*d$N)/sqrt(d$N)))
d$boot_adj_ub <- d$estimate+(d$x_2*(sqrt((2/3)*d$N)/sqrt(d$N)))

head(d)
