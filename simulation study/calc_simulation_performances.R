


rm(list=ls())
source(here::here("0_config.R"))
source(paste0(here::here(),"/0_ltmle_Estimate_update.R"))
source(paste0(here::here(),"/simulation study/0_simulation_functions.R"))
source(paste0(here::here(),"/simulation study/0_simulation_cleaning_functions.R"))


#---------------------------------------------------------
# Null, T4
#---------------------------------------------------------
files <- dir(path=paste0(here::here(),"/sim_res/"), pattern = "*.RDS")
files <- files[grepl("null_",files)]
files <- files[grepl("_T4",files)]

setwd(paste0(here::here(),"/sim_res/"))
d <- files %>% map(readRDS) %>% map_dfr(~bind_rows(.) , .id="analysis")
d <- d %>% mutate(analysis = factor(analysis))
levels(d$analysis) = files[as.numeric(levels(d$analysis))]
d$analysis <- gsub(".RDS","",d$analysis)

#load bootstrap
boot_iter_files <- dir(path=paste0(here::here(),"/data/bootstrap/"), pattern = "*.RDS")
boot_iter_files <- boot_iter_files[grepl("sim_res_boot_null_4_",boot_iter_files)]
length(boot_iter_files)

sim_res_null_T4 <- calc_sim_performance(files, boot_iter_files, 1, 0)
tab<-sim_res_null_T4$perf_tab_RR
tab

#---------------------------------------------------------
# Null, T11
#---------------------------------------------------------
files <- dir(path=paste0(here::here(),"/sim_res/"), pattern = "*.RDS")
files <- files[grepl("null_",files)]
files <- files[grepl("_T11",files)]

setwd(paste0(here::here(),"/sim_res/"))
d <- files %>% map(readRDS) %>% map_dfr(~bind_rows(.) , .id="analysis")
d <- d %>% mutate(analysis = factor(analysis))
levels(d$analysis) = files[as.numeric(levels(d$analysis))]
d$analysis <- gsub(".RDS","",d$analysis)

df <- d %>% filter(analysis=="null_no_cens_sim_res_noDetQ_Qint_ic_T11")
head(df)

# #load bootstrap
# boot_iter_files <- dir(path=paste0(here::here(),"/data/bootstrap/"), pattern = "*.RDS")
# boot_iter_files <- boot_iter_files[grepl("sim_res_boot_null_11_",boot_iter_files)]
# length(boot_iter_files)

sim_res_null_T11 <- calc_sim_performance(files, boot_iter_files=NULL, 1, 0)


#---------------------------------------------------------
# Outcome blind, T4
#---------------------------------------------------------

files <- dir(path=paste0(here::here(),"/sim_res/"), pattern = "*.RDS")
files <- files[grepl("outcome_blind_",files)]
files <- files[grepl("_T4",files)]

boot_iter_files <- dir(path=paste0(here::here(),"/data/bootstrap/"), pattern = "*.RDS")
boot_iter_files <- boot_iter_files[grepl("_T4",boot_iter_files)]
boot_iter_files <- boot_iter_files[!grepl("_null",boot_iter_files)]


boot_iter_files_no_ties <- boot_iter_files[grepl("_no_ties",boot_iter_files)]
length(boot_iter_files_no_ties)
boot_iter_files <- boot_iter_files[!grepl("_no_ties",boot_iter_files)]
length(boot_iter_files)

boot_iter_files_no_cens <- boot_iter_files[!grepl("_cens_competing",boot_iter_files)]
length(boot_iter_files_no_cens)

boot_iter_files_cens <- boot_iter_files[grepl("_cens_competing",boot_iter_files)]
length(boot_iter_files_cens)
boot_iter_files_cens_no_detQ <- boot_iter_files_cens[grepl("noDetQ",boot_iter_files_cens)]
boot_iter_files_cens <- boot_iter_files_cens[!grepl("noDetQ",boot_iter_files_cens)]
length(boot_iter_files_cens_no_detQ)
length(boot_iter_files_cens)

load(paste0(here::here(),"/results/truth_blind_T4.Rdata"))
trueRR=cRR3
trueRD=cRD3


boot_iter_files <-bind_rows(
  data.frame(boot_file=boot_iter_files_cens, analysis="outcome_blind_bootstrap_cens_competing_risks_"),
  data.frame(boot_file=boot_iter_files_cens_no_detQ, analysis="outcome_blind_bootstrap_cens_competing_risks_noDetQ_"),
  data.frame(boot_file=boot_iter_files_no_ties, analysis="outcome_blind_bootstrap_no_ties_cens_competing_risks")
)


load(paste0(here::here(),"/results/truth_blind_T4.Rdata"))
sim_res_ob_T4 <- calc_sim_performance(files, boot_iter_files, 0.4493307, cRD3)



#---------------------------------------------------------
# Outcome blind, T11
#---------------------------------------------------------

files <- dir(path=paste0(here::here(),"/sim_res/"), pattern = "*.RDS")
files <- files[grepl("outcome_blind_",files)]
files <- files[grepl("_T11",files)]

boot_iter_files <- dir(path=paste0(here::here(),"/data/bootstrap/"), pattern = "*.RDS")
boot_iter_files <- boot_iter_files[grepl("_T11",boot_iter_files)]
boot_iter_files <- boot_iter_files[!grepl("_null",boot_iter_files)]
boot_iter_files <- boot_iter_files[!grepl("_subsampled",boot_iter_files)]
boot_iter_files <- boot_iter_files[!grepl("_outcome_blind",boot_iter_files)]
length(boot_iter_files)

load(paste0(here::here(),"/results/truth_blind_T10.Rdata"))

sim_res_ob_T11 <- calc_sim_performance(files, boot_iter_files, 0.4398625, cRD)

save(sim_res_null_T4, sim_res_null_T11, sim_res_ob_T4, sim_res_ob_T11, file=paste0(here::here(),"/results/sim_performance_results.Rdata"))


