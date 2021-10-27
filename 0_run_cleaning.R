

source(here::here("0_config.R"))

filepath <- here::here()
### CLEAN DATA AND TABULATE REGIMES:
#source("./1_data_cleaning/1_prepare_dataset.R")#this takes a long time and sets up general cohort, uncomment only as necessary
source(paste0(filepath,"/1_data_cleaning/2a_subset_and_transform_for_TMLE.R"))
# source("./1_data_cleaning/2b_impute_data.R")
source("./1_data_cleaning/3_transform_long_to_wide.R")
# source("./1_data_cleaning/4_tabulate_data.R")
# source("./1_data_cleaning/5_tabulate_regimes.R")


