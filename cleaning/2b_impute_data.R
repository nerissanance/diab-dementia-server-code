# Created: 2021-10-12
# Author(s): 
# Input files: long data from file 2a and base_dem.rds from file 1
# Output files: A longitudinal data.table with covariates and datasets 
#                prepared  for LTMLE and covariate file, all with full data (no missings)
#NOTE: currently doing something SUPER simple --this is just a placeholder
#for something fancier later on.


#########
####time-constant DATA
#########

base_dem <- readRDS(file=here::here("data/base_dem.rds"))

names(base_dem)
#look at percent missing
apply(base_dem,2,function(x) prop.table(table(is.na(x))))
#about 40% missing in the biomarkers, but we are not surprised by that

# I don't really see how these variables are relevant?'
# *  last_ind                 : last immigration into DK
# *  first_ud                 : first immigration out of DK
# *  last_ud                  : last immigration out of DK
# * year (not sure what this is? year of what?)

#we also don't need dates of death in this dataset (that's covered in the long data)





#impute the rest of them that have missings to the mode for now

# *  opr_land                 : Country of origin
# * quartile_income 
# * code5txt--not sure what this is, there is no explanation in the readme
# * ie_type--has some hidden missing
class(base_dem$opr_land)

get_mode <- function(x){
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x,uniqv)))]
  
}
base_dem$opr_land[base_dem$opr_land=="."|is.na(base_dem$opr_land)] <- get_mode(base_dem$opr_land)
table(base_dem$opr_land)
table(is.na(base_dem$opr_land))
base_dem[is.na(quartile_income),quartile_income:=get_mode(quartile_income)]
table((base_dem$quartile_income))
base_dem[is.na(code5txt),code5txt:=get_mode(code5txt)]
table(base_dem$code5txt)
base_dem$ie_type[base_dem$ie_type=="."|is.na(base_dem$ie_type)] <- 
  get_mode(base_dem$ie_type)


saveRDS(base_dem,file=here::here("data/base_dem_imputed.rds"))
apply(base_dem,2,function(x) prop.table(table(is.na(x))))
table(base_dem$opr_land)
# summary(base_dem$age_base)
# table(base_dem$sex)
# table(base_dem$code5txt)
# table(base_dem$ie_type)
# table(base_dem$quartile_income)



#here we want to impute to the mode again but within each subject
apply(time_dem_wide,2,function(x) prop.table(table(is.na(x))))






