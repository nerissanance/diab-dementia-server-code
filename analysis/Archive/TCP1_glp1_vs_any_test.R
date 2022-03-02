
rm(list=ls())
Sys.Date()
library(here)
source(here::here("0_config.R"))
source(paste0(here::here(),"/analysis/ltmle_Estimate_update.R"))

#-----------------------------------
#-----------------------------------

# NOTE: edit these hyperparameters or inputs as needed

yr = 2011 #year to start cohort definition (depending on which drugs)
N_time = 11 #number of time points you want to look at
N_time = 2 #number of time points you want to look at
ncores = 10 #number of cores to use

SL.library = c("SL.glmnet")
# SL.library = c("glm") #for debugging
nfolds = 10 #number of folds for CV SL
varmethod = "ic" #variance method

#-----------------------------------
#-----------------------------------



d_wide <- readRDS(file=here("data/data_clean.rds"))

# #subset to after yr
# #ATTN: commenting out for now, need to add secdate in later
# d_wide_sub <- d_wide %>% filter(secdate>=yr)

#Use only first N time points
d <- d_wide %>%
  dplyr::select(!!(baseline_vars),matches(paste0("_(",paste0(0:(N_time-1),collapse="|"),")$")))
colnames(d)


# NOTE: set up to run with just glmnet
#-----------------------------------
#Need to run this script to get glmnet implementation of ltmle, otherwise is CRAN version
#source(here::here("0_glmnet_specs.R"))


#set up parallelization on windows with the Snow package
options(snow.cores=ncores)


#-------------------------------------------------------------------------------
#  Contrasting if everyone had glp1 versus not
#-------------------------------------------------------------------------------

#specify LTMLE analysis
spec_ltmle <- spec_analysis(data=d, c(long_covariates,"event_death_"),
                            baseline_vars, N_time,
                            Avars=c("glp1_"),
                            Yvars=c("event_dementia_"))

#specify the intervened treatment
abar_spec = list(rep(1,N_time),rep(0,N_time))

dsub <- spec_ltmle$data %>% select(spec_ltmle$Anodes,spec_ltmle$Ynodes)
dsub <- data.frame(dsub)



package_stub("SuperLearner", "SuperLearner", SuperLearner_override, {
  testthatsomemore::package_stub("ltmle", "Estimate", Estimate_override, {
    res_RR <- ltmle(
                    data=spec_ltmle$data,
                    #data=dsub,
                    Anodes = spec_ltmle$Anodes,
                    Cnodes = spec_ltmle$Cnodes,
                    Lnodes = spec_ltmle$Lnodes,
                    Ynodes = spec_ltmle$Ynodes,
                    survivalOutcome = T,
                    abar = abar_spec,
                    deterministic.Q.function = det.Q.function,
                    #SL.library = "glm"#,
                    SL.library = SL.library,
                    SL.cvControl = list(V=nfolds),
                    variance.method = varmethod #use tmle variance option for accuracy with positivity violations
    )})})


summary(res_RR)



det.Q.function




cv.glmnet()

Q.kplus1 ~ ie_type + age_base + sex + code5txt + quartile_income + glp1_0 + insulin_0 + any.malignancy_0 + chronic.pulmonary.disease_0 + hypertension_0 + myocardial.infarction_0 + ischemic.heart.disease_0 + heart.failure_0 + renal.disease_0 + event_death_0 + glp1_1



df <- spec_ltmle$data %>% subset(., select = c("ie_type","age_base","sex","code5txt","quartile_income","glp1_0","insulin_0","any.malignancy_0","chronic.pulmonary.disease_0","hypertension_0","myocardial.infarction_0","ischemic.heart.disease_0","heart.failure_0","renal.disease_0","event_death_0","glp1_1"))
head(df)

library(caret)

zeroVar

nearZeroVar(df)
nearZeroVar(df,freqCut = 100/0)
df[,nearZeroVar(df)]
df[,c(11,12,13,14,15)]
table(df[,11])
table(df[,12])
table(df[,13])
table(df[,14])
table(df[,15])

cv.glmnet(x=as.matrix(df),y=rnorm(nrow(df)))

cv.glmnet(x=as.matrix(df),y=rep(0,nrow(df)), family="binomial")



glmnet(x=as.matrix(df),y=rnorm(nrow(df)))

glmnet(x=as.matrix(df),y=factor(rep(c(0,1),nrow(df)/2)), family="binomial")





# Gaussian
x = matrix(rnorm(100 * 20), 100, 20)

# binomial
g2 = sample(c(0,1), 100, replace = TRUE)
g2 = sample(c(0), 100, replace = TRUE)
g2[1] <-1
fit2 = cv.glmnet(x, g2, family = "binomial")
fit2 = glmnet(x, g2, family = "binomial")

#fit2 = glm(formula= y~. ,x=x, y=g2, family = "binomial")
fit2 = glm(formula= y~. , data.frame(y=g2,x), family = "binomial")
predict(fit2, type="response")

