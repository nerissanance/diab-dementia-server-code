### synthesizeDD.R ---
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Nov  3 2021 (15:20)
## Version:
## Last-Updated: Nov  5 2021 (17:47)
##           By: Thomas Alexander Gerds
##     Update #: 6
#----------------------------------------------------------------------
##
### Commentary:
##
### Change Log:
#----------------------------------------------------------------------
##
### Code:
##' Synthesizing longitudinal diabetes dementia followup data
##'
##' A sequence of logistic regression models Danmark Statistics
##' @title Synthesizing longitudinal diabetes dementia followup data
##' @param coefficients Intercepts and regression coefficients (log-odds-ratios)
##' @return \code{lvm} object for simulation
##' @seealso \code{lvm}, \code{distribution}, \code{regression}, \code{sim}
##' @examples
##' library(lava)
##' library(data.table)
##' cc <- fread("data/coefficients.txt")
##' u <- synthesizeDD(cc)
##' d <- sim(u,1000)
##' @export
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
synthesizeDD <- function(coefficients){
    requireNamespace("lava")
    coefficients <- data.table(coefficients)
    XNAMES <- names(coefficients)[-(1:3)]
    BETA <- coefficients[,-(1:3),with=0L]
    INTERCEPT <- coefficients[["(Intercept)"]]
    # empty lava model for simulation
    m <- lvm()
    distribution(m,"age_base") <- normal.lvm(mean=70,sd=10)
    distribution(m,"sex") <- binomial.lvm(p=0.4)
    m <- addvar(m,"ie_type")
    m <- addvar(m,"code5txt")
    m <- addvar(m,"quartile_income")
    # loop across time and variables
    for (j in 1:NROW(coefficients)){
        V <- coefficients$var[j]
        beta <- unlist(BETA[j,])
        X <- XNAMES[!is.na(beta)]
        beta <- beta[!is.na(beta)]
        # add V ~ Intercept + beta X
        distribution(m,V) <- binomial.lvm()
        intercept(m,V) <- INTERCEPT[j]
        regression(m,from=X,to=V) <- beta
    }
    class(m) <- c("synthesizeDD",class(m))
    m
}
sim.synthesizeDD <- function(x,...){
    class(x) <- class(x)[-1]
    d <- data.table(sim(x,...))
    v <- names(d)
    ttt <- sapply(strsplit(grep("death",v,value=TRUE),"event_death_"),"[",2)
    ## HERE
    ## for (t in ttt) set(x=d,j=paste0("fup_",t),value=d[[paste("event_death_",t)]]+d[[paste("event_dementia_",t)]]+d[[paste("censor_",t)]])
    #d <- apply(d,1,function(i){    })
    #fup <- data.table()
    #d[,fup:={browser();apply(.SD,1,function(x){browser()})},.SDcols=grep("event|censor|death",names(d))]
    d
}


synthesizeDD.always <- function(coefficients, A_name = "glp1"){
    requireNamespace("lava")
    coefficients <- data.table(coefficients)
    XNAMES <- names(coefficients)[-(1:3)]
    BETA <- coefficients[,-(1:3),with=0L]
    # collect At and Ct nodes; intervene At=1 and Ct=0 later
    loc_A <- grep(paste0("^", A_name, "_"), XNAMES)
    beta_A <- BETA[, loc_A, with = F]
    loc_C <- grep("^censor_", XNAMES)
    beta_C <- BETA[, loc_C, with = F]


    INTERCEPT <- coefficients[["(Intercept)"]]
    # empty lava model for simulation
    m <- lvm()
    distribution(m,"age_base") <- normal.lvm(mean=70,sd=10)
    distribution(m,"sex") <- binomial.lvm(p=0.4)
    m <- addvar(m,"ie_type")
    m <- addvar(m,"code5txt")
    m <- addvar(m,"quartile_income")
    # loop across time and variables
    for (j in 1:NROW(coefficients)){
        # At constant 1 -> intercept becomes intercept + At coefficient
        # also remove At from fitted betas
        # Ct constant 0 -> intercept no change, remove Ct from fitted betas
        temp_intercept <- INTERCEPT
        temp_sum_A_coef <- rowSums(beta_A, na.rm = T)  # intercept + At coefficients
        temp_intercept <- temp_intercept + temp_sum_A_coef

        V <- coefficients$var[j]
        beta <- unlist(BETA[j,])
        beta[loc_A] <- NA  # absorb A coefficient into intercept for always-on group; not depending on observed A values any more
        beta[loc_C] <- NA  # C is also intervened so will be ignored in conditional logistic models

        X <- XNAMES[!is.na(beta)]
        beta <- beta[!is.na(beta)]
        # add V ~ Intercept + beta X
        distribution(m,V) <- binomial.lvm()
        # intercept(m,V) <- INTERCEPT[j]
        intercept(m,V) <- temp_intercept[j]
        regression(m,from=X,to=V) <- beta
    }
    class(m) <- c("synthesizeDD",class(m))
    m
}


# example
library(lava)
library(data.table)
cc <- fread("data/coefficients.txt")

set.seed(123)
u <- synthesizeDD(cc)
d <- sim(u,10^6)
if_always_on <- d[, grep("glp1_", names(d)), with=F] %>% apply(1, function(u) all(u==1))
d[if_always_on, ]$event_dementia_12 %>% table


synthesizeDD.never <- function(coefficients, A_name = "glp1"){
    requireNamespace("lava")
    coefficients <- data.table(coefficients)
    XNAMES <- names(coefficients)[-(1:3)]
    BETA <- coefficients[,-(1:3),with=0L]
    # collect At and Ct nodes; intervene At=1 and Ct=0 later
    loc_A <- grep(paste0("^", A_name, "_"), XNAMES)
    beta_A <- BETA[, loc_A, with = F]
    loc_C <- grep("^censor_", XNAMES)
    beta_C <- BETA[, loc_C, with = F]


    INTERCEPT <- coefficients[["(Intercept)"]]
    # empty lava model for simulation
    m <- lvm()
    distribution(m,"age_base") <- normal.lvm(mean=70,sd=10)
    distribution(m,"sex") <- binomial.lvm(p=0.4)
    m <- addvar(m,"ie_type")
    m <- addvar(m,"code5txt")
    m <- addvar(m,"quartile_income")
    # loop across time and variables
    for (j in 1:NROW(coefficients)){
        # At constant 1 -> intercept becomes intercept + At coefficient
        # also remove At from fitted betas
        # Ct constant 0 -> intercept no change, remove Ct from fitted betas
        temp_intercept <- INTERCEPT
        temp_sum_A_coef <- rowSums(beta_A, na.rm = T)  # intercept + At coefficients
        temp_intercept <- temp_intercept + temp_sum_A_coef

        V <- coefficients$var[j]
        beta <- unlist(BETA[j,])
        beta[loc_A] <- NA  # absorb A coefficient into intercept for always-on group; not depending on observed A values any more
        beta[loc_C] <- NA  # C is also intervened so will be ignored in conditional logistic models

        X <- XNAMES[!is.na(beta)]
        beta <- beta[!is.na(beta)]
        # add V ~ Intercept + beta X
        distribution(m,V) <- binomial.lvm()
        intercept(m,V) <- INTERCEPT[j]#keep only intercept for "never on"
        # intercept(m,V) <- temp_intercept[j]
        regression(m,from=X,to=V) <- beta
    }
    class(m) <- c("synthesizeDD",class(m))
    m
}


# TESTING:

library(lava)
library(data.table)
library(tidyverse)
cc <- fread("../powerhouse/data/coefficients.txt")

gc()
nsamp <- 17000
#truth: 0.3600823
#nsamp <- 100000
#RR=
nsamp <- 10000000
#RR=0.392377


set.seed(1234)
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


set.seed(1234)
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

