
library(ltmle)

rexpit <- function(x) rbinom(n=length(x), size=1, prob=plogis(x))

# Single time point Example
set.seed(234)
n <- 1000
W <- rnorm(n)
A1 <- rexpit(-1 + 2 * W)
A2 <- rexpit(-1 + 2 * W)
Y1 <- rexpit(W + A1 + rnorm(n))
Y2 <- rexpit(W + A2 + rnorm(n))
Y2[Y1==1] <-1
L1 <- rexpit(W+ rnorm(n))
L2 <- c(1, rep(0,999))
data <- data.frame(W, A1, L1, Y1, A2, L2, Y2)

result1 <- ltmle(data, Anodes=c("A1","A2"), Ynodes=c("Y1","Y2"), Lnodes=c("L1","L2"),
                 survivalOutcome=T,  abar=list(c(1,1),c(0,0)), SL.library=c("SL.glm","SL.glmnet"), variance.method="tmle")
summary(result1)


data <- data.frame(W, A1, Y1, A2, Y2)

result1 <- ltmle(data, Anodes=c("A1","A2"), Ynodes=c("Y1","Y2"), #Lnodes=c("L1","L2"),
                 survivalOutcome=T,  abar=list(c(1,1),c(0,0)), SL.library=c("SL.glmnet"), variance.method="tmle")
summary(result1)



# try(fit <- ltmle(data=spec_ltmle$data,
#                  Anodes = spec_ltmle$Anodes,
#                  Cnodes = spec_ltmle$Cnodes,
#                  Lnodes = spec_ltmle$Lnodes,
#                  Ynodes = spec_ltmle$Ynodes,
#                  gbound=gbound,
#                  survivalOutcome = T,
#                  abar = abar_spec,
#                  gcomp=gcomp,
#                  Qform = qform,
#                  estimate.time=T,
#                  deterministic.Q.function = det.q.fun,
#                  SL.library = SL.library,
#                  variance.method = varmethod
# ))
