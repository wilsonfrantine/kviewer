#-Description----
# Estimators calculation
#  This file intends to hold functions to control
#  the flux of calculations using data from read.structure
#  and formulas.R maths
#-Functions----

#' @name calc.estimators
#' @title Calculating best K estimators for Evanno's and Pritchard's methods
#' @description this function grab data from read.structure
#' and returns a table with estimators calculated
#' @param x a table returned from read.structure function
#' @export
calc.estimators <- function(x){
  nK  <- nlevels( as.factor(x$K) )
  out <- data.frame()
#Evanno calculation
  for( i in 1:nK ){
    lk       <- x[x$K==i,"LnP.of.data"]
    lkplus1  <- x[x$K==i+1,"LnP.of.data"]
    lkminus1 <- x[x$K==i-1,"LnP.of.data"]

    L1K <- lk - lkminus1
    L2K <- I(lkplus1 - lk) - L1K
    dK  <- mean(L2K)/sd(lk)

    out <- rbind(out,
                 data.frame(
                   "K"      = i,
                   "Reps"   = nrow(x[x$K==i,]),
                   "meanLk" = mean(lk),
                   "sdLk"   = sd(lk),
                   "L1K"    = mean(L1K),
                   "L2K"    = sqrt(mean(L2K)^2),
                   "DeltaK" = sqrt(dK^2)
                   )
                 )
  }

#Calculating Pritchard's 2003 evidence of K (PK) according to Structure's Manual
  PK<-c()
  for(eachlk in out$meanLk){
    PK<-c(PK, exp(1)^-1/sum(exp(1)^(out$meanLk - eachlk -1)) )
  }
  out <- cbind(out,"pK"=PK)
  out$K <- as.factor(out$K)
  return(out)

}
