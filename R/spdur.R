##########
# What: spdur R package functions
# Date: November 2012
# Who:  Daniel W. Hill, Nils Metternich, Andreas Beger
#
###########

# Input validation
#if (is.null(data)) stop("No data provided")
#if (is.null(last)) stop("Must specify censoring variable")
#if (distr == "") stop("Must specify distribution")

spdur <- function(x, ...) UseMethod('spdur')

spdur.default <- function(Y, X, Z, distr, max.iter=100)
{
  if (distr=='weibull') {
    est <- spweibull(Y, X, Z, max.iter)
  }
  if (distr=='loglog') {
    est <- sploglog(Y, X, Z, max.iter)
  }
  
  # Calculate uncertainty measures
  est$se <- sqrt(diag(est$vcv))
  est$zstat <- with(est, coefficients/se)
  est$pval <- 2*(1-pnorm(abs(est$zstat)))
  
  est$call <- match.call()
  class(est) <- 'spdur'
  return(est)
}

spdur.formula <- function(duration, atrisk, data=list(), last, distr='weibull', 
                          max.iter=100)
{
  # Duration equation
  mf.dur <- model.frame(formula=duration, data=data)
  X <- model.matrix(attr(mf.dur, 'terms'), data=mf.dur)
  lhb <- model.response(mf.dur)
  
  # Risk/non-immunity equation
  mf.risk <- model.frame(formula=atrisk, data=data)
  Z <- model.matrix(attr(mf.risk, 'terms'), data=mf.risk)
  lhg <- model.response(mf.risk)
  
  # Y vectors
  censor <- last
  Y <- cbind(atrisk=lhg, duration=lhb, last=censor)
  
  # Call default method for estimation
  est <- spdur.default(Y, X, Z, distr, max.iter)
  est$call <- match.call()
  est$duration.formula <- duration
  est$atrisk.formula <- atrisk
  
  return(est)
}

##########
# spdur.icews
# Top level wrapper
#
# in:  
#
# to do:
# - clean up code
#
###########
spdur.icews <- function (formula, cure = formula2, data = NULL, test = NULL, 
                         last = NULL, distr = "", re = "", iter = NULL, sims = NULL) 
{
  if (is.null(data)) 
    stop("No data provided")
  if (is.null(last)) 
    stop("Must specify censoring variable")
  if (distr == "") 
    stop("Must specify distribution")
  a <- as.character(formula)
  b <- as.character(cure)
  lhb <- a[2]
  rhb <- strsplit(a[3], split = " + ", fixed = T)[[1]]
  lhg <- b[2]
  rhg <- strsplit(b[3], split = " + ", fixed = T)[[1]]
  X <- data[rhb]
  Z <- data[rhg]
  Y <- cbind(data[lhg], data[lhb], last)
  X.test <- test[rhb]
  Z.test <- test[rhg]
  Y.test <- cbind(test[lhg], test[lhb])
  if (is.null(iter)) 
    iter <- 100
  if (is.null(sims)) 
    sims <- 1000
  if (distr == "weibull") {
    model <- spweibull(Y, X, Z, Y.test, X.test, Z.test, iter, 
                       sims)
  }
  if (distr == "loglog") {
    model <- sploglog(Y, X, Z, Y.test, X.test, Z.test, iter, 
                      sims)
  }
  
  # Format and show estimates
  rownames(model$results) <- c('duration const', rhb, 'risk constant', rhg, 'alpha')
  print(model$results, digits=4)
  
  invisible((model))
}