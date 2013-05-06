#' @export spdur

##########
# What: spdur R package core functions
# Date: November 2012
# Who:  Andreas Beger, Daniel W. Hill, Nils Metternich
#
###########

spdur <- function(duration, atrisk, data=NULL, last="end.spell", t.0="t.0", distr='weibull', max.iter=300, ...) { 
  # Duration equation
  mf.dur <- model.frame(formula=duration, data=data)
  X <- model.matrix(attr(mf.dur, 'terms'), data=mf.dur)
  lhb <- model.response(mf.dur)
  # Risk/non-immunity equation
  mf.risk <- model.frame(formula=atrisk, data=data)
  Z <- model.matrix(attr(mf.risk, 'terms'), data=mf.risk)
  lhg <- model.response(mf.risk)
  # Y vectors
  Y <- cbind(atrisk=lhg, duration=lhb, last=data[, last], t.0=data[, t.0])
  
  # Estimation
  if (distr=='weibull') {
    est <- spweibull(Y, X, Z, max.iter)
  }
  if (distr=='loglog') {
    est <- sploglog(Y, X, Z, max.iter)
  }
  # Names
  varnames <- c(paste(unlist(attr(X, 'dimnames')[2])), paste(unlist(attr(Z, 'dimnames')[2])), 'log(alpha)')
  attr(est$coefficients, 'names') <- varnames
  colnames(est$vcv) <- rownames(est$vcv) <- varnames
  
  # Calculate uncertainty measures
  est$se <- sqrt(diag(est$vcv))
  est$zstat <- with(est, coefficients/se)
  est$pval <- 2*(1-pnorm(abs(est$zstat)))
  
  # Other class elements
  est$call <- expand.call() # instead of match.call to catch default parameters
  est$distr <- eval.parent(distr, 1)
  est$obs <- nrow(Y)
  
  class(est) <- 'spdur'
  return(est)
}