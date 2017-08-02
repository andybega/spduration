#' Split-population Weibull regression
#' 
#' @param Y Dependent variables. 
#' @param X Duration equation covariates.
#' @param Z Risk equation covariates.
#' @param max.iter Maximum iterations to try.
#' @param silent Suppress \code{\link{optim}} trace output?
#' 
#' @keywords internal
spweibull <- function(Y, X, Z, max.iter, silent=FALSE) {  
  # Estimate base model
  if (!exists("base.inits")) {
    base.inits <- c(rep(0, ncol(X)), 0)
  }
  if (!silent) cat('Fitting base weibull...\n')
  base <- weibull(Y=Y, X=X, inits=base.inits, max.iter=200, silent=TRUE)
  
  # Estimate full model
  x.inits <- base$coefficients[1:ncol(X)]
  a.init <- base$coefficients[ncol(X)+1]
  if (!silent) cat('Fitting split weibull...\n')
  trace <- !silent
  est <- stats::optim(c(x.inits, rep(0, ncol(Z)), a.init), spweib_lnl, method="BFGS", 
    control=list(trace=trace, maxit=max.iter), hessian=T, y=Y, X=X, Z=Z)
  
  # Solve other results
  if (est$convergence!=0 & !silent) stop('Model did not converge')
  coef <- est$par
  vcv <- solve(est$hessian)
  vcv <- corpcor::make.positive.definite(vcv)
  logL <- -est$value
  
  # Put together results
  return(list(coefficients = coef, vcv = vcv, logL = logL, base=base))
}

#' Regular Weibull regression
#' 
#' @param Y Dependent variables. 
#' @param X Duration equation covariates.
#' @param inits Vector of starting values. 
#' @param max.iter Maximum iterations to try.
#' @param silent Suppress \code{\link{optim}} trace output?
#' 
#' @keywords internal
weibull <- function(Y, X, inits=NULL, max.iter, silent=TRUE) {
  if (is.null(inits)) {
    inits <- c(rep(0, ncol(X)), 0)
  }
  
  trace <- !silent
  est <- stats::optim(inits, weib_lnl, method="BFGS", 
               control=list(trace=trace, maxit=max.iter), 
               hessian=T, y=Y, X=X)
  
  # Solve other results
  if (est$convergence!=0 & !silent) stop('Model did not converge')
  coef <- est$par
  vcv <- solve(est$hessian)
  vcv <- corpcor::make.positive.definite(vcv)
  logL <- -est$value
  
  # Put together results
  return(list(coefficients = coef, vcv = vcv, logL = logL))
}

