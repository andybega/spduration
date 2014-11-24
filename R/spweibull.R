#' Split-population Weibull regression
#' 
#' @importFrom corpcor make.positive.definite
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
  est <- optim(c(x.inits, rep(0, ncol(Z)), a.init), spweib_lnl, method="BFGS", 
    control=list(trace=trace, maxit=max.iter), hessian=T, y=Y, X=X, Z=Z)
  
  # Solve other results
  if (est$convergence!=0 & !silent) stop('Model did not converge')
  coef <- est$par
  vcv <- solve(est$hessian)
  vcv <- make.positive.definite(vcv)
  logL <- -est$value
  
  # Put together results
  return(list(coefficients = coef, vcv = vcv, logL = logL, base=base))
}

#' Regular Weibull regression
weibull <- function(Y, X, inits=NULL, max.iter, silent=TRUE) {
  if (is.null(inits)) {
    inits <- c(rep(0, ncol(X)), 0)
  }
  
  trace <- !silent
  est <- optim(inits, weib_lnl, method="BFGS", 
               control=list(trace=trace, maxit=max.iter), 
               hessian=T, y=Y, X=X)
  
  # Solve other results
  if (est$convergence!=0 & !silent) stop('Model did not converge')
  coef <- est$par
  vcv <- solve(est$hessian)
  vcv <- make.positive.definite(vcv)
  logL <- -est$value
  
  # Put together results
  return(list(coefficients = coef, vcv = vcv, logL = logL))
}

#' Regular weibull log likelihood
weib_lnl <- function(theta, y, X) {
  # Parameters
  beta   <- theta[1:ncol(X)]
  lambda <- exp(-X %*% beta)
  p      <- theta[ncol(X) + 1]
  alpha  <- exp(-p)
  
  # Dependent variables
  ti <- y[,2]    # duration at current period
  t0 <- y[,4]    # duration at previous period
  rc <- y[,1]    # right-censored spell (1 if uncensored)
  lo <- y[,3]    # last observation in spell
  
  # Weibull failure rate and survivor functions
  #ln.ft  <- log(alpha * lambda) + (alpha-1) * log(lambda * ti) - (lambda * ti)^alpha
  ln.ft <- log(alpha) + (alpha) * log(lambda) + (alpha - 1) * log(ti) - (lambda * ti)^alpha
  ln.st  <- -(lambda * ti)^alpha  # S(t) for current period
  ln.st0 <- -(lambda * t0)^alpha  # S(t) for previous period
  
  # Likelihood contributions
  cens   <- ifelse((rc==0) | (lo==0), ln.st - ln.st0, 0)  # censored or not failure
  nocens <- ifelse((rc==1) & (lo==1), ln.ft - ln.st0, 0)  # failure event
  
  logl <- sum(cens+nocens)
  return(-logl)
}

#' Split-population Weibull log likelihood
spweib_lnl <- function(theta, y, X, Z) {
  # Parameters
  rx <- ncol(X)
  rz <- ncol(Z)
  beta <- theta[1:rx]
  gamma <- theta[(rx+1):(rx+rz)]
  p <- theta[rx+rz+1]
  lambda <- exp(-X%*%beta)
  alpha <- exp(-p)
  
  # Dependent variables
  rc <- y[,1]    # right-censored spell (1 if uncensored)
  ti <- y[,2]
  t0 <- y[,4]    # duration at previous period
  lo <- y[,3]    # last observation in spell

  # Logistic for population split (1 = at risk)
  pr1 <- plogis(Z%*%gamma)
  pr0 <- plogis(Z%*%gamma, lower.tail=FALSE)
  
  # Weibull 
  #ln.ft <- log(alpha * lambda) + (alpha-1) * log(lambda * ti) - (lambda * ti)^alpha
  ln.ft <- log(alpha) + (alpha) * log(lambda) + (alpha - 1) * log(ti) - (lambda * ti)^alpha
  st    <- exp( -(lambda * ti)^alpha )    # S(t) for current period
  st0   <- exp( -(lambda * t0)^alpha )    # S(t) for previous period
  
  # Likelihood contributions
  cens   <- ifelse((lo==0) | (rc==0), log(pr0 + (pr1 * st)) - log(pr0 + (pr1 * st0)), 0)
  nocens <- ifelse((rc==1) & (lo==1), log(pr1) + ln.ft      - log(pr0 + (pr1 * st0)), 0)

  logl <- sum(cens+nocens)
  return(-logl)
}