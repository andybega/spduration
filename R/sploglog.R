#' Split-population Log-logistic regression
#' 
#' @importFrom corpcor make.positive.definite
sploglog <- function(Y, X, Z, max.iter, silent=FALSE) {
  # Estimate base model
  if (!exists("base.inits")) {
    base.inits <- c(rep(0, ncol(X)), 1)
  }
  if (!silent) cat('Fitting base loglog...\n')
  base <- loglog(Y=Y, X=X, inits=base.inits, max.iter=200, silent=TRUE)
    
  # Estimate full model
  x.inits <- base$coefficients[1:ncol(X)]
  a.init <- base$coefficients[ncol(X)+1]
  if (!silent) cat('Fitting split loglog...\n')
  trace <- !silent
  est <- optim(c(x.inits, rep(0, ncol(Z)), a.init), sploglog_lnl, method="BFGS", 
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

#' Regular Log-logistic regression
loglog <- function(Y, X, inits=NULL, max.iter, silent=TRUE) {
  if (is.null(inits)) {
    inits <- c(rep(0, ncol(X)), 0)
  }
  
  trace <- !silent
  est <- optim(inits, loglog_lnl, method="BFGS", 
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

#' Regular Log-logistic likelihood
loglog_lnl <- function(theta, y, X) {
  beta <- theta[1:ncol(X)]
  g <- theta[ncol(X) + 1]
  d <- y[,1]
  ti <- y[,2]
  t0 <- y[,4]
  ly <- y[,3]
  lambda <- exp(-X%*%beta)
  alpha <- exp(-g)
  ln.ft <- log(alpha) + alpha*log(lambda) + (alpha-1)*log(ti) -2 * log(1+(lambda*ti)^alpha)
  ln.st  <- -log((1+(lambda*ti)^alpha))
  ln.st0 <- -log((1+(lambda*t0)^alpha))
  cens <- ifelse((d==1) & (ly==0) | (d==0) , ln.st - ln.st0 , 0)
  nocens <- ifelse((d==1) & (ly==1) , ln.ft - ln.st0 , 0)
  logl <- sum(cens+nocens)
  return(-logl)
}

#' Split-population Log-logistic log likelihood
sploglog_lnl <- function(theta, y, X, Z) {
  rx<-ncol(X)
  rz<-ncol(Z)
  beta<-theta[1:rx]
  gamma<-theta[(rx+1):(rx+rz)]
  g<-theta[rx+rz+1]
  d<-y[,1]
  ti<-y[,2]
  t0 <- y[,4]
  ly<-y[,3]
  lambda<-exp(-X%*%beta)
  alpha<-exp(-g)
  pr1 <- plogis(Z%*%gamma)
  pr0 <- plogis(Z%*%gamma, lower.tail=F)
  ln.ft <- log(alpha) + alpha*log(lambda) + (alpha-1)*log(ti) -2 * log(1+(lambda*ti)^alpha)
  st  <- 1/((1+(lambda*ti)^alpha))
  st0 <- 1/((1+(lambda*t0)^alpha))
  nocens <- ifelse((d==1) & (ly==1), log(pr1) + ln.ft - log(pr0 + (pr1 * st0)), 0)
  cens <- ifelse((d==1) & (ly==0) | (d==0), log(pr0 + (pr1 * st)) - log(pr0 + (pr1 * st0)), 0)
  logl<-sum(cens+nocens)
  return(-logl)
}