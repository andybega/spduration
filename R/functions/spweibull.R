spweibull <- function(Y, X, Z, max.iter) {  
  # Base model likelihood
  weib.lik <- function(theta, y, X) {
    beta <- theta[1:ncol(X)]
    p <- theta[ncol(X) + 1]
    d <- y[,1]
    ti <- y[,2]
    t0 <- y[,2]-1
    ly <- y[,3]
    lambda <- exp(-X%*%beta)
    alpha <- exp(-p)
    haz <- log(alpha) + (alpha)*log(lambda) + (alpha-1)*log(ti)
    su <- log(exp(-(lambda*ti)^alpha)/exp(-(lambda*t0)^alpha)) 
    cens <- ifelse((d==1) & (ly==0) | (d==0) , su , 0)
    nocens <- ifelse((d==1) & (ly==1) , haz+su ,0)
    logl <- sum(cens+nocens)
    return(-logl)
  }
  
  # Estimate base model
  base.inits <- c(rep(0, ncol(X)), 0)
  cat('Fitting base weibull...\n')
  base <- optim(base.inits, weib.lik, method="BFGS", control=list(maxit=max.iter), hessian=T, y=Y, X=X)
  
  # Full model likelihood
  spweib.lik <- function(theta, y, X, Z) {
    rx <- ncol(X)
    rz <- ncol(Z)
    beta <- theta[1:rx]
    gamma <- theta[(rx+1):(rx+rz)]
    p <- theta[rx+rz+1]
    d <- y[,1]
    ti <- y[,2]
    t0 <- y[,2]-1
    ly <- y[,3]
    lambda <- exp(-X%*%beta)
    alpha <- exp(-p)
    pr1 <- plogis(Z%*%gamma)
    pr0 <- plogis(Z%*%gamma, lower.tail=F)
    haz <- log(alpha) + (alpha)*log(lambda) + (alpha-1)*log(ti)
    su <- log(exp(-(lambda*ti)^alpha)/exp(-(lambda*t0)^alpha)) 
    su.exp <- exp(su)
    cens <- ifelse((d==1) & (ly==0) | (d==0), log(pr0+pr1*su.exp), 0)
    nocens <- ifelse((d==1) & (ly==1), log(pr1)+haz+log(su.exp), 0)
    logl <- sum(cens+nocens)
    return(-logl)
  }
  
  # Estimate full model
  x.inits <- base$par[1:ncol(X)]
  a.init <- base$par[ncol(X)+1]
  cat('Fitting split weibull...\n')
  est <- optim(c(x.inits, rep(0, ncol(Z)), a.init), spweib.lik, method="BFGS", control=list(trace=T, maxit=max.iter), hessian=T, y=Y, X=X, Z=Z)
  
  # Solve other results
  if (est$convergence!=0) stop('Model did not converge')
  coef <- est$par
  vcv <- solve(est$hessian)
  logL <- -est$value
  
  # Put together results
  return(
    list(coefficients = coef,
         vcv = vcv,
         logL = logL))
}