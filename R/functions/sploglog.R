sploglog <- function(Y, X, Z, max.iter) {
  require(corpcor) # make.positive.definite
  # Base model likelihood
  loglog.lik <- function(theta, y, X){
    beta <- theta[1:ncol(X)]
    g <- theta[ncol(X) + 1]
    d <- y[,1]
    ti <- y[,2]
    t0 <- y[,2]-1
    ly <- y[,3]
    lambda <- exp(-X%*%beta)
    alpha <- exp(-g)
    su <- log((1+(lambda*t0)^g))-log((1+(lambda*ti)^g))
    haz <- log(g)+g*log(lambda)+(g-1)*log(ti)-log(1+(lambda*ti)^g)
    cens <- ifelse((d==1) & (ly==0) | (d==0) , su , 0)
    nocens <- ifelse((d==1) & (ly==1) , haz+su , 0)
    logl <- sum(cens+nocens)
    return(-logl)
  }
  
  # Estimate base model
  base.inits <- c(rep(0, ncol(X)), 1)
  cat('Fitting base loglog...\n')
  base <- optim(base.inits, loglog.lik, method="BFGS", control=list(maxit=max.iter), hessian=T, y=Y, X=X)
  
  # Full model likelihood
  sploglog.lik <- function(theta,y,X,Z){
    rx<-ncol(X)
    rz<-ncol(Z)
    beta<-theta[1:rx]
    gamma<-theta[(rx+1):(rx+rz)]
    g<-theta[rx+rz+1]
    d<-y[,1]
    ti<-y[,2]
    t0<-y[,2]-1
    ly<-y[,3]
    lambda<-exp(-X%*%beta)
    alpha<-exp(-g)
    pr1<-plogis(Z%*%gamma)
    pr0<-plogis(Z%*%gamma, lower.tail=F)
    su<-log((1+(lambda*t0)^g))-log((1+(lambda*ti)^g))
    haz<-log(g)+g*log(lambda)+(g-1)*log(ti)-log(1+(lambda*ti)^g)
    su.exp<-exp(su)
    cens<-ifelse((d==1) & (ly==0) | (d==0), log(pr0+pr1*su.exp) , 0)
    nocens<-ifelse((d==1) & (ly==1), log(pr1)+haz+log(su.exp) , 0)
    logl<-sum(cens+nocens)
    return(-logl)
  }
  
  # Estimate full model
  x.inits <- base$par[1:ncol(X)]
  a.init <- base$par[ncol(X)+1]
  cat('Fitting split loglog...\n')
  est <- optim(c(x.inits, rep(0, ncol(Z)), a.init), sploglog.lik, method="BFGS", control=list(trace=T, maxit=max.iter), hessian=T, y=Y, X=X, Z=Z)
  
  # Solve other results
  if (est$convergence!=0) stop('Model did not converge')
  coef <- est$par
  vcv <- solve(est$hessian)
  vcv <- make.positive.definite(vcv)
  logL <- -est$value
  
  # Put together results
  return(
    list(coefficients = coef,
         vcv = vcv,
         logL = logL))
}