## Loglog part

##########
# sploglog
# LogLog wrapper
#
# to do:
# - clean up code
#
###########
sploglog<-function(y, X, Z, y.test, X.test, Z.test, iter, sims) {
  X<-as.matrix(cbind(1, X))
  Z<-as.matrix(cbind(1, Z))
  X.test <- as.matrix(cbind(1, X.test))
  Z.test <- as.matrix(cbind(1, Z.test))
  base<-optim(c(rep(1, ncol(X)+1)), loglog.lik, method="BFGS", control=list(maxit=iter), hessian=T, y=y, X=X)
  x.inits<-base$par[1:ncol(X)]
  a.init<-base$par[ncol(X)+1]
  est<-optim(c(x.inits, rep(1, ncol(Z)), a.init), sploglog.lik, method="BFGS", control=list(trace=T, maxit=iter), hessian=T, y=y, X=X, Z=Z)
  if (est$convergence!=0) stop("Model did not converge")
  coeff<-est$par
  vcv<-solve(est$hessian)
  se<-sqrt(diag(vcv))
  zstat<-coeff/se
  pval<-2*(1-pnorm(abs(zstat)))
  print(results<-cbind(coeff, se, zstat, pval))
  
  X.train<-as.matrix(X[train$last.observation==1,])
  Z.train<-as.matrix(Z[train$last.observation==1,])
  y.train<-as.matrix(y[train$last.observation==1,])
  
  X<-as.matrix(X.test[test$last.observation==1,])
  Z<-as.matrix(Z.test[test$last.observation==1,])
  y<-as.matrix(y.test[test$last.observation==1,])
  
  reps <- sims 
  cyhat.train <- matrix(NA, nrow(X.train), reps) 
  cyhat <- matrix(NA, nrow(X), reps) 
  
  for (i in 1:reps){
    draw<-mvrnorm(1,coeff,vcv)
    
    lxbeta <- X.train %*% draw[1:ncol(X.train)]
    pxbeta <- Z.train %*% draw[(ncol(X.train)+1):(ncol(X.train)+ncol(Z.train))]
    alph <- 1*draw[ncol(X.train)+ncol(Z.train)+1]
    
    alph<-exp(-alph)
    lyhat <- exp(-lxbeta)
    pyhat <- 1/(1+exp(-pxbeta))
    cyhat.train[,i]<-pyhat/(1/(1+(lyhat*y.train[,2])^alph)+pyhat*(1-1/(1+(lyhat*y.train[,2])^alph)))
    
    lxbeta <- X %*% draw[1:ncol(X)]
    pxbeta <- Z %*% draw[(ncol(X)+1):(ncol(X)+ncol(Z))]
    alph <- 1*draw[ncol(X)+ncol(Z)+1]
    
    alph<-exp(-alph)
    lyhat <- exp(-lxbeta)
    pyhat <- 1/(1+exp(-pxbeta))
    cyhat[,i]<-pyhat/(1/(1+(lyhat*y[,2])^alph)+pyhat*(1-1/(1+(lyhat*y[,2])^alph)))
  }
  
  predvalues.train <- apply(cyhat.train, 1, quantile, probs = c(0.025,0.5,0.975), na.rm=T)         
  predvalues <- apply(cyhat, 1, quantile, probs = c(0.025,0.5,0.975), na.rm=T)         
  invisible(return(list(results, vcv, predvalues.train, predvalues, pyhat)))
}


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

loglog.lik <- function(theta,y,X){
  k<-ncol(X)
  beta<-theta[1:k]
  g<-theta[k+1]
  d<-y[,1]
  ti<-y[,2]
  t0<-y[,2]-1
  ly<-y[,3]
  lambda<-exp(-X%*%beta)
  alpha<-exp(-g)
  su<-log((1+(lambda*t0)^g))-log((1+(lambda*ti)^g))
  haz<-log(g)+g*log(lambda)+(g-1)*log(ti)-log(1+(lambda*ti)^g)
  cens<-ifelse((d==1) & (ly==0) | (d==0) , su , 0)
  nocens<-ifelse((d==1) & (ly==1) , haz+su , 0)
  logl<-sum(cens+nocens)
  return(-logl)
}