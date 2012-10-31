##########
# What: spdur R package functions
# Date: November 2012
# Who:  Daniel W. Hill, Nils Metternich, Andreas Beger
#
###########

##########
# spdur
# Top level wrapper
#
# to do:
# - clean up code
#
###########
spdur <- function (formula, cure = formula2, data = NULL, test = NULL, 
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

print.spdur

summary.spdur

##########
# spweibull
# Weibull wrapper
#
# to do:
# - clean up code
#
###########
spweibull<-function(y, X, Z, y.test, X.test, Z.test, iter, sims) {
  X<-as.matrix(cbind(1, X))
  Z<-as.matrix(cbind(1, Z))
  base.inits <- c(rep(0, ncol(X)), 0)
  base<-optim(base.inits, weib.lik, method="BFGS", control=list(maxit=iter), hessian=T, y=y, X=X)
  x.inits<-base$par[1:ncol(X)]
  a.init<-base$par[ncol(X)+1]
  est<-optim(c(x.inits, rep(0, ncol(Z)), a.init), spweib.lik, method="BFGS", control=list(trace=T, maxit=iter), hessian=T, y=y, X=X, Z=Z)
  if (est$convergence!=0) stop("Model did not converge")
  coeff<-est$par
  vcv<-solve(est$hessian)
  se<-sqrt(diag(vcv))
  zstat<-coeff/se
  pval<-2*(1-pnorm(abs(zstat)))
  
  # Put together results
  results <- cbind(coeff, se, zstat, pval)
  
  X.train<-as.matrix(X)
  Z.train<-as.matrix(Z)
  y.train<-as.matrix(y)
  
  X<-as.matrix(X.test)
  Z<-as.matrix(Z.test)
  y<-as.matrix(y.test)
  
  reps <- sims 
  
  cyhat.train <- matrix(NA, nrow(X.train), reps) 
  cyhat <- matrix(NA, nrow(X), reps)
  
  for (i in 1:reps){
    draw<-mvrnorm(1,coeff,vcv)
    
    lxbeta <- X.train %*% draw[1:ncol(X.train)]
    pxbeta <- Z.train %*% draw[(ncol(X.train)+1):(ncol(X.train)+ncol(Z.train))]
    alph <- 1*draw[ncol(X.train)+ncol(Z.train)+1]
    
    alph <-exp(-alph)
    lyhat <- exp(-lxbeta)
    pyhat <- 1/(1+exp(-pxbeta))
    cyhat.train[,i]<-pyhat/((lyhat*y.train[,2])^alph+pyhat*(1-(lyhat*y.train[,2])^alph))
    
    lxbeta <- X %*% draw[1:ncol(X)]
    pxbeta <- Z %*% draw[(ncol(X)+1):(ncol(X)+ncol(Z))]
    alph <- 1*draw[ncol(X)+ncol(Z)+1]
    
    alph<-exp(-alph)
    lyhat <- exp(-lxbeta)
    pyhat <- 1/(1+exp(-pxbeta))
    cyhat[,i]<-pyhat/((lyhat*y[,2])^alph+pyhat*(1-(lyhat*y[,2])^alph))
  }
  
  predvalues.train <- apply(cyhat.train, 1, quantile, probs = c(0.025,0.5,0.975), na.rm=T)         
  predvalues <- apply(cyhat, 1, quantile, probs = c(0.025,0.5,0.975), na.rm=T)         
  
  # Return results
  invisible(return(list(
    results=results, vcv=vcv, predvalues.train=predvalues.train, 
    predvalues=predvalues, pyhat=pyhat)))
}
  
# X <- subset(duration, select=c('d.major', 'd.minor', 'ln.gdppc.l1'))
# X <- as.matrix(cbind(1, X))
# y <- cbind(duration['cured'], duration['duration'], duration$end.spell)
# theta <- c(runif(ncol(X), 0, 1), 0)  

weib.lik <- function(theta,y,X){
  beta<-theta[1:ncol(X)]
  p<-theta[ncol(X)+1]
  d<-y[,1]
  ti<-y[,2]
  t0<-y[,2]-1
  ly<-y[,3]
  lambda<-exp(-X%*%beta)
  alpha<-exp(-p)
  haz<-log(alpha) + (alpha)*log(lambda) + (alpha-1)*log(ti)
  su<-log(exp(-(lambda*ti)^alpha)/exp(-(lambda*t0)^alpha)) 
  cens<-ifelse((d==1) & (ly==0) | (d==0) , su , 0)
  nocens<-ifelse((d==1) & (ly==1) , haz+su ,0)
  logl<-sum(cens+nocens)
  return(-logl)
}

##########
# sploglog
# LogLog wrapper
#
# to do:
# - clean up code
#
###########
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