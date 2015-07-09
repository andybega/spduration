#'  Simulate and plot hazard function
#'  
#'  Given a set of values for the duration and risk equations, plot the shape of
#'  estimated hazard function in respect to duration. Confidence intervals are 
#'  provided through simulation.
#'  
#'  @param x An object of class \code{spdur}
#'  @param xvals A vector of values for the duration equation variables, in the 
#'    same order as the duration equation in \code{x}. Defaults to means.
#'  @param zvals A vector of values for the risk equation varialbes, in teh same
#'    order as the risk equation in \code{x}. Defaults to means.
#'    
#'  @importFrom MASS mvrnorm
#'  @importFrom graphics plot lines
#'  @importFrom stats plogis quantile
#'  @export
plot_hazard2 <- function(x,xvals=NULL,zvals=NULL){
  dur.dat<-x$mf.dur
  risk.dat<-x$mf.risk 
  ti<-seq(1, max(dur.dat[, 1])*1.2, length.out=100)
  X<-cbind(1, dur.dat[ ,2:dim(dur.dat)[2]])
  Z<-cbind(1, risk.dat[ ,2:dim(risk.dat)[2]])
  
  beta<-x$coef[1:ncol(X)]
  beta.vcv<-x$vcv[1:ncol(X),1:ncol(X)]
  gamma<-x$coef[(ncol(X) + 1):(ncol(X) + ncol(Z))]
  gamma.vcv<-x$vcv[(ncol(X) + 1):(ncol(X) + ncol(Z)),(ncol(X) + 1):(ncol(X) + ncol(Z))]
  a<-x$coef[ncol(X) + ncol(Z) + 1]
  alpha<-exp(-a)
  
  if (is.null(xvals)){
    X.vals<-apply(X,2,mean)		
  } else{
    X.vals<-c(1,xvals)			
  }
  
  if (is.null(zvals)){
    Z.vals<-apply(Z,2,mean)
  } else{
    Z.vals<-c(1,zvals)
  }
  
  Beta<-mvrnorm(n=1000,mu=beta,Sigma=beta.vcv)	
  lambda<-pmax(1e-10, exp(-X.vals %*% t(Beta)))
  Gamma<-mvrnorm(n=1000,mu=gamma,Sigma=gamma.vcv)
  cure<-1 - plogis(Z.vals %*% t(Gamma))
  
  preds<-matrix(nrow=length(ti),ncol=3)
  
  if (x$distr=="weibull"){
    for(i in 1:length(ti)){
      st<-exp(-(lambda * ti[i])^alpha)
      cure.t<-cure / pmax(1e-10, (st + cure * (1 - st)))
      atrisk.t<-1 - cure.t
      ft<-lambda * alpha * (lambda * ti[i])^(alpha-1) * exp(-(lambda * ti[i])^alpha)
      pr<-atrisk.t * ft / pmax(1e-10, (cure.t + atrisk.t * st))
      preds[i,1]<-mean(pr)
      preds[i,2]<-quantile(pr,probs=0.05)
      preds[i,3]<-quantile(pr,probs=0.95)
    }
  }
  
  if (x$distr=="loglog"){
    for(i in 1:length(ti)){
      st<-1/(1+(lambda * ti[i])^alpha)
      cure.t<-cure / pmax(1e-10, (st + cure * (1 - st)))
      atrisk.t<-1 - cure.t
      ft<-(lambda * alpha * (lambda * ti[i])^(alpha-1)) / ((1 + (lambda * ti[i])^alpha)^2)
      pr<-atrisk.t * ft / pmax(1e-10, (cure.t + atrisk.t * st))
      preds[i,1]<-mean(pr)
      preds[i,2]<-quantile(pr,probs=0.05)
      preds[i,3]<-quantile(pr,probs=0.95)
    }
  }
  
  plot(ti,preds[,1],type="l",xlab="Time",ylab="Conditional Hazard",ylim=c(0,max(preds[,3])))
  lines(ti,preds[,2],lty=2)
  lines(ti,preds[,3],lty=2)
  
}