## predict.spdur

predict.spdur <- function(object, data=NULL, sims=500, ...)
{
  if(is.null(data)) {
    data <- get(paste(object$call$data), pos=1)
  }
  
  # Duration equation
  mf.dur <- model.frame(formula=object$call$duration, data=data)
  X <- model.matrix(attr(mf.dur, 'terms'), data=mf.dur)
  lhb <- model.response(mf.dur)
    
  # Risk/non-immunity equation
  mf.risk <- model.frame(formula=object$call$atrisk, data=data)
  Z <- model.matrix(attr(mf.risk, 'terms'), data=mf.risk)
  lhg <- model.response(mf.risk)
    
  # Y vectors
  censor <- data[, as.character(object$call$last)[3]]
  Y <- cbind(atrisk=lhg, duration=lhb, last=censor)

  if (object$distr=='weibull') {
    res <- pred_weibull(coef=coef(object), vcv=object$vcv, y=Y, X=X, Z=Z, sims)
  }
  if (object$distr=='loglog') {
    res <- pred_loglog(y=Y, X=X, Z=Z, sims)
  }
  
  return(res)
}

pred_weibull <- function(coef, vcv, y, X, Z, sims) {
  require(MASS)
  
  # Empty results matrix
  cure <- matrix(NA, nrow(y), sims)
  
  for (i in 1:sims){
    draw <- mvrnorm(1, coef, vcv)
    
    lxbeta <- X %*% draw[1:ncol(X)]
    pxbeta <- Z %*% draw[(ncol(X)+1):(ncol(X)+ncol(Z))]
    alph <- 1*draw[ncol(X)+ncol(Z)+1]
    
    alph <- exp(-alph)
    lyhat <- exp(-lxbeta)
    pyhat <- 1/(1+exp(-pxbeta))
    cure[, i]<-pyhat/((lyhat*y[,2])^alph+pyhat*(1-(lyhat*y[,2])^alph))
  }
  print(dim(cure))
  predvalues <- apply(cure, 1, quantile, probs = c(0.025,0.5,0.975), na.rm=T)
  predvalues <- t(predvalues)
  return(predvalues)
}

pred_loglog <- function(y, X, Z, sims) {
  cyhat <- matrix(NA, nrow(X), reps)
}