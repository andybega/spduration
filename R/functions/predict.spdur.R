##########
# Predict and separationplot methods
# November 2012
# Andreas Beger
#
###########

## Main predict method, calls on predict functions for each distribution
predict.spdur <- function(object, data=NULL, stat='cure', ...)
{
  if(is.null(data)) {
    data <- get(paste(object$call$data))
  } 
  
  print(object$call)
  print(sys.nframe())
  print(eval(object$call$duration), sys.frame(sys.parent(2)))
  ## problem is that object has unevaluated duration in it's call
  
  
  # Duration equation
  mf.dur <- model.frame(formula=object$call$duration, data=data)
  print('predict 1.1')
  X <- model.matrix(attr(mf.dur, 'terms'), data=mf.dur)
  print('predict 1.2')
  lhb <- model.response(mf.dur) 
  print('predict 1.3')
  # Risk/non-immunity equation
  mf.risk <- model.frame(formula=object$call$atrisk, data=data)
  Z <- model.matrix(attr(mf.risk, 'terms'), data=mf.risk)
  lhg <- model.response(mf.risk) 
  # Y vectors
  print('predict 1.4')
  Y <- cbind(atrisk=lhg, duration=lhb, last=data[, object$call$last])

  print('predict 2')
  if (object$distr=='weibull') {
    res <- pred_weibull(coef=coef(object), vcv=object$vcv, Y=Y, X=X, Z=Z, stat)
  }
  if (object$distr=='loglog') {
    res <- pred_loglog(coef=coef(object), vcv=object$vcv, Y=Y, X=X, Z=Z, stat)
  }
  
  return(res)
}

## Weibull predict function
# Input: parameters, data, and sims to run
# Output: row-matrix of predicted probability quantiles
#
pred_weibull <- function(coef, vcv, Y, X, Z, stat) {
  
  coeff.b <- coef[1 : ncol(X)]
  coeff.g <- coef[(ncol(X) + 1) : (ncol(X) + ncol(Z))]
  coeff.a <- coef[(ncol(X) + ncol(Z) + 1)]
  
  # alpha
  al.hat <- exp(-coeff.a)  
  # lambda
  la.hat <- exp(-X %*% coeff.b)
  # unconditional pr(~cure) & pr(cure)
  n.cure <- plogis(Z %*% coeff.g)
  cure <- 1 - n.cure
  # S(T)
  st <- exp(-(la.hat * Y[,2])^al.hat)
  s0 <- exp(-(la.hat * (Y[,2]-1))^al.hat)
  # conditional pr(non cure | T) = 1 - pr(non cure | T)
  cure.t <- cure / (st + cure * (1 - st))
  n.cure.t  <- 1 - cure.t
  # f(t)
  ft <- la.hat * al.hat * (la.hat * Y[,2])^(al.hat-1) * exp(-(la.hat * Y[,2])^al.hat)
  
  # quantities of interest
  pr.c.f <- n.cure.t * ft / s0 # Pr(T=t | (T > t-1, not cured)) * Pr(not cured | T > t)
  pr.c.h <- n.cure.t * ft / st # Pr(T=t | (T > t, not cured)) * Pr(not cured | T > t)
  pr.u.f <- n.cure * ft / s0 # Pr(T=t | (T > t-1, not cured)) * Pr(not cured)
  pr.u.h <- n.cure * ft / st # Pr(T=t | (T > t, not cured)) * Pr(not cured)
  
  res <- data.frame(n.cure.t)
  return(res)
}
  
#   # Empty results matrix
#   cure <- matrix(NA, nrow(y), sims)
#   
#   for (i in 1:sims){
#     draw <- mvrnorm(1, coef, vcv)
#     
#     lxbeta <- X %*% draw[1:ncol(X)]
#     pxbeta <- Z %*% draw[(ncol(X)+1):(ncol(X)+ncol(Z))]
#     alph <- 1*draw[ncol(X)+ncol(Z)+1]
#     
#     alph <- exp(-alph)
#     lyhat <- exp(-lxbeta)
#     pyhat <- 1/(1+exp(-pxbeta))
#     cure[, i]<-pyhat/((lyhat*y[,2])^alph+pyhat*(1-(lyhat*y[,2])^alph))
#   }
#   print(dim(cure))
#   predvalues <- apply(cure, 1, quantile, probs = c(0.025,0.5,0.975), na.rm=T)
#   predvalues <- t(predvalues)
#   return(predvalues)
# }

## Loglog predict function
# Input: parameters, data, and sims to run
# Output: row-matrix of predicted probability quantiles
#
pred_loglog <- function(coef, vcv, Y, X, Z, stat) {
  
  coeff.b <- coef[1 : ncol(X)]
  coeff.g <- coef[(ncol(X) + 1) : (ncol(X) + ncol(Z))]
  coeff.a <- coef[(ncol(X) + ncol(Z) + 1)]
  
  # alpha
  al.hat <- exp(-coeff.a)  
  # lambda
  la.hat <- exp(-X %*% coeff.b)
  # unconditional pr(~cure) & pr(cure)
  n.cure <- plogis(Z %*% coeff.g)
  cure <- 1 - n.cure
  # S(T)
  st <- exp(-(la.hat * Y[,2])^al.hat)
  s0 <- exp(-(la.hat * (Y[,2]-1))^al.hat)
  # conditional pr(non cure | T) = 1 - pr(non cure | T)
  cure.t <- cure / (st + cure * (1 - st))
  n.cure.t  <- 1 - cure.t.in
  # f(t)
  ft <- (la.hat * al.hat * (la.hat * Y[,2])^(al.hat-1)) / ((1 + (la.hat * Y[,2])^al.hat)^2)
  
  # quantities of interest
  ## in-sample
  pr.c.f <- n.cure.t * ft / s0 # Pr(T=t | (T > t-1, not cured)) * Pr(not cured | T > t)
  pr.c.h <- n.cure.t * ft / st # Pr(T=t | (T > t, not cured)) * Pr(not cured | T > t)
  pr.u.f <- n.cure * ft / s0 # Pr(T=t | (T > t-1, not cured)) * Pr(not cured)
  pr.u.h <- n.cure * ft / st # Pr(T=t | (T > t, not cured)) * Pr(not cured)
  
  res <- data.frame(n.cure.t)
  return(res)
}

## Separationplot method for spdur
# Input: parameters, data, and sims to run
# Output: row-matrix of predicted probability quantiles
#
plot.spdur <- function(object, failure='failure', endSpellOnly=F, ...)
{
  require(separationplot)
  
  # Input validation
  
  # Get predicted/observed values
  pred <- as.vector(as.matrix(predict(object)))
  actual <- get(paste(object$call$data))[, failure]
  
  # Keep end of spell only
  if (endSpellOnly==T) {
    pred <- pred[ get(paste(object$call$data))[, object$call$last]==1 ]
    actual <- actual[ get(paste(object$call$data))[, object$call$last]==1 ]
  }

  # Separationplot call
  plot <- separationplot(pred, actual,
                 shuffle=T, heading='', show.expected=T, newplot=F, 
                 type='line', lwd1=5, lwd2=2)
  return(plot)
}

## Forecast method
# Input: object of class 'spdur', time units to predict out
# Ouput: row-matrix of forecast probabilities
#
forecast.spdur <- function(object, npred = 1, ...)
{
  NULL
}