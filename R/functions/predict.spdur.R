##########
# Predict and separationplot methods
# November 2012
# Andreas Beger
#
###########

## Main predict method, calls on predict functions for each distribution
predict.spdur <- function(object, data=NULL, stat='atrisk', ...)
{
  # Input validation
  if (!stat %in% c('cure', 'atrisk', 'c.f', 'c.h', 'u.f', 'u.h')) stop('unknown statistic')
  
  # Evaluate call values
  if(is.null(data)) {
    data <- eval.parent(object$call$data, 1)
  } 
  duration <- eval.parent(object$call$duration, 1)
  atrisk <- eval.parent(object$call$atrisk, 1)
  last <- data[, eval.parent(object$call$last, 1)]
  distr <- eval.parent(object$distr, 1)
  
  # Duration equation
  mf.dur <- model.frame(formula=duration, data=data)
  X <- model.matrix(attr(mf.dur, 'terms'), data=mf.dur)
  lhb <- model.response(mf.dur) 
  # Risk/non-immunity equation
  mf.risk <- model.frame(formula=atrisk, data=data)
  Z <- model.matrix(attr(mf.risk, 'terms'), data=mf.risk)
  lhg <- model.response(mf.risk) 
  # Y vectors
  Y <- cbind(atrisk=lhg, duration=lhb, last=last)

  if (distr=='weibull') {
    res <- pred_weibull(coef=coef(object), vcv=object$vcv, Y=Y, X=X, Z=Z, stat)
  }
  if (distr=='loglog') {
    res <- pred_loglog(coef=coef(object), vcv=object$vcv, Y=Y, X=X, Z=Z, stat)
  }
  
  return(res)
}

## Weibull predict function
# Input: parameters, data, and sims to run
# Output: row-matrix of predicted probability quantiles
#
pred_weibull <- function(coef, vcv, Y, X, Z, stat) {
  # In: matrices
  # Out: vector
  
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
  
  ## Determine result
  # conditional pr(non cure | T) = 1 - pr(non cure | T)
  cure.t <- cure / (st + cure * (1 - st))
  n.cure.t  <- 1 - cure.t
  if (stat=='cure') res <- cure.t
  if (stat=='atrisk') res <- n.cure.t
  # more obscure stats
  if (!stat %in% c('cure', 'atrisk')) {
    # f(t)
    ft <- la.hat * al.hat * (la.hat * Y[,2])^(al.hat-1) * exp(-(la.hat * Y[,2])^al.hat)
    
    if (stat=='c.f') res <- n.cure.t * ft / s0 # Pr(T=t | (T > t-1, not cured)) * Pr(not cured | T > t)
    if (stat=='c.h') res <- n.cure.t * ft / st # Pr(T=t | (T > t, not cured)) * Pr(not cured | T > t)
    if (stat=='u.f') res <- n.cure * ft / s0 # Pr(T=t | (T > t-1, not cured)) * Pr(not cured)
    if (stat=='u.h') res <- n.cure * ft / st # Pr(T=t | (T > t, not cured)) * Pr(not cured)
  }
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
  # In: matrices
  # Out: vector
  
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
  
  ## Determine result
  # conditional pr(non cure | T) = 1 - pr(non cure | T)
  cure.t <- cure / (st + cure * (1 - st))
  n.cure.t  <- 1 - cure.t
  if (stat=='cure') res <- cure.t
  if (stat=='atrisk') res <- n.cure.t
  # more obscure stats
  if (!stat %in% c('cure', 'atrisk')) {
    # f(t)
    ft <- (la.hat * al.hat * (la.hat * Y[,2])^(al.hat-1)) / ((1 + (la.hat * Y[,2])^al.hat)^2)
    
    if (stat=='c.f') res <- n.cure.t * ft / s0 # Pr(T=t | (T > t-1, not cured)) * Pr(not cured | T > t)
    if (stat=='c.h') res <- n.cure.t * ft / st # Pr(T=t | (T > t, not cured)) * Pr(not cured | T > t)
    if (stat=='u.f') res <- n.cure * ft / s0 # Pr(T=t | (T > t-1, not cured)) * Pr(not cured)
    if (stat=='u.h') res <- n.cure * ft / st # Pr(T=t | (T > t, not cured)) * Pr(not cured)
  }
  return(res)
}

## Forecast method
# Input: object of class 'spdur', time units to predict out
# Ouput: vector of forecast probabilities
#
forecast <- function(x, ...) { UseMethod('forecast') }

forecast.default <- function(x, ...) { NULL }

forecast.spdur <- function(object, pred.data = NULL, stat = 'atrisk', npred = 6, ...)
{
  if (is.null(pred.data)) stop("Must provide pred.data")
  valid.stat <- c('atrisk', 'cure', 'u.f')
  if (!stat %in% valid.stat) stop(paste('Invalid stat, choices are:', paste(valid.stat, collapse=' ')))
  
  # Evaluate call values
  duration <- eval.parent(object$call$duration, 1)
  atrisk <- eval.parent(object$call$atrisk, 1)
  distr <- eval.parent(object$distr, 1)
  
  # Duration equation
  mf.dur <- model.frame(formula=duration, data=pred.data)
  X <- model.matrix(attr(mf.dur, 'terms'), data=mf.dur)
  lhb <- model.response(mf.dur) 
  # Risk/non-immunity equation
  mf.risk <- model.frame(formula=atrisk, data=pred.data)
  Z <- model.matrix(attr(mf.risk, 'terms'), data=mf.risk)
  lhg <- model.response(mf.risk) 
  # Y vectors
  Y <- cbind(atrisk=0, duration=lhb, last=0)
  
  if (distr=='weibull') {
    res <- forecast_weibull(coef=coef(object), vcv=object$vcv, Y=Y, X=X, Z=Z, stat, npred)
  }
  if (distr=='loglog') {
    res <- forecast_loglog(coef=coef(object), vcv=object$vcv, Y=Y, X=X, Z=Z, stat, npred)
  }
  
  return(res)
}

## Weibull forecast function
#
forecast_weibull <- function(coef, vcv, Y, X, Z, stat, npred) {
  # In: matrices
  # Out: matrix with npred columns
  
  coeff.b <- coef[1 : ncol(X)]
  coeff.g <- coef[(ncol(X) + 1) : (ncol(X) + ncol(Z))]
  coeff.a <- coef[(ncol(X) + ncol(Z) + 1)]
  
  # alpha
  al.hat <- exp(-coeff.a)	
  # lambda
  la.hat.pred <- exp(-X %*% coeff.b)
  # unconditional pr(~cure) & pr(cure)
  n.cure.pred <- plogis(Z %*% coeff.g)
  cure.pred <- 1 - n.cure.pred
  
  ## Create emtpy matrices for various quantities
  rows <- length(la.hat.pred)
  # S(T)
  s0 <- exp(-(la.hat.pred * (Y[,2]-1))^al.hat)
  st <- matrix(nrow=rows, ncol=npred)
  # conditional pr(non cure | T) = 1 - pr(non cure | T)
  cure.t <- matrix(nrow=rows, ncol=npred)
  # Fill in values
  for (i in 1:npred) {
    st[, i] <- exp(-(la.hat.pred * (Y[, 2] + (i - 1)))^al.hat)
    cure.t[, i] <- cure.pred / (st[, i] + cure.pred * (1 - st[, i]))
  }
  n.cure.t <- 1 - cure.t
  
  if (stat=='cure') res <- cure.t
  if (stat=='atrisk') res <- n.cure.t
  
  # more obscure stats
  if (!stat %in% c('cure', 'atrisk')) {
    # f(t)
    ft <- matrix(nrow=rows, ncol=npred)
    # unconditional f(t)
    u.f <- matrix(nrow=rows, ncol=npred)
    
    for (i in 1:npred) {
      ft[, i] <- la.hat.pred * al.hat * (la.hat.pred * (Y[,2] + (i - 1)))^(al.hat - 1) * exp(-(la.hat.pred * (Y[,2] + (i - 1)))^al.hat)
      if (i==1) u.f[, i] <- n.cure.t[, i] * ft[, i] / s0
      if (i > 1) u.f[, i] <- n.cure.t[, i] * ft[, i] / st[, (i-1)]
    }
    if (stat=='u.f') res <- u.f
  }
  
  return(res)
}

## Loglog forecast function
#
forecast_loglog <- function(coef, vcv, Y, X, Z, stat, npred) {
  # In: matrices
  # Out: matrix with npred columns
  
  coeff.b <- coef[1 : ncol(X)]
  coeff.g <- coef[(ncol(X) + 1) : (ncol(X) + ncol(Z))]
  coeff.a <- coef[(ncol(X) + ncol(Z) + 1)]
  
  # alpha
  al.hat <- exp(-coeff.a)  
  # lambda
  la.hat.pred <- exp(-X %*% coeff.b)
  # unconditional pr(~cure) & pr(cure)
  n.cure.pred <- plogis(Z %*% coeff.g)
  cure.pred <- 1 - n.cure.pred
  
  ## Create emtpy matrices for various quantities
  rows <- length(la.hat.pred)
  # S(T)
  s0 <- 1/(1+(la.hat.pred * Y[,2])^al.hat)
  st <- matrix(nrow=rows, ncol=npred)
  # conditional pr(non cure | T) = 1 - pr(non cure | T)
  cure.t <- matrix(nrow=rows, ncol=npred)
  # Fill in values
  for (i in 1:npred) {
    st[, i] <- 1/(1 + (la.hat.pred * (Y[,2] + (i - 1)))^al.hat)
    cure.t[, i] <- cure.pred / (st[, i] + cure.pred * (1 - st[, i]))
  }
  n.cure.t <- 1 - cure.t
  
  if (stat=='cure') res <- cure.t
  if (stat=='atrisk') res <- n.cure.t
  
  # more obscure stats
  if (!stat %in% c('cure', 'atrisk')) {
    # f(t)
    ft <- matrix(nrow=rows, ncol=npred)
    # unconditional f(t)
    u.f <- matrix(nrow=rows, ncol=npred)
    
    for (i in 1:npred) {
      ft[, i] <- (la.hat.pred * al.hat * (la.hat.pred * (Y[,2] + (i - 1)))^(al.hat - 1)) / ((1 + (la.hat.pred * (Y[,2] + (i - 1)))^al.hat)^2)
      if (i==1) u.f[, i] <- n.cure.t[, i] * ft[, i] / s0
      if (i > 1) u.f[, i] <- n.cure.t[, i] * ft[, i] / st[, (i - 1)]
    }
    if (stat=='u.f') res <- u.f
  }
  
  return(res)
}