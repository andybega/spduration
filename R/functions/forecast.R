## Forecast method
# Input: object of class 'spdur', time units to predict out
# Ouput: vector of forecast probabilities
#
forecast <- function(object, ...) { UseMethod('forecast') }

forecast.default <- function(object, ...) { NULL }

forecast.spdur <- function(object, ..., pred.data = NULL, stat = 'hazard', npred = 6)
{
  if (is.null(pred.data)) stop("Must provide pred.data")
  stat_choices <- c('conditional risk', 'conditional cure', 'hazard', 'failure',
                    'unconditional risk', 'unconditional cure', 
                    'conditional hazard', 'conditional failure')
  
  if (!stat %in% stat_choices) stop('unknown statistic')
  
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
  atrisk.pred <- plogis(Z %*% coeff.g)
  cure.pred <- 1 - atrisk.pred
  
  if (stat=='unconditional cure') res <- matrix(rep(cure.pred, npred), ncol=npred)
  if (stat=='unconditional risk') res <- matrix(rep(atrisk.pred, npred), ncol=npred)
  
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
  atrisk.t <- 1 - cure.t
  
  if (stat=='conditional cure') res <- cure.t
  if (stat=='conditional risk') res <- atrisk.t
  
  # f(t)
  ft <- matrix(nrow=rows, ncol=npred)
  # unconditional f(t)
  u.f <- matrix(nrow=rows, ncol=npred)
  # unconditional hazard
  u.h <- matrix(nrow=rows, ncol=npred)
    
  for (i in 1:npred) {
    ft[, i] <- la.hat.pred * al.hat * (la.hat.pred * (Y[,2] + (i - 1)))^(al.hat - 1) * 
      exp(-(la.hat.pred * (Y[,2] + (i - 1)))^al.hat)
    if (i==1) u.f[, i] <- atrisk.t[, i] * ft[, i] / s0
    if (i > 1) u.f[, i] <- atrisk.t[, i] * ft[, i] / st[, (i-1)]
  }
  
  if (stat=='hazard') res <- NULL
  if (stat=='failure') res <- u.f
  
  if (stat=='conditional hazard') res <- NULL
  if (stat=='conditional failure') res <- NULL
  
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
  atrisk.pred <- plogis(Z %*% coeff.g)
  cure.pred <- 1 - atrisk.pred
  
  if (stat=='unconditional cure') res <- matrix(rep(cure.pred, npred), ncol=npred)
  if (stat=='unconditional risk') res <- matrix(rep(atrisk.pred, npred), ncol=npred)
  
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
  atrisk.t <- 1 - cure.t
  
  if (stat=='conditional cure') res <- cure.t
  if (stat=='conditional risk') res <- atrisk.t
  
  # f(t)
  ft <- matrix(nrow=rows, ncol=npred)
  # unconditional f(t)
  u.f <- matrix(nrow=rows, ncol=npred)
    
  for (i in 1:npred) {
    ft[, i] <- (la.hat.pred * al.hat * (la.hat.pred * (Y[,2] + (i - 1)))^(al.hat - 1)) / 
      ((1 + (la.hat.pred * (Y[,2] + (i - 1)))^al.hat)^2)
    if (i==1) u.f[, i] <- atrisk.t[, i] * ft[, i] / s0
    if (i > 1) u.f[, i] <- atrisk.t[, i] * ft[, i] / st[, (i - 1)]
  }
  if (stat=='failure') res <- u.f
  
  return(res)
}