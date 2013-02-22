###########
# Predict and separationplot methods
# December 2012
# Andreas Beger
#
###########

## Main predict method, calls on predict functions for each distribution
predict.spdur <- function(object, data=NULL, stat='conditional risk', ...) {
  # Input validation
  stat_choices <- c('conditional risk', 'conditional cure', 'hazard', 'failure',
                    'unconditional risk', 'unconditional cure', 
                    'conditional hazard', 'conditional failure')
                    
  if (!stat %in% stat_choices) stop('unknown statistic')
  
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
  
  ## Start with actual prediction
  # coefficients
  coef <- coef(object)
  coeff.b <- coef[1 : ncol(X)]
  coeff.g <- coef[(ncol(X) + 1) : (ncol(X) + ncol(Z))]
  coeff.a <- coef[(ncol(X) + ncol(Z) + 1)]
  
  # alpha
  al.hat <- exp(-coeff.a)  
  # lambda
  la.hat <- exp(-X %*% coeff.b)
  
  # Unconditional cure/atrisk rate
  atrisk <- plogis(Z %*% coeff.g)
  cure <- 1 - atrisk
  if (stat=='unconditional cure') res <- cure
  if (stat=='unconditional risk') res <- atrisk
  
  # S(T)
  if (distr=='weibull') {
    st <- exp(-(la.hat * Y[,2])^al.hat)
    s0 <- exp(-(la.hat * (Y[,2]-1))^al.hat)
  }
  if (distr=='loglog') {
    st <- 1/(1+(la.hat * Y[,2])^al.hat)
    s0 <- 1/(1+(la.hat * Y[,4])^al.hat)
  }
  
  # Conditional cure/atrisk rate
  cure.t <- cure / (st + cure * (1 - st))
  atrisk.t  <- 1 - cure.t
  if (stat=='conditional cure') res <- cure.t
  if (stat=='conditional risk') res <- atrisk.t
  
  # f(t)
  if (distr=='weibull') {
    ft <- la.hat * al.hat * (la.hat * Y[,2])^(al.hat-1) * exp(-(la.hat * Y[,2])^al.hat) 
  }
  if (distr=='loglog') {
    ft <- (la.hat * al.hat * (la.hat * Y[,2])^(al.hat-1)) / ((1 + (la.hat * Y[,2])^al.hat)^2)  
  }
  
  if (stat=='conditional failure') res <- atrisk.t * ft / s0 # Pr(T=t | (T > t-1, not cured)) * Pr(not cured | T > t)
  if (stat=='conditional hazard')  res <- atrisk.t * ft / st # Pr(T=t | (T > t, not cured)) * Pr(not cured | T > t)
  if (stat=='failure')             res <- atrisk * ft / s0   # Pr(T=t | (T > t-1, not cured)) * Pr(not cured)
  if (stat=='hazard')              res <- atrisk * ft / st   # Pr(T=t | (T > t, not cured)) * Pr(not cured)
  
  return(res)
}