##########
# What: spdur R package core functions
# Date: November 2012
# Who:  Andreas Beger, Daniel W. Hill, Nils Metternich
#
###########

spdur <- function(duration, atrisk, data=list(), last, distr='weibull', 
                  max.iter=100, ...)
{
  # Duration equation
  mf.dur <- model.frame(formula=duration, data=data)
  X <- model.matrix(attr(mf.dur, 'terms'), data=mf.dur)
  lhb <- model.response(mf.dur)
  # Risk/non-immunity equation
  mf.risk <- model.frame(formula=atrisk, data=data)
  Z <- model.matrix(attr(mf.risk, 'terms'), data=mf.risk)
  lhg <- model.response(mf.risk)
  # Y vectors
  censor <- last
  Y <- cbind(atrisk=lhg, duration=lhb, last=censor)
  
  # Estimation
  if (distr=='weibull') {
    est <- spweibull(Y, X, Z, max.iter)
  }
  if (distr=='loglog') {
    est <- sploglog(Y, X, Z, max.iter)
  }
  # Names
  varnames <- c(paste(unlist(attr(X, 'dimnames')[2])), paste(unlist(attr(Z, 'dimnames')[2])), 'alpha')
  attr(est$coefficients, 'names') <- varnames
  colnames(est$vcv) <- rownames(est$vcv) <- varnames
  
  # Calculate uncertainty measures
  est$se <- sqrt(diag(est$vcv))
  est$zstat <- with(est, coefficients/se)
  est$pval <- 2*(1-pnorm(abs(est$zstat)))
  
  est$call <- match.call()
  est$distr <- distr
  class(est) <- 'spdur'
  return(est)
}

summary.spdur <- function(object, ...)
{
  table <- cbind(Estimate = coef(object),
                 StdErr = object$se,
                 Z = object$zstat,
                 p = object$pval)
  res <- list(call = object$call,
              coefficients = table)
  class(res) <- 'summary.spdur'
  return(res)
}

print.summary.spdur <- function(x, ...)
{
  cat('Call:\n')
  print(x$call)
  cat('\n')
  
  printCoefmat(x$coefficients, P.value=T, has.Pvalue=T, digits=4)
}

##########
# spdur.icews
# Top level wrapper
#
# in:  
#
# to do:
# - clean up code
#
###########
spdur.crisp <- function (duration = formula, atrisk = formula2, data = NULL, test = NULL, 
                         last = NULL, distr = NULL, stat = cure, iter = 100, ...) 
{
  if (is.null(data)) 
    stop("No data provided")
  if (is.null(last)) 
    stop("Must specify censoring variable")
  if (distr == NULL) 
    stop("Must specify distribution")
  
  # Estimate parameters
  model <- spdur(duration, atrisk, data=data, last, distr=distr, max.iter=100)
  
  # In-sample and test predictions
  train <- predict(model, stat=stat)
  test <- predict(model, data=test, stat=stat)
  
  # Forecast
  pred <- forecast(model, npred = 6)
  
  # Format and show estimates
  print(model)
  
  res <- model
  res$train <- train
  res$test <- test
  res$pred <- pred
  class(res) <- c('crisp', 'spdur')
  
  return(res)
}