##########
# What: spdur R package core functions
# Date: November 2012
# Who:  Andreas Beger, Daniel W. Hill, Nils Metternich
#
###########

spdur <- function(duration, atrisk, data=NULL, last, distr='weibull', max.iter=100, ...)
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
  Y <- cbind(atrisk=lhg, duration=lhb, last=data[, last])
  
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
  est$distr <- eval.parent(distr, 1)
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
  
  printCoefmat(x$coefficients, P.values=T, has.Pvalue=T, digits=4)
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
spdur.crisp <- function (duration, atrisk, train = NULL, test = NULL, 
                         pred = NULL, last = NULL, distr = NULL, 
                         stat = 'atrisk', iter = 100, npred=6, ...) {
  if (is.null(data)) stop("No data provided")
  if (is.null(last)) stop("Must specify censoring variable")
  if (is.null(distr)) stop("Must specify distribution")
  
  # Estimate parameters
  model <- spdur(duration=duration, atrisk=atrisk, data=train, last=last, distr=distr, max.iter=iter)
  
  # In-sample and test predictions
  cat('Training set predictions...\n')
  train.p <- predict(model, stat=stat)
  cat('Validation set predictions...\n')
  test.p <- predict(model, data=test, stat=stat)
  
  # Forecast
  cat('Forecast...\n')
  pred.p <- forecast(model, pred, stat=stat, npred=npred)
  
  # Format and show estimates
  print(summary(model))
  
  res <- model
  res$train.p <- train.p
  res$test.p <- test.p
  res$pred.p <- pred.p
  class(res) <- c('crisp', 'spdur')
  
  return(res)
}