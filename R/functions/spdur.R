##########
# What: spdur R package core functions
# Date: November 2012
# Who:  Andreas Beger, Daniel W. Hill, Nils Metternich
#
###########

spdur <- function(duration, atrisk, data=NULL, last, distr='weibull', max.iter=100, ...) { 
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
  
  # Other class elements
  est$call <- match.call()
  est$distr <- eval.parent(distr, 1)
  est$obs <- nrow(Y)
  
  class(est) <- 'spdur'
  return(est)
}

summary.spdur <- function(object, ...) {
  # Find index to separate 2 equations
  start_split <- which(names(object$coefficients)=='(Intercept)')[2]
  end_duration <- start_split - 1
  
  table <- cbind(Estimate = coef(object),
                 StdErr = object$se,
                 t = object$zstat,
                 p = object$pval)
  
  duration_table <- table[1:end_duration, ]
  split_table <- table[start_split:(nrow(table)-1), ]
  
  res <- list(call = object$call,
              duration = duration_table,
              split = split_table,
              alpha = table[nrow(table), , drop=FALSE])
  class(res) <- 'summary.spdur'
  return(res)
}

print.summary.spdur <- function(x, ...)
{
  cat('Call:\n')
  print(x$call)
  cat('\n')
  cat('Duration equation: \n')
  printCoefmat(x$duration, P.values=T, has.Pvalue=T, digits=4, zap.ind=4, signif.legend=F)
  cat('\n')
  cat('Risk equation: \n')
  printCoefmat(x$split, P.values=T, has.Pvalue=T, digits=4, zap.ind=4, signif.legend=F)
  cat('\n')
  printCoefmat(x$alpha, P.values=T, has.Pvalue=T, digits=4, zap.ind=4, signif.legend=F)
  cat('---\n')
  cat("Signif. codes: *** = 0.001, ** = 0.01, * = 0.05, . = 0.1")
}

logLik.spdur <- function(object, ...) 
{
  p <- nobs(object) - length(object$coefficients)
  val <- object$logL
  attr(val, "nobs") <- nobs(object)
  attr(val, "df")  <- p
  class(val) <- 'logLik'  
  
  return(val)
}

nobs.spdur <- function(object, ...)
{
  val <- object$obs
  return(val)
}

AIC.spdur <- function(object, ..., k = 2)
{
  npar <- length(object$coefficients)
  lnL <- logLik(object)
  aic <- as.vector(-2*lnL + k * npar)
  
  return(aic)
}

BIC.spdur <- function(object, ...)
{
  bic <- AIC(object, k = log(nobs(object)))
  
  return(bic)
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
                         stat = 'conditional risk', iter = 100, npred=6, ...) {
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
  pred.p <- forecast(model, pred.data=pred, stat=stat, npred=npred)
  
  # Format and show estimates
  print(summary(model))
  
  res <- model
  res$train.p <- train.p
  res$test.p <- test.p
  res$pred.p <- pred.p
  class(res) <- c('crisp', 'spdur')
  
  return(res)
}