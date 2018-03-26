#' Split-population duration (cure) regression
#' 
#' This function estimates a split-population duration model and returns a 
#' object of class \code{spdur}.
#' 
#' @param duration A formula of the form Y ~ X1 + X2 \dots, where Y is duration 
#' until failure or censoring.
#' @param atrisk A formula of the form C ~ Z1 + Z2 \dots, where C is a binary 
#' indicator of risk (1 - cure).
#' @param data A data frame containing the variables in formula and formula2.
#' @param last A string identifying the vector in \code{data} that indicates 
#' when a spell ends due to failure or right-censoring.
#' @param t.0 The starting point for time-varying covariate intervals, by 
#' default \code{duration-1} when using \code{\link{add_duration}}.
#' @param fail Name of the variable indicating that a spell ended in failure.
#' @param distr The type of distribution to use in the hazard rate. Valid 
#' options are ``weibull'' or ``loglog''; defaults to ``weibull''. 
#' @param max.iter Maximum number of iterations to use in the likelihood 
#' maximization.
#' @param na.action a function which indicates what should happen when the data 
#' contain NAs. The default is set by the \code{na.action} setting of options, 
#' and is \code{\link{na.fail}} if that is unset.
#' @param silent Suppress optimization output, \code{FALSE} by default.
#' @param \dots Optional arguments, see details.
#' 
#' @details 
#' See \code{\link{summary.spdur}}, \code{\link{predict.spdur}} ,
#' and \code{\link{plot.spdur}} for post-estimation options.
#' 
#' Optional arguments:
#' \describe{
#' \item{base.inits}{Initial values for the base duration model that is 
#' estimated to get initial values for the full split-population model. This
#' needs to be a vector with starting values for the constant, coefficients
#' in the duration equation, and an additional value for the shape parameter
#' of the density used, e.g. Weibull. By default they are 0 for all 
#' coefficients and 0 or 1 for the Weibull and LogLog shape parameters 
#' respectively.}
#' }
#' 
#' @return Returns an object of class \code{spdur}, with attributes:
#' \item{coefficients }{A named vector of coefficient point estimates.}
#' \item{vcv }{Estimated covariance matrix.}
#' \item{se }{Standard error estimates.}
#' \item{zstat }{Z-statistic values.}
#' \item{pval }{P-values.}
#' \item{mf.dur }{Model frame for the duration equation.}
#' \item{mf.risk }{Model frame for the risk equation.}
#' \item{Y }{Matrix of duration variables: risk, duration, end of spell, and
#' t.0.}
#' \item{na.action }{What action was taken for missing values in \code{data}.}
#' \item{call }{The original, unevaluated \code{spdur} call.}
#' \item{distr }{Distribution used for the hazard rate.}
#' 
#' @examples
#' # Prepare data
#' data(coups)
#' dur.coups <- add_duration(coups, "succ.coup", unitID="gwcode", tID="year",
#'                            freq="year")
#'
#' # Estimate model
#' model.coups <- spdur(duration ~ polity2, atrisk ~ polity2, data=dur.coups)
#' model.coups <- spdur(duration ~ polity2, atrisk ~ polity2, data=dur.coups,
#'                      distr="loglog")
#' 
#' @export spdur

spdur <- function(duration, atrisk, data=NULL, last="end.spell", t.0="t.0", 
                  fail="failure", distr=c("weibull", "loglog"), max.iter=300, na.action,
                  silent=FALSE, ...) 
{ 
  cl <- match.call()
  distr <- match.arg(distr)
  
  # NA actions, separately because model.frame for two different equations
  # might return data frames with different row numbers, breaking the function.
  f1 <- as.formula(eval(duration))
  f2 <- as.formula(eval(atrisk))
  vars <- unique(c(all.vars(f1), all.vars(f2)))
  vars <- c(vars, last, t.0, fail)
  if (missing(na.action)) {
    na.action <- options("na.action")[[1]]
  }
  df <- do.call(na.action, list(data[, vars]))
  # the only way to get to this point is na.pass; easier than trying to check
  # if na.action is identical to na.pass function
  if (any(is.na(df))) stop("na.pass is not supported")
  
  # Duration equation
  mf.dur <- eval(model.frame(formula = duration, data=df), parent.frame())
  X <- model.matrix(attr(mf.dur, 'terms'), data=mf.dur)
  attr(X, "na.action") <- na.action(df)
  lhb <- model.response(mf.dur)
  k.dur <- ncol(X)  # mark number of terms in dur. eq. for summary()
  
  # Risk/non-immunity equation
  mf.risk <- eval(model.frame(formula=atrisk, data=df), parent.frame())
  Z <- model.matrix(attr(mf.risk, 'terms'), data=mf.risk)
  attr(Z, "na.action") <- na.action(df)
  lhg <- model.response(mf.risk)
  k.risk <- ncol(Z)  # mark number of terms in risk. eq. for summary()
  
  # Y vectors
  Y <- cbind(atrisk=lhg, duration=lhb, last=df[, last], t.0=df[, t.0], 
             fail=df[, fail])
  attr(Y, "last") <- last
  attr(Y, "t.0") <- t.0
  attr(Y, "fail") <- fail
  
  # Estimation
  if (distr=='weibull') {
    fit <- spweibull(Y, X, Z, max.iter, silent=silent, ...)
  }
  if (distr=='loglog') {
    fit <- sploglog(Y, X, Z, max.iter, silent=silent, ...)
  }
  # Names
  varnames <- c(paste(unlist(attr(X, 'dimnames')[2])), paste(unlist(attr(Z, 'dimnames')[2])), 'log(alpha)')
  attr(fit$coefficients, 'names') <- varnames
  colnames(fit$vcv) <- rownames(fit$vcv) <- varnames
  
  coef.dur  <- fit$coefficients[c(1:k.dur)] 
  coef.risk <- fit$coefficients[(k.dur + 1):(k.dur + k.risk)]
  coef.distr <- fit$coefficients[(k.dur + k.risk + 1)]

  fit$coefficients <- list(duration = coef.dur, risk = coef.risk, distr = coef.distr)
  
  # Calculate uncertainty measures
  se    <- sqrt(diag(fit$vcv))
  zstat <- c(fit$coefficients$duration, fit$coefficients$risk, fit$coefficients$distr)/se
  pval <- 2*(1-pnorm(abs(zstat)))
  
  res <- list(
    coefficients = fit$coefficients,
    vcov = fit$vcv,
    logL = fit$logL,
    base = fit$base,
    se = se, zstat = zstat, pval = pval,
    mf.dur = mf.dur,
    mf.risk = mf.risk,
    Y = Y,
    na.action = attr(df, "na.action"),
    call = cl,
    formula = NULL,
    terms = NULL,
    distr = eval.parent(distr, 1),
    obs = nrow(Y),
    n.terms = list(duration = k.dur, risk = k.risk)
    )
  
  class(res) <- 'spdur'
  return(res)
}