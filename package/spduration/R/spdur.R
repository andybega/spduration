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
#' default \code{duration-1} when using \code{\link{buildDuration}}.
#' @param distr The type of distribution to use in the hazard rate. Valid 
#' options are ``weibull'' or ``loglog''.
#' @param max.iter Maximum number of iterations to use in the likelihood 
#' maximization.
#' @param \dots Additional parameters passed to \code{spdur()}
#' 
#' @details See \code{\link{summary.spdur}}, \code{\link{predict.spdur}} ,
#' \code{\link{plot.spdur}}, and \code{\link{countryplot}} for post-estimation
#' options.
#' 
#' @return Returns an object of class \code{spdur}, with attributes:
#' \item{coefficients }{A named vector of coefficient point estimates.}
#' \item{vcv }{Estimated covariance matrix.}
#' \item{se }{Standard error estimates.}
#' \item{zstat }{Z-statistic values.}
#' \item{pval }{P-values.}
#' \item{call }{The original, unevaluated \code{spdur} call.}
#' \item{distr }{Distribution used for the hazard rate.}
#' 
#' @author Andreas Beger
#' 
#' @examples
#' \dontrun{
#' # Prepare data
#' data(insurgency)
#' duration.ins <- buildDuration(insurgency, 'insurgency', unitID='ccode', tID='date')
#' duration.ins <- duration.ins[!is.na(duration.ins$failure), ]
#'
#' # Estimate model
#' model <- spdur(
#'   duration ~ low_intensity + high_neighbors + exclpop.l1,
#'   atrisk ~ excl_groups_count.l1 + high_neighborhood + high_intensity + exclpop.l1 + 
#'     lgdppc.l1,
#'   last='end.spell', data=duration.ins, distr="weibull", max.iter=300)
#' }
#' 
#' @export spdur

spdur <- function(duration, atrisk, data=NULL, last="end.spell", t.0="t.0", 
                  distr='weibull', max.iter=300, ...) { 
  # Duration equation
  mf.dur <- model.frame(formula=duration, data=data)
  X <- model.matrix(attr(mf.dur, 'terms'), data=mf.dur)
  lhb <- model.response(mf.dur)
  # Risk/non-immunity equation
  mf.risk <- model.frame(formula=atrisk, data=data)
  Z <- model.matrix(attr(mf.risk, 'terms'), data=mf.risk)
  lhg <- model.response(mf.risk)
  # Y vectors
  Y <- cbind(atrisk=lhg, duration=lhb, last=data[, last], t.0=data[, t.0])
  
  # Estimation
  if (distr=='weibull') {
    est <- spweibull(Y, X, Z, max.iter)
  }
  if (distr=='loglog') {
    est <- sploglog(Y, X, Z, max.iter)
  }
  # Names
  varnames <- c(paste(unlist(attr(X, 'dimnames')[2])), paste(unlist(attr(Z, 'dimnames')[2])), 'log(alpha)')
  attr(est$coefficients, 'names') <- varnames
  colnames(est$vcv) <- rownames(est$vcv) <- varnames
  
  # Calculate uncertainty measures
  est$se <- sqrt(diag(est$vcv))
  est$zstat <- with(est, coefficients/se)
  est$pval <- 2*(1-pnorm(abs(est$zstat)))
  
  # Other class elements
  est$call <- expand.call(definition=spdur)  # instead of match.call to catch 
                                             # default parameters
  est$distr <- eval.parent(distr, 1)
  est$obs <- nrow(Y)
  
  class(est) <- 'spdur'
  return(est)
}