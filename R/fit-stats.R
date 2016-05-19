#' AIC method for spdur
#' 
#' Computes the Akaike Information Criterion for an \code{spdur} class object.
#' 
#' @method AIC spdur
#' 
#' @param object An object of class \code{spdur}.
#' @param \dots Optional arguments.
#' @param k The penalty parameter, by default 2. For \code{\link{BIC.spdur}},
#' the penalty parameter equals \code{log(N)}.
#' 
#' @seealso \code{link{AIC}}, \code{link{BIC.spdur}}
#' 
#' @examples
#' data(model.coups)
#' AIC(model.coups)
#'
#' @export 
#' @import stats
AIC.spdur <- function(object, ..., k = 2)
{
  npar <- length(object$coefficients)
  lnL <- logLik(object)
  aic <- as.vector(-2*lnL + k * npar)
  
  return(aic)
}

#' BIC method for spdur
#' 
#' Computes the Bayesian Information Criterion for an \code{spdur} class object.
#' 
#' @method BIC spdur
#' 
#' @param object An object of class \code{spdur}.
#' @param \dots Optional arguments.
#' 
#' @details Computed as \code{AIC(object, k = log(nobs(object)))}.
#' 
#' @seealso \code{\link{BIC}}, \code{\link{AIC.spdur}}
#' 
#' @examples
#' data(model.coups)
#' BIC(model.coups)
#' 
#' @export
BIC.spdur <- function(object, ...)
{
  bic <- AIC(object, k = log(nobs(object)))
  
  return(bic)
}