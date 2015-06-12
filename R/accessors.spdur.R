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
#' @importFrom stats AIC
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
#' @importFrom stats BIC
BIC.spdur <- function(object, ...)
{
  bic <- AIC(object, k = log(nobs(object)))
  
  return(bic)
}

#' Log-Likelihood of an spdur Object
#' 
#' Returns the log-likelihood of a \code{spdur} class object.
#' 
#' @method logLik spdur
#' 
#' @param object an object inheriting from class \code{spdur}.
#' @param \dots not used
#' 
#' @examples
#' data(model.coups)
#' logLik(model.coups)
#' 
#' @export
#' @importFrom stats logLik
logLik.spdur <- function(object, ...) 
{
  p <- nobs(object) - length(object$coefficients)
  val <- object$logL
  attr(val, "nobs") <- nobs(object)
  attr(val, "df")  <- p
  class(val) <- 'logLik'  
  
  return(val)
}

#' Number of Observations in a spdur Object
#' 
#' Extract the number of observations in a \code{spdur} class model object.
#' 
#' @param object an object inheriting from class \code{spdur}.
#' @param \dots not used.
#' 
#' @seealso \code{\link{AIC.spdur}}, \code{\link{BIC.spdur}}
#' 
#' @export
#' @importFrom stats nobs
nobs.spdur <- function(object, ...)
{
  val <- object$obs
  return(val)
}