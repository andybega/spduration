#' AIC method for spdur
#' 
#' Computes the Akaike Information Criterion for an \code{spdur} class object.
#' 
#' @method AIC spdur
#' 
#' @param object An object of class \code{spdur}.
#' @param k The penalty parameter, by default 2. For \code{\link{BIC.spdur}},
#' the penalty parameter equals \code{log(N)}.
#'
#' author Andreas Beger
#' 
#' @seealso \code{link{AIC}}, \code{link{BIC.spdur}}
#' 
#' @examples
#' data(model.ins)
#' AIC(model.ins)
#'
#' @S3method AIC spdur
#' @importFrom stats AIC
AIC.spdur <- function(object, k = 2)
{
  npar <- length(object$coefficients)
  lnL <- logLik(object)
  aic <- as.vector(-2*lnL + k * npar)
  
  return(aic)
}