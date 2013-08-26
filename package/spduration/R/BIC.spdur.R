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
#' @author Andreas Beger
#' 
#' @seealso \code{\link{BIC}}, \code{\link{AIC.spdur}}
#' 
#' @examples
#' data(model.ins)
#' BIC(model.ins)
#' 
#' @S3method BIC spdur
#' @importFrom stats BIC
BIC.spdur <- function(object, ...)
{
  bic <- AIC(object, k = log(nobs(object)))
  
  return(bic)
}