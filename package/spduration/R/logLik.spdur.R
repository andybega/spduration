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
#' @export logLik spdur
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