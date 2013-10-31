#' Number of Observations in a spdur Object
#' 
#' Extract the number of observations in a \code{spdur} class model object.
#' 
#' @param object an object inheriting from class \code{spdur}.
#' @param \dots not used.
#' 
#' @seealso \code{\link{AIC.spdur}}, \code{\link{BIC.spdur}}
#' 
#' @S3method nobs spdur
#' @importFrom stats nobs
nobs.spdur <- function(object, ...)
{
  val <- object$obs
  return(val)
}