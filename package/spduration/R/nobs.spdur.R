#' @S3method nobs spdur
#' @importFrom stats nobs
nobs.spdur <- function(object, ...)
{
  val <- object$obs
  return(val)
}