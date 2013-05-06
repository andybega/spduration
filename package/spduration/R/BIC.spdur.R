#' @S3method BIC spdur
#' @importFrom stats BIC
BIC.spdur <- function(object, ...)
{
  bic <- AIC(object, k = log(nobs(object)))
  
  return(bic)
}