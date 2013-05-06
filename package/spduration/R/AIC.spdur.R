#' @S3method AIC spdur
#' @importFrom stats AIC
AIC.spdur <- function(object, ..., k = 2)
{
  npar <- length(object$coefficients)
  lnL <- logLik(object)
  aic <- as.vector(-2*lnL + k * npar)
  
  return(aic)
}