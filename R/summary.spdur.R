#' Summarize split-population duration results
#'
#' \code{summary} method for class ``\code{spdur}''.
#' 
#' @method summary spdur
#' 
#' @param object An object with class \code{spdur}.
#' @param \dots Further arguments passed to or from other methods.
#' 
#' @details
#' This will list the estimated coefficients and standard errors for the
#' risk and duration equations of a split-population duration model.
#' 
#' @return
#' An object with class \code{summary.spdur}.
#' 
#' @seealso The model fitting function is \code{\link{spdur}}, and see 
#' \code{\link{summary}} for the generic function.
#' 
#' For print formatting, see \code{\link{print.summary.spdur}}.
#' 
#' @examples
#' data(model.coups)
#' s <- summary(model.coups)
#' class(s)
#' print(s)
#' 
#' @export
#' @importFrom stats coef
summary.spdur <- function(object, ...) {
  
  dur_idx  <- 1:object$n.terms$duration
  risk_idx <- (object$n.terms$duration + 1):(object$n.terms$duration + object$n.terms$risk)
  
  tbl_dur <- cbind(
    object$coefficients$duration[dur_idx],
    object$se[dur_idx],
    object$zstat[dur_idx],
    object$pval[dur_idx]
  )
  
  tbl_risk <- cbind(
    object$coefficients$risk,
    object$se[risk_idx],
    object$zstat[risk_idx],
    object$pval[risk_idx]
  )
  
  tbl_alpha <- cbind(
    object$coefficients$duration[length(object$coefficients$duration)],
    object$se[length(object$se)],
    object$zstat[length(object$se)],
    object$pval[length(object$se)]
  )
  
  colnames(tbl_dur) <- colnames(tbl_risk) <- colnames(tbl_alpha) <- c("Estimate", "StdErr", "t", "p")
  
  res <- list(call = object$call,
              duration = tbl_dur,
              split = tbl_risk,
              alpha = tbl_alpha)
  class(res) <- 'summary.spdur'
  return(res)
}