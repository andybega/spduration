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
    object$coefficients$distr,
    object$se[length(object$se)],
    object$zstat[length(object$se)],
    object$pval[length(object$se)]
  )
  
  colnames(tbl_dur) <- colnames(tbl_risk) <- colnames(tbl_alpha) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  
  res <- list(call = object$call,
              duration = tbl_dur,
              split = tbl_risk,
              alpha = tbl_alpha)
  class(res) <- 'summary.spdur'
  return(res)
}

#' Print a split-population duration model results summary
#'
#' \code{print} method for class ``\code{summary.spdur}''.
#' 
#' @method print summary.spdur
#' 
#' @param x An object with class \code{spdur}.
#' @param \dots Further arguments passed to or from other methods.
#' 
#' @details
#' Formats \code{spdur} summaries for printing.
#' 
#' @seealso The model fitting function is \code{\link{spdur}}, and see 
#' \code{\link{summary.spdur}} for associated summary method.
#' 
#' @examples
#' data(model.coups)
#' s <- summary(model.coups)
#' class(s)
#' print(s)
#' 
#' @importFrom stats printCoefmat
#' @method print summary.spdur
#' @export
print.summary.spdur <- function(x, ...)
{
  cat('Call:\n')
  print(x$call)
  cat('\n')
  cat('Duration equation: \n')
  printCoefmat(x$duration, P.values=T, has.Pvalue=T, digits=4, zap.ind=4, signif.legend=F)
  cat('\n')
  cat('Risk equation: \n')
  printCoefmat(x$split, P.values=T, has.Pvalue=T, digits=4, zap.ind=4, signif.legend=F)
  cat('\n')
  printCoefmat(x$alpha, P.values=T, has.Pvalue=T, digits=4, zap.ind=4, signif.legend=F)
  cat('---\n')
  cat("Signif. codes: *** = 0.001, ** = 0.01, * = 0.05, . = 0.1")
  cat("\n")
}