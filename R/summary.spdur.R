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
  # Find index to separate 2 equations
  start_split <- which(names(object$coefficients)=='(Risk Intercept)')
  end_duration <- start_split - 1
  
  table <- cbind(Estimate = coef(object),
                 StdErr = object$se,
                 t = object$zstat,
                 p = object$pval)
  
  duration_table <- table[1:end_duration, , drop=FALSE]
  split_table <- table[start_split:(nrow(table)-1), , drop=FALSE]
  
  res <- list(call = object$call,
              duration = duration_table,
              split = split_table,
              alpha = table[nrow(table), , drop=FALSE])
  class(res) <- 'summary.spdur'
  return(res)
}