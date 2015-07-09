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