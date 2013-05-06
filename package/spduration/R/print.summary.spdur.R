#' @S3method print summary.spdur
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
}