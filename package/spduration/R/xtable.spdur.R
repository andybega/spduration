#' Create export table for a split-duration model
#'
#' \code{xtable} method for class ``\code{spdur}''.
#' 
#' @method xtable spdur
#' 
#' @param object An object with class \code{spdur}.
#' @param \dots Further arguments passed to \code{xtable}.
#' 
#' @details
#' Format a split-duration model for export to Latex or html. 
#' 
#' @return
#' An object with class \code{xtable}.
#' 
#' @seealso \code{\link{xtable}}, or \code{\link{table.spdur}} for a simpler
#' alternative that will convert a \code{spdur} object to a data frame
#' containing model parameter estimates. 
#' 
#' For print formatting, see \code{\link{print.xtable}}.
#' 
#' @examples
#' data(model.coups)
#' xtable(model.coups)
#' 
#' @S3method xtable spdur

xtable.spdur <- function(model, ...) {
	if (!require(xtable))
        stop("Please install xtable: install.packages('xtable')")

  model.sum <- summary(model)
  model.tbl <- rbind(model.sum$duration, model.sum$alpha, model.sum$split)
  res <- xtable(model.tbl[, c("Estimate", "StdErr", "p")], ...)
  res
}