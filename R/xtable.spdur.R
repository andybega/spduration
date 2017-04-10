#' Create export table for a split-duration model
#'
#' \code{xtable}-like function for class ``\code{spdur}''. 
#' 
#' @param x An object with class \code{spdur}.
#' @param \dots Further arguments passed to \code{\link[xtable]{xtable}}.
#' 
#' @details
#' Format a split-duration model for export to Latex or html. 
#' 
#' @return
#' An object with class \code{xtable}.
#' 
#' @seealso \code{\link[xtable]{xtable}}, or \code{\link{as.data.frame.spdur}} for a
#'   simpler alternative that will convert a \code{spdur} object to a data frame
#'   containing model parameter estimates.
#' 
#' For print formatting, see \code{\link[xtable]{print.xtable}}.
#' 
#' @examples
#' library(xtable)
#' data(model.coups)
#' xtable(model.coups)
#' print(xtable(model.coups), include.rownames=FALSE)
#' 
#' @export
#' @import xtable

xtable.spdur <- function(x, ...) {

  model_table <- as.data.frame(x, row.names=FALSE)
  res <- xtable::xtable(model_table, ...)
  res
}