#' Convert spdur results to summary data frame
#'
#' \code{table}-like function for class ``\code{spdur}''.
#' 
#' @param x An object with class \code{spdur}.
#' @param row.names Indicates whether parameter names should be added as row
#' names to the data frame returned, or as a separate column with blank row 
#' row names.
#' @param optional Not used 
#' @param \dots Not used.
#' 
#' @details
#' This will create a data frame containing the estimated coefficients and 
#' standard errors for the risk and duration equations of a split-population 
#' duration model. It's intented purpose is to help create larger tables
#' combining several model results.
#' 
#' @return
#' An data frame with model coefficients and p-values. 
#' 
#' @seealso \code{\link{xtable.spdur}} for formatting a single model to 
#' Latex output. 
#' 
#' @examples
#' data(model.coups)
#' data.frame(model.coups)
#' 
#' @method as.data.frame spdur
#' @export

as.data.frame.spdur <- function(x, row.names = TRUE, optional = FALSE, ...) {
  # Format spdur estimates summary for pretty printing with xtable
  # if row.names=FALSE parameter names will be column in results
  sum.tbl <- summary(x)
  want_rows <- c(1, 2, 3, 4)
  dur   <- sum.tbl$duration[, want_rows, drop=FALSE]
  risk  <- sum.tbl$split[, want_rows, drop=FALSE]
  alpha <- sum.tbl$alpha[, want_rows, drop=FALSE]
  
  # Id equation for row names, also fixes duplicate rownames issue
  rownames(dur) <- paste0("Dur_", rownames(dur))
  rownames(risk) <- paste0("Risk_", rownames(risk))
  
  res <- data.frame(rbind(dur, alpha, risk), check.names=FALSE)

  if (row.names) {
    return(res)
  } else {
    res <- data.frame(Parameter=rownames(res), res, check.names=FALSE)
    rownames(res) <- NULL
    return(res)
  }
}