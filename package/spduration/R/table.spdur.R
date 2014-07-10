#' Create a simple table of split-population duration results
#'
#' \code{table} method for class ``\code{spdur}''.
#' 
#' @method table spdur
#' 
#' @param object An object with class \code{spdur}.
#' @param row.names Indicates whether parameter names should be added as row
#' names to the data frame returned, or as a separate column with blank row 
#' row names. 
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
#' tbl <- table(model.coups)
#' tbl
#' 
#' @S3method table spdur

table.spdur <- function(model, row.names=TRUE) {
  # Format spdur estimates summary for pretty printing with xtable
  # if row.names=FALSE parameter names will be column in results
  sum.tbl <- summary(model)
  dur   <- sum.tbl$duration[, c(1, 4), drop=FALSE]
  risk  <- sum.tbl$split[, c(1, 4), drop=FALSE]
  alpha <- sum.tbl$alpha[, c(1, 4), drop=FALSE]
  
  # Fix for duplicated rownames, which won't allow merge
  duprows <- rownames(risk) %in% rownames(dur)
  if (any(duprows)) {
    rownames(risk)[which(duprows)] <- paste(rownames(risk)[which(duprows)], "1", sep=".")
  }
  res <- data.frame(rbind(dur, alpha, risk))

  if (row.names) {
    return(res)
  } else {
    res$par <- rownames(res)
    rownames(res) <- NULL
    return(res)
  }
}