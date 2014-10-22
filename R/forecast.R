#' Model forecasts
#' 
#' \code{forecast} is a generic function for creating out-of-sample predictions.
#' It invokes particular \emph{methods} which depend on the \code{\link{class}}
#' of the first argument
#' 
#' @param object A model object from which forecasts are created.
#' @param \dots additional arguments affecting the forecasts produced.
#' 
#' @details This generic is implemented in the \code{\link{spduration}} package
#' with a \code{\link{plot.spdur}} method.
#' 
#' @export 
forecast <- function(object, ...) { UseMethod('forecast') }