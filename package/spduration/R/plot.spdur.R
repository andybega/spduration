#' Plot split-duration model fit.
#' 
#' \code{plot} method for class ``\code{spdur}''.
#' 
#' @method plot spdur
#' 
#' @param x An object of class "\code{spdur}".
#' @param \dots Optional parameters passed to \code{predict.spdur}, e.g. type 
#' of statistic to calculate.
#' @param failure The variable indicating that a failure event has occurred at 
#' time \code{t}.
#' @param endSpellOnly Should only the last observation in each spell be kept? 
#' \code{TRUE} by default.
#' 
#' @details Creates a \code{\link{separationplot}} of fitted values from 
#' split-duration model results using \code{\link{predict.spdur}}.
#' 
#' @author Andreas Beger
#' 
#' @seealso \code{\link{separationplot}}, \code{\link{predict.spdur}}
#' @examples
#' # build data
#' data(insurgency)
#' duration.ins <- buildDuration(insurgency, 'insurgency', unitID='ccode', 
#'                               tID='date')
#' duration.ins <- duration.ins[!is.na(duration.ins$failure), ]
#' 
#' # get model estimates
#' data(model.ins)
#' 
#' # plot
#' p <- plot(model.ins)
#' 
#' @S3method plot spdur
#' @importFrom separationplot separationplot
plot.spdur <- function(x, ..., failure='failure', endSpellOnly=TRUE)
{
  require(separationplot)
  
  # Input validation
  if (!'spdur' %in% class(x)) stop('"object" argument must have class "spdur"')
  if (!'failure' %in% colnames(get(paste(x$call$data)))) stop(paste(failure, 'not in data'))
  
  # Get predicted/observed values
  pred <- as.vector(as.matrix(predict(x, ...)))
  actual <- get(paste(x$call$data))[, failure]
  
  # Keep end of spell only
  if (endSpellOnly==T) {
    pred <- pred[ get(paste(x$call$data))[, x$call$last]==1 ]
    actual <- actual[ get(paste(x$call$data))[, x$call$last]==1 ]
  }
  
  # Separationplot call
  plot <- separationplot(pred, actual,
                         shuffle=T, heading='', show.expected=T, newplot=F, 
                         type='line', lwd1=5, lwd2=2)
}