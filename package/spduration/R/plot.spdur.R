#' Plot split-duration model fit.
#' 
#' \code{plot} method for class ``\code{spdur}''.
#' 
#' @method plot spdur
#' 
#' @param model An object of class "\code{spdur}".
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
#' # get model estimates
#' data(model.coups)
#' 
#' # plot
#' p <- plot(model.coups)
#' 
#' @S3method plot spdur
#' @importFrom separationplot separationplot
plot.spdur <- function(model, ..., failure='failure', endSpellOnly=TRUE)
{
  require(separationplot)
  
  # Input validation
  if (!'spdur' %in% class(model)) stop('"object" argument must have class "spdur"')
  
  # Get data
  # need to do something with napredict to make this more flexible
  data <- get(paste(model$call$data))
  if (class(na.action(model))=="omit") data <- data[-na.action(model), ]
  if (!'failure' %in% colnames(data)) stop(paste(failure, 'not in data'))
  
  # Get predicted/observed values
  pred <- as.vector(as.matrix(predict(model, ...)))
  actual <- data[, failure]
  
  # Keep end of spell only
  if (endSpellOnly==T) {
    pred <- pred[ data[, attr(model$Y, "last")]==1 ]
    actual <- actual[ data[, attr(model$Y, "last")]==1 ]
  }
  
  # Separationplot call
  plot <- separationplot(pred, actual,
                         shuffle=T, heading='', show.expected=T, newplot=F, 
                         type='line', lwd1=5, lwd2=2)
}