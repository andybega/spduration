## Separationplot method (not really a method) for spdur
# Input: parameters, data, and sims to run
# Output: row-matrix of predicted probability quantiles
#
plot.spdur <- function(object, failure='failure', endSpellOnly=FALSE, ...)
{
  require(separationplot)
  
  # Input validation
  if (!'spdur' %in% class(object)) stop('"object" argument must have class "spdur"')
  if (!'failure' %in% colnames(get(paste(object$call$data)))) stop(paste(failure, 'not in data'))
  
  # Get predicted/observed values
  pred <- as.vector(as.matrix(predict(object)))
  actual <- get(paste(object$call$data))[, failure]
  
  # Keep end of spell only
  if (endSpellOnly==T) {
    pred <- pred[ get(paste(object$call$data))[, object$call$last]==1 ]
    actual <- actual[ get(paste(object$call$data))[, object$call$last]==1 ]
  }
  
  # Separationplot call
  plot <- separationplot(pred, actual,
                         shuffle=T, heading='', show.expected=T, newplot=F, 
                         type='line', lwd1=5, lwd2=2)
}