## Separationplot method (not really a method) for spdur
# Input: parameters, data, and sims to run
# Output: row-matrix of predicted probability quantiles
#
plot.spdur <- function(x, ..., failure='failure', endSpellOnly=FALSE)
{
  require(separationplot)
  
  # Input validation
  if (!'spdur' %in% class(x)) stop('"object" argument must have class "spdur"')
  if (!'failure' %in% colnames(get(paste(x$call$data)))) stop(paste(failure, 'not in data'))
  
  # Get predicted/observed values
  pred <- as.vector(as.matrix(predict(x)))
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