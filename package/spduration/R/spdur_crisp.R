#' @export spdur_crisp
##########
# spdur-crisp
# Top level wrapper
#
# in:  
#
# to do:
# - clean up code
#
###########
spdur_crisp <- function (duration, atrisk, train = NULL, test = NULL, 
                         pred = NULL, last = NULL, t.0="t.0", distr = NULL, 
                         stat = 'conditional risk', iter = 100, npred=6, ...) {
  if (is.null(data)) stop("No data provided")
  if (is.null(last)) stop("Must specify censoring variable")
  if (is.null(distr)) stop("Must specify distribution")
  
  # Estimate parameters
  model <- spdur(duration=duration, atrisk=atrisk, data=train, last=last, distr=distr, max.iter=iter)
  
  # In-sample and test predictions
  cat('Training set predictions...\n')
  train.p <- predict(model, stat=stat)
  cat('Validation set predictions...\n')
  test.p <- predict(model, data=test, stat=stat)
  
  # Forecast
  cat('Forecast...\n')
  pred.p <- forecast(model, pred.data=pred, stat=stat, npred=npred)
  
  # Format and show estimates
  print(summary(model))
  
  res <- model
  res$train.p <- train.p
  res$test.p <- test.p
  res$pred.p <- pred.p
  class(res) <- c('crisp', 'spdur')
  
  return(res)
}
