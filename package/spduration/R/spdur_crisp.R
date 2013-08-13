#' Split-Pop Duration Model Wrapper for CRISP/ICEWS
#' 
#' This is a wrapper for the CRISP and ICEWS packages that replicated the 
#' original functionality of the \code{spdur} command by estimating a model, 
#' calculating training and validation set fitted values and 6-month forecasts.
#' 
#' @param duration Duration model formula.
#' @param atrisk Risk model formula.
#' @param train Object containing the training data used to estimate the model.
#' @param test Object containing the test data used to validate the model.
#' @param pred Object containing the data to use for forecasting.
#' @param last
#' @param t.0 Variable indicating the start time for an observation. By default
#' duration-1.
#' @param distr Distribution for the hazard rate, either "weibull" or 
#' "lognormal".
#' @param stat See \code{\link[spduration]{predict.spdur}}
#' @param iter Maximum iterations to use in optimization.
#' @param npred Number of months to predict out.
#' @param \dots Optional arguments; not used currently.
#' 
#' @return Returns an object of class \code{crisp}, which inherits from 
#' \code{spdur}. 
#' 
#' @author Andreas Beger
#' @seealso \code{\link{spdur}}
#' @example
#' # Prepare data
#' data(insurgency)
#' duration.ins <- buildDuration(insurgency, 'insurgency', unitID='ccode', tID='date')
#' 
#' # Estimate model
#' model3 <- spdur.crisp(
#' duration ~ low_intensity + high_neighbors + exclpop.l1,
#' atrisk ~ excl_groups_count.l1 + high_neighborhood + high_intensity + exclpop.l1 + lgdppc.l1,
#' last='end.spell', train=duration.ins, test=duration.ins[1,], pred=duration.ins[1,], distr="weibull", iter=300)
#' 
#' @export


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
