#' Split-Pop Duration Model Wrapper for CRISP/ICEWS
#' 
#' This is a wrapper for the CRISP and ICEWS packages that replicated the 
#' original functionality of the \code{spdur} command by estimating a model, 
#' calculating training and validation set fitted values and 6-month forecasts.
#' 
#' @param duration A formula of the form Y ~ X1 + X2 \dots, where Y is duration 
#' until failure or censoring.
#' @param atrisk A formula of the form C ~ Z1 + Z2 \dots, where C is a binary 
#' indicator of risk (1 - cure).
#' @param train Object containing the training data used to estimate the model.
#' @param test Object containing the test data used to validate the model.
#' @param pred Object containing the data to use for forecasting.
#' @param last A string identifying the vector in \code{data} that indicates 
#' when a spell ends due to failure or right-censoring.
#' @param t.0 The starting point for time-varying covariate intervals, by 
#' default \code{duration-1} when using \code{\link{add_duration}}.
#' @param distr The type of distribution to use in the hazard rate. Valid 
#' options are ``weibull'' or ``loglog''.
#' @param stat See \code{\link[spduration]{predict.spdur}}
#' @param iter Maximum number of iterations to use in the likelihood 
#' maximization.
#' @param npred Number of months to predict out.
#' @param \dots Optional arguments; not used currently.
#' 
#' @return Returns an object of class \code{crisp}, which inherits from 
#' \code{spdur}. 
#' 
#' @author Andreas Beger
#' @seealso \code{\link{spdur}}
#' 
#' @examples
#' \dontrun{
#' # Prepare data
#' data(coups)
#' dur.coups <- add_duration(coups, "succ.coup", unitID="gwcode", tID="year",
#'                            freq="year")
#' 
#' # Estimate model
#' model3 <- spdurCrisp(
#'   duration ~ polity2,
#'   atrisk ~ polity2,
#'   train=dur.coups, test=dur.coups[1,], 
#'   pred=dur.coups[1,])
#' }
#' 
#' @export spdurCrisp

spdurCrisp <- function (duration, atrisk, train = train, test = test, 
                        pred = pred, last = "end.spell", t.0="t.0", 
                        distr = "weibull", stat = 'conditional risk', 
                        iter = 100, npred=6, ...) {
  
  # Input validation
  if (is.null(data)) stop("No data provided")
  if (is.null(last)) stop("Must specify censoring variable")
  if (is.null(distr)) stop("Must specify distribution")
  
  # Estimate parameters
  model <- spdur(duration=duration, atrisk=atrisk, data=train, last=last, 
                 distr=distr, max.iter=iter)
  
  # In-sample and test predictions
  cat('Training set predictions...\n')
  train.p <- predict(model, stat=stat)
  cat('Validation set predictions...\n')
  test.p <- predict(model, data=test, stat=stat)
  
  # Forecast
  cat('Forecast...\n')
  pred.p <- forecast(model, pred.data=pred, stat=stat, npred=npred)
  
  # Prepare and return results
  res <- model
  res$call <- match.call() 
  res$train.p <- train.p
  res$test.p <- test.p
  res$pred.p <- pred.p
  class(res) <- c('crisp', 'spdur')
  return(res)
  
  # Format and show estimates
  print(summary(model))
}
