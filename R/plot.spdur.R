#' Plot split-duration model results.
#' 
#' See \code{\link{separationplot.spdur}} and \code{\link{plot_hazard2}}.
#' 
#' @method plot spdur
#' 
#' @param x An object of class "\code{spdur}".
#' @param type What kind of plot? "hazard1", "hazard2", "sepplot".
#' @param \dots Optional parameters passed to \code{\link{separationplot.spdur}},
#'   \code{\link{plot_hazard1}} or \code{\link{plot_hazard2}}.
#' 
#' @seealso \code{\link{separationplot.spdur}}, \code{\link{plot_hazard1}},
#'   \code{\link{plot_hazard2}}
#' @examples
#' # get model estimates
#' data(model.coups)
#' 
#' # plot
#' p <- plot(model.coups)
#' 
#' @export 
plot.spdur <- function(x, type="sepplot", ...) { 
  # Input validation
  if (!'spdur' %in% class(x)) stop('"object" argument must have class "spdur"')
  
  if (type=="hazard1") {
    plot_hazard1(x, ...)
  } else if (type=="hazard2") {
    plot_hazard2(x, ...)
  } else if (type=="sepplot") {
    separationplot.spdur(x, ...)
  } else {
    stop("Unrecognized plot type")
  }
}

#' Plot hazard rate
#' 
#' @param x An object of class "\code{spdur}".
#' @param t_lim Time period over which to plot the hazard function.
#' @param \dots Optional arguments to \code{\link{plot}}.
#' 
#' @importFrom graphics plot
plot_hazard1 <- function(x, t_lim=NULL, ...) {
  # Choose t values if not set
  if (is.null(t_lim)) {
    max_t <- round(max(x$Y[x$Y[, "last"]==1, "duration"]) * 1.2)
    t <- seq(1, max_t, length.out=100)
  } else {
    t <- seq(t_lim[1], t_lim[2], length.out=100)
  }
  
  h_t <- hazard(x, t)
  plot(t, h_t, ..., type="l", xlab="Time", ylab="Conditional Hazard")
}

#' Calculate hazard function values
#' 
#' @param x An object of class "\code{spdur}".
#' @param t Vector of duration values over which to evaluate the hazard function.
#' 
#' @export
hazard <- function(x, t=NULL) {
  # Choose t values if not set
  if (is.null(t)) {
    max_t <- round(max(x$Y[x$Y[, "last"]==1, "duration"]) * 1.2)
    t <- seq(1, max_t, length.out=100)
  }

  # Pass to hazard function for `dist`
  dist <- x$dist
  if (dist=="weibull") {
    h_t <- weibull_hazard(t, x)
  } else if (dist=="loglog") {
    h_t <- loglog_hazard(t, x)
  } else {
    stop(paste0("Unrecognized distribution: ", dist))
  }
  return(h_t)
}

#' Weibull hazard function
#' 
#' @param t t
#' @param x x
#' 
#' @importFrom stats plogis
weibull_hazard <- function(t, x) {
  dur.dat<-x$mf.dur
  risk.dat<-x$mf.risk 
  ti<-t
  
  X <- model.matrix(attr(x$mf.dur, 'terms'), data=x$mf.dur)
  Z <- model.matrix(attr(x$mf.risk, 'terms'), data=x$mf.risk)
  
  beta<-x$coef[1:ncol(X)]
  gamma<-x$coef[(ncol(X) + 1):(ncol(X) + ncol(Z))]
  a<-x$coef[ncol(X) + ncol(Z) + 1]
  alpha<-exp(-a)
  
  mean.X<-apply(X,2,mean)
  mean.Z<-apply(Z,2,mean)
  
  lambda<-pmax(1e-10, exp(-mean.X %*% beta))
  cure<-1 - plogis(mean.Z %*% gamma)
  
  preds<-vector(length=length(ti))
  for(i in 1:length(ti)){
    st<-exp(-(lambda * ti[i])^alpha)
    cure.t<-cure / pmax(1e-10, (st + cure * (1 - st)))
    atrisk.t<-1 - cure.t
    ft<-lambda * alpha * (lambda * ti[i])^(alpha-1) * exp(-(lambda * ti[i])^alpha)
    preds[i]<-atrisk.t * ft / pmax(1e-10, (cure.t + atrisk.t * st))
  }
  return(preds)
}

#' Log-logistic hazard function
#' 
#' @param t t
#' @param x x
loglog_hazard  <- function(t, x) {
  NULL
}

#' Generate a Separation Plot
#' 
#' \code{\link{separationplot}} method for class ``\code{spdur}''.
#' 
#' @method separationplot spdur
#' 
#' @param x An object of class "\code{spdur}".
#' @param pred_type Which statistic to plot, i.e. "conditional hazard" or 
#'   "conditional risk".
#' @param obs Variable that captures observed outcomes. If \code{NULL} (default), 
#'   it is chosen based on \code{pred_type}: "fail" for (conditional) hazard, and 
#'   "atrisk" for (conditional) risk.
#' @param endSpellOnly Should only the last observation in each spell be kept? 
#' \code{FALSE} by default.
#' @param lwd1 See \code{\link{separationplot}}.
#' @param lwd2 See \code{\link{separationplot}}.
#' @param shuffle See \code{\link{separationplot}}.
#' @param heading See \code{\link{separationplot}}.
#' @param show.expected See \code{\link{separationplot}}.
#' @param newplot See \code{\link{separationplot}}
#' @param type See \code{\link{separationplot}}.
#' @param \dots Optional parameters passed to \code{\link{separationplot}}, 
#'   e.g. type of statistic to calculate.
#' 
#' @details Creates a \code{\link{separationplot}} of fitted values from 
#' split-duration model results using \code{\link{predict.spdur}}.
#' 
#' @seealso \code{\link{separationplot}}, \code{\link{predict.spdur}}
#' @examples
#' # get model estimates
#' data(model.coups)
#' 
#' # plot
#' p <- plot(model.coups)
#' 
#' @export 
#' @importFrom separationplot separationplot
#' @importFrom stats predict
separationplot.spdur <- function(x, pred_type="conditional hazard", obs=NULL, 
  endSpellOnly=FALSE, lwd1=5, lwd2=2, shuffle=TRUE, heading="", 
  show.expected=TRUE, newplot=FALSE, type="line", ...) {
  # Input validation
  if (!'spdur' %in% class(x)) stop('"object" argument must have class "spdur"')
  
  # Try to find value for `obs`
  if (is.null(obs)) {
    obs <- switch(pred_type, "conditional hazard"="fail", "hazard"="fail",
                  "risk"="atrisk", "conditional risk"="atrisk")
    error <- paste0("obs is NULL, no default for pred_type=", pred_type)
    if (is.null(obs)) stop(error)
  }
  model <- x
  
  # Get data
  # need to do something with napredict to make this more flexible
  actual <- model$Y[, obs]
  
  # Get predicted 
  pred <- as.vector(as.matrix(predict(model, type=pred_type)))
  
  # Keep end of spell only
  if (endSpellOnly==T) {
    pred <- pred[ model$Y[, "last"]==1 ]
    actual <- actual[ model$Y[, "last"]==1 ]
  }
  
  # Separationplot call
  plot <- separationplot(pred, actual,
                         shuffle=T, heading='', show.expected=T, newplot=F, 
                         type='line', lwd1=lwd1, lwd2=lwd2, ...)
}