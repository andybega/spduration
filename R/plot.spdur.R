#' Plot split-duration model results.
#' 
#' Plot results from a spduration model. Two types are currently implemented:
#' a separation plot for evaluating model predictions ("sepplot"), and a plot of 
#' the conditional hazard rate ("hazard"), with or without simulation-based
#' confidence intervals.
#' 
#' @method plot spdur
#' 
#' @param x An object of class "\code{spdur}".
#' @param type What kind of plot? "sepplot" or "hazard".
#' @param ci For plots of the hazard rate, should a confidence interval be included?
#' @param \dots Optional parameters passed to \code{\link{separationplot.spdur}}
#'   or \code{\link{plot_hazard}}.
#' 
#' @seealso \code{\link{separationplot.spdur}}, \code{\link{plot_hazard}}
#' @examples
#' # get model estimates
#' data(model.coups)
#' 
#' # plot
#' plot(model.coups)
#' plot(model.coups, type = "hazard")
#' 
#' @export 
plot.spdur <- function(x, type="sepplot", ci=TRUE, ...) { 
  # Input validation
  if (!'spdur' %in% class(x)) stop('"object" argument must have class "spdur"')
  
  if (type=="hazard") {
    plot_hazard(x, ci = ci, ...)
  } else if (type=="sepplot") {
    separationplot.spdur(x, ...)
  } else {
    stop("Unrecognized plot type")
  }
}

#' Plot hazard function
#' 
#' \code{plot_hazard} plots the shape of estimated hazard function in respect 
#' to duration, given a set of values for the duration and risk equations 
#' covariates. Confidence intervals are provided through simulation.
#' 
#' @param x An object of class \code{spdur}
#' @param t Time values at which to evaluate hazard function, e.g. \code{c(1:50)}.
#'    Defaults to 1 through 1.2 * maximum duration value in data. 
#' @param ci Compute simulation-based confidence interval?
#' @param n Number of simulations to use for CI, defaults to 1,000. 
#' @param xvals A vector of values for the duration equation variables, in the 
#'    same order as the duration equation in \code{x}. Defaults to means.
#' @param zvals A vector of values for the risk equation varialbes, in teh same
#'    order as the risk equation in \code{x}. Defaults to means.
#' @param \dots Additional parameters passed to \code{\link{plot}}.
#' 
#' @seealso \code{\link{separationplot.spdur}}
#' 
#' @examples 
#' # Get model estimates
#' data(model.coups)
#' 
#' # Plot
#' plot_hazard(model.coups, ci = FALSE)
#' plot_hazard(model.coups, ci = TRUE)
#' 
#' @importFrom MASS mvrnorm
#' @importFrom graphics lines plot
#' @importFrom stats quantile rnorm
#' @export
plot_hazard <- function(x, t = NULL, ci=TRUE, n=1000, xvals=NULL, zvals=NULL, ...) {
  
  # Set t vector if needed to 1.2 * max observed duration; lower limit is 1
  if (is.null(t)) {
    max_t <- round(max(x$Y[x$Y[, "last"]==1, "duration"]) * 1.2)
    t     <- seq(1, max_t, length.out=100)
  } 
  
  # Extract covariate matrices
  dur.dat  <- x$mf.dur
  risk.dat <- x$mf.risk 
  X <- model.matrix(attr(x$mf.dur, 'terms'), data=x$mf.dur)
  Z <- model.matrix(attr(x$mf.risk, 'terms'), data=x$mf.risk)
  
  # Extract coefficient point estimates
  beta  <- coef(x, model = "duration")
  gamma <- coef(x, model = "risk")
  alpha <- coef(x, model = "distr")
  alpha <- exp(-alpha)
  beta_vcv  <- vcov(x, "duration")
  gamma_vcv <- vcov(x, "risk")
  alpha_vcv <- vcov(x, "distr")
  
  if (is.null(xvals)) {
    X_vals <- apply(X, 2, mean)		
  } else if (!length(xvals)==ncol(X) && length(xvals)-ncol(X)==-1) {
    stop("Incorrect length for xvals, did you forget 1 for intercept term?")			
  } else if (!length(xvals)==ncol(X)) {
    stop("Incorrect length for xvals")
  } else {
    X_vals <- xvals
  }
  
  if (is.null(zvals)) {
    Z_vals <- apply(Z, 2, mean)		
  } else if (!length(zvals)==ncol(Z) && length(zvals)-ncol(Z)==-1) {
    stop("Incorrect length for zvals, did you forget 1 for intercept term?")			
  } else if (!length(zvals)==ncol(Z)) {
    stop("Incorrect length for zvals")
  } else {
    Z_vals <- zvals
  }
  
  # Calculate hazard using point estimates only
  lambda <- exp(-X_vals %*% beta)
  cure   <- 1 - plogis(Z_vals %*% gamma)
  
  ht <- hazard(ti = t, lambda = lambda, cure = cure, alpha = alpha,
               out = NULL, dist = x$distr)
  
  if (ci==TRUE) {
    Coef_smpl <- mvrnorm(n = n, mu = coef(x, "full"), Sigma = vcov(x, "full"))
    
    b_idx <- 1:x$n.terms$duration
    g_idx <- (max(b_idx) + 1):(max(b_idx) + x$n.terms$risk)
    a_idx <- (max(g_idx) + 1):ncol(Coef_smpl)
    
    Beta  <- Coef_smpl[, b_idx]
    Gamma <- Coef_smpl[, g_idx]
    A     <- Coef_smpl[, a_idx]
    Alpha <- exp(-A)
    
    lambda <- exp(-tcrossprod(X_vals, Beta))
    cure   <- 1 - plogis(tcrossprod(Z_vals, Gamma))
    
    sims <- matrix(nrow = length(t), ncol = n)
    hmat <- matrix(nrow = length(t), ncol = 3)
    
    for (i in 1:n) {
      sims[, i] <- hazard(ti = t, lambda = lambda[i], cure = cure[i], 
                          alpha = Alpha[i], out = NULL, dist = x$distr)
    }
    
    hmat[, 1] <- ht
    hmat[, 2] <- apply(sims, 1, quantile, probs = 0.05)
    hmat[, 3] <- apply(sims, 1, quantile, probs = 0.95)
    
    plot(t, hmat[,1], type="l", xlab="Time", ylab="Conditional Hazard",
         ylim = c(0, max(hmat[, 3])), ...)
    lines(t , hmat[,2], lty=2)
    lines(t , hmat[,3], lty=2)
  } else {
    # Plot without CIs
    plot(t, ht, type = "l", xlab = "Time", "ylab" = "Conditional hazard",
         ylim = c(0, 1.2*max(ht)), ...)
  }
  
  invisible(NULL)
}


#' Calculate hazard function values
#' 
#' @param x An object of class "\code{spdur}".
#' @param ti Vector of duration values over which to evaluate the hazard function.
#' 
#' @keywords internal
hazard <- function(ti, lambda, cure, alpha, out, dist) {
  
  # minimum value for P, to avoid divide by 0 errors
  p_min <- 1e-16
  
  ht <- vector("numeric", length(ti))
  if (dist=="weibull") {
    
    st       <- exp(-(lambda * ti)^alpha)
    cure.t   <- cure / pmax(p_min, (st + cure * (1 - st))) # pmax to avoid dividing it by 0
    atrisk.t <- 1 - cure.t
    ft       <- lambda * alpha * (lambda * ti)^(alpha-1) * exp(-(lambda * ti)^alpha)
    
    ht       <- atrisk.t * ft / pmax(p_min, (cure.t + atrisk.t * st))
    
  } else if (dist=="loglog") {
    
    st       <- 1/(1+(lambda * ti)^alpha)
    cure.t   <- cure / pmax(p_min, (st + cure * (1 - st))) # pmax to avoid dividing it by 0
    atrisk.t <- 1 - cure.t
    ft       <- (lambda * alpha * (lambda * ti)^(alpha-1)) / ((1 + (lambda * ti)^alpha)^2)  
    
    ht       <- atrisk.t * ft / pmax(p_min, (cure.t + atrisk.t * st))
    
  } else {
    
    stop(paste0("Unrecognized distribution: ", dist))
    
  }
  
  ht
}


#' Plot conditional hazard rate
#'
#' Plot hazard function without simulated confidence intervals. See
#' \code{\link{plot_hazard}} instead.
#' 
#' @param x class "spdur" object
#' @param ... passed to \code{plot_hazard}
#'   
#' @return NULL, plots.
#' @export
plot_hazard1 <- function(x, ...) {
  warning("Use plot_hazard(x, ci = FALSE, ...) instead")
  plot_hazard(x, ci = FALSE, ...)
}


#' Simulate and plot hazard function
#' 
#' Plot hazard function with simulated confidence intervals. See
#' \code{\link{plot_hazard}} instead.
#' 
#' @param x class "spdur" object
#' @param ... passed to \code{plot_hazard}
#'   
#' @return NULL, plots.
#' @export
plot_hazard2 <- function(x, ...) {
  warning("Use plot_hazard(x, ci = TRUE, ...) instead")
  plot_hazard(x, ci = TRUE, ...)
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
#' @importFrom separationplot separationplot
#' @importFrom stats predict
#' @export 
#' @method separationplot spdur
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

