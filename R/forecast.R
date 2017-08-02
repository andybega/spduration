#' Forecast from a spdur model
#' 
#' \code{\link{forecast}} method for \code{\link{spdur}} class objects.
#' 
#' @method forecast spdur
#' 
#' @param object A \code{\link{spdur}} class model object.
#' @param \dots Optional arguments, not used.
#' @param pred.data Data on which to base forecasts, i.e. slice of last time
#' unit's observations for all cross-sectional units.
#' @param stat Which statistic to forecast, see \code{\link{predict.spdur}}
#' for possible options
#' @param n.ahead How many time periods to predict ahead. Default is 6.
#' 
#' @details This function will create out-of-sample predictions of ``stat'' 
#' using model estimates and the prediction data provided. It is assumed that
#' prediction data consist of a slice of the last time period observed for
#' the data used to estimate the model in \code{object}. For each row, 
#' \code{forecast.spdur} will estimate the model predictions for that time point
#' and then extrapolate the resulting probability to \code{n.ahead} time
#' periods using appropriate probability theory. 
#' 
#' For situations in which the covariate values are known for future time
#' periods, e.g. in a test sample use \code{\link{predict.spdur}} instead.
#' 
#' @examples
#' library(forecast)
#' data(coups)
#' data(model.coups)
#' 
#' coups.dur <- add_duration(coups, "succ.coup", "gwcode", "year", freq="year")
#' pred.data <- coups.dur[coups.dur$year==max(coups.dur$year), ]
#' pred.data <- pred.data[complete.cases(pred.data), ]
#' fcast <- forecast(model.coups, pred.data=pred.data)
#' 
#' @export 
#' @importFrom forecast forecast
forecast.spdur <- function(object, ..., pred.data = NULL, 
                           stat = 'conditional hazard', n.ahead = 6)
{
  if (is.null(pred.data)) stop("Must provide pred.data")
  stat_choices <- c('conditional risk', 'conditional cure', 'hazard', 'failure',
                    'unconditional risk', 'unconditional cure', 
                    'conditional hazard', 'conditional failure')
  
  if (!stat %in% stat_choices) stop('unknown statistic')
  
  # Equation formulas
  fmla.dur  <- terms(object$mf.dur)
  fmla.risk <- terms(object$mf.risk)
  
  # Duration equation
  mf.dur <- model.frame(formula=fmla.dur, data=pred.data)
  X <- model.matrix(attr(mf.dur, 'terms'), data=mf.dur)
  lhb <- model.response(mf.dur) 
  # Risk/non-immunity equation
  mf.risk <- model.frame(formula=fmla.risk, data=pred.data)
  Z <- model.matrix(attr(mf.risk, 'terms'), data=mf.risk)
  lhg <- model.response(mf.risk) 
  # Y vectors
  t.0  <- attr(object$Y, "t.0")
  Y <- cbind(atrisk=0, duration=lhb, last=0, t.0=pred.data[, t.0])
  
  ## Start with actual prediction
  # coefficients
  coef <- coef(object)
  coeff.b <- coef[1 : ncol(X)]
  coeff.g <- coef[(ncol(X) + 1) : (ncol(X) + ncol(Z))]
  coeff.a <- coef[(ncol(X) + ncol(Z) + 1)]
  
  # alpha
  al.hat <- exp(-coeff.a)  
  # lambda
  la.hat.pred <- exp(-X %*% coeff.b)
  
  # unconditional pr(~cure) & pr(cure)
  atrisk.pred <- plogis(Z %*% coeff.g)
  cure.pred <- 1 - atrisk.pred
  
  if (stat=='unconditional cure') res <- matrix(rep(cure.pred, n.ahead), ncol=n.ahead)
  if (stat=='unconditional risk') res <- matrix(rep(atrisk.pred, n.ahead), ncol=n.ahead)
  
  # Create emtpy matrices for various quantities
  rows <- length(la.hat.pred)
  
  ########################################################
  # S(T), will differ by distr.
  ########################################################
  distr <- object$distr
  if (distr=='weibull') {
    # S(T)
    s0 <- exp(-(la.hat.pred * (Y[,2]-1))^al.hat)
    st <- matrix(nrow=rows, ncol=n.ahead)
    # conditional pr(non cure | T) = 1 - pr(non cure | T)
    cure.t <- matrix(nrow=rows, ncol=n.ahead)
    # Fill in values
    for (i in 1:n.ahead) {
      st[, i] <- exp(-(la.hat.pred * (Y[, 2] + (i - 1)))^al.hat)
      cure.t[, i] <- cure.pred / (st[, i] + cure.pred * (1 - st[, i]))
    }
    atrisk.t <- 1 - cure.t
  }
  if (distr=='loglog') {
    # S(T)
    s0 <- 1/(1+(la.hat.pred * Y[,2])^al.hat)
    st <- matrix(nrow=rows, ncol=n.ahead)
    # conditional pr(non cure | T) = 1 - pr(non cure | T)
    cure.t <- matrix(nrow=rows, ncol=n.ahead)
    # Fill in values
    for (i in 1:n.ahead) {
      st[, i] <- 1/(1 + (la.hat.pred * (Y[,2] + (i - 1)))^al.hat)
      cure.t[, i] <- cure.pred / (st[, i] + cure.pred * (1 - st[, i]))
    }
    atrisk.t <- 1 - cure.t
  }
  
  if (stat=='conditional cure') res <- cure.t
  if (stat=='conditional risk') res <- atrisk.t
  
  ########################################################
  # Empty matrices to be filled in by distr. below
  ########################################################
  # f(t)
  ft <- matrix(nrow=rows, ncol=n.ahead)
  # unconditional/conditional f(t)
  u.f <- matrix(nrow=rows, ncol=n.ahead)
  c.f <- matrix(nrow=rows, ncol=n.ahead)
  # unconditional/conditional hazard
  u.h <- matrix(nrow=rows, ncol=n.ahead)
  c.h <- matrix(nrow=rows, ncol=n.ahead)
  
  ########################################################
  # f(t) will differ by distr.
  ########################################################
  if (distr=='weibull') {
    for (i in 1:n.ahead) {
      ft[, i] <- la.hat.pred * al.hat * (la.hat.pred * (Y[,2] + (i - 1)))^(al.hat - 1) * 
        exp(-(la.hat.pred * (Y[,2] + (i - 1)))^al.hat)
      # f(t) with unconditional risk
      if (i==1) u.f[, i] <- atrisk.pred * ft[, i] / s0
      if (i > 1) u.f[, i] <- atrisk.pred * ft[, i] / st[, (i-1)]
      # f(t) with risk|t>T
      if (i==1) u.f[, i] <- atrisk.t[, i] * ft[, i] / s0
      if (i > 1) u.f[, i] <- atrisk.t[, i] * ft[, i] / st[, (i-1)]
      # h(t) with unconditional risk
      u.h[, i] <- atrisk.pred*ft[, i] / (cure.pred + atrisk.pred*st[, i])
      # h(t) with risk|t>T
      c.h[, i] <- atrisk.t[, i]*ft[, i] / (cure.t[, i] + atrisk.t[, i]*st[, i])
    }
  }
  if (distr=='loglog') {
    for (i in 1:n.ahead) {
      ft[, i] <- (la.hat.pred * al.hat * (la.hat.pred * (Y[,2] + (i - 1)))^(al.hat - 1)) / 
        ((1 + (la.hat.pred * (Y[,2] + (i - 1)))^al.hat)^2)
      # f(t) with unconditional risk
      if (i==1) u.f[, i] <- atrisk.pred * ft[, i] / (cure.pred + atrisk.pred * s0)
      if (i > 1) u.f[, i] <- atrisk.pred * ft[, i] / (cure.pred + atrisk.pred * st[, (i-1)])
      # f(t) with risk|t>T
      if (i==1) u.f[, i] <- atrisk.t[, i] * ft[, i] / (cure.t[, i] + atrisk.t[, i] * s0)
      if (i > 1) u.f[, i] <- atrisk.t[, i] * ft[, i] / (cure.t[, i] + atrisk.t[, i] * st[, (i-1)])
      # h(t) with unconditional risk
      u.h[, i] <- atrisk.pred*ft[, i] / (cure.pred + atrisk.pred*st[, i])
      # h(t) with risk|t>T
      c.h[, i] <- atrisk.t[, i]*ft[, i] / (cure.t[, i] + atrisk.t[, i]*st[, i])
    }
  }
  
  ########################################################
  # Tally final results
  ########################################################
  if (stat=='hazard') res <- u.h
  if (stat=='failure') res <- u.f
  if (stat=='conditional hazard') res <- c.h
  if (stat=='conditional failure') res <- c.f
  
  return(res)
}