#' Predict fitted values for a split-population duration model
#' 
#' \code{predict} method for class ``\code{spdur}''.
#' 
#' @method predict spdur
#' 
#' @param object Object of class ``\code{spdur}''.
#' @param data Optional data for which to calculate fitted values, defaults to 
#' training data.
#' @param stat Quantity of interest to calculate. Default conditional probability 
#' of being at risk, i.e. conditioned on observed survival up to time \code{t}. 
#' See below for list of values.
#' @param \dots Optional arguments to pass to \code{predict} function.
#' 
#' @details
#' Calculates various types of probabilities, where ``conditional'' is used in 
#' reference to conditioning on the observed survival time of a spell up to 
#' time \eqn{t}, in addition to conditioning on any variables included in the 
#' model (which is always done). Valid values for the \code{stat} option 
#' include:
#' \itemize{
#' \item ``conditional risk'': \eqn{Pr(Cure=0|Z\gamma, T>t)}{Pr(Cure=0|Z*gamma, T>t)}
#' \item ``conditional cure'': \eqn{Pr(Cure=1|Z\gamma, T>t)}{Pr(Cure=1|Z*gamma, T>t)}
#' \item ``hazard'': \eqn{Pr(T=t|T>t, C=0, X\beta) * Pr(Cure=0|Z\gamma)}{Pr(T=t|T>t, C=0, X*beta) * Pr(Cure=0|Z*gamma)}
#' \item ``failure'': \eqn{Pr(T=t|T>t-1, C=0, X\beta) * Pr(Cure=0|Z\gamma)}{Pr(T=t|T>t-1, C=0, X*beta) * Pr(Cure=0|Z*gamma)}
#' \item ``unconditional risk'': \eqn{Pr(Cure=0|Z\gamma)}{Pr(Cure=0|Z*gamma)}
#' \item ``unconditional cure'': \eqn{Pr(Cure=1|Z\gamma)}{Pr(Cure=1|Z*gamma)}
#' \item ``conditional hazard'': \eqn{Pr(T=t|T>t, C=0, X\beta) * Pr(Cure=0|Z\gamma, T>t)}{Pr(T=t|T>t, C=0, X*beta) * Pr(Cure=0|Z*gamma, T>t)}
#' \item ``conditional failure'': \eqn{Pr(T=t|T>t-1, C=0, X\beta) * Pr(Cure=0|Z\gamma, T>t)}{Pr(T=t|T>t-1, C=0, X*beta) * Pr(Cure=0|Z*gamma, T>t)}
#' }
#' The vector \eqn{Z\gamma}{Z*gamma} indicates the cure/at risk equation 
#' covariate vector, while \eqn{X\beta}{X*beta} indicates the duration equation 
#' covariate vector.
#' 
#' @return
#' Returns a data frame with 1 column corresponding to \code{stat}, in the same 
#' order as the data frame used to estimate \code{object}.
#' 
#' @author Andreas Beger, Daina Chiba, Daniel W. Hill, Nils Metternich
#' 
#' @note See \code{\link{forecast.spdur}} for producing forecasts when future 
#' covariate values are unknown.
#' 
#' @examples 
#' # get model estimates
#' data(model.coups)
#' atrisk <- predict(model.coups)
#' 
#' @S3method predict spdur
predict.spdur <- function(object, data=NULL, stat='conditional risk', ...) {
  # Input validation
  stat_choices <- c('conditional risk', 'conditional cure', 'hazard', 'failure',
                    'unconditional risk', 'unconditional cure', 
                    'conditional hazard', 'conditional failure')
                    
  if (!stat %in% stat_choices) stop('unknown statistic')
  
  # Get model frames
  if(is.null(data)) {
    # From object
    mf.dur <- object$mf.dur
    mf.risk <- object$mf.risk
    Y <- object$Y
  } else {
    # From provided data
    # First, process NA's
    fmla.dur  <- terms(object$mf.dur)
    fmla.risk <- terms(object$mf.risk)
    vars <- unique(c(all.vars(fmla.dur), all.vars(fmla.risk)))
    last <- attr(object$Y, "last")
    t.0  <- attr(object$Y, "t.0")
    vars <- c(vars, last, t.0)
    # hack to fix bug when data are complete (missing model na.action)
    if (all(complete.cases(data[, vars]))==FALSE) {
      na.action <- paste0("na.", class(na.action(object)))
      if (na.action=="na.NULL") na.action <- options("na.action")[[1]]
      df <- do.call(na.action, list(data[, vars]))
    } else {
      df <- data[, vars]
    }
    
    mf.dur <- model.frame(formula=fmla.dur, data=df)
    mf.risk <- model.frame(formula=fmla.risk, data=df)
    lhb  <- model.response(mf.dur)
    lhg  <- model.response(mf.risk)
    Y <- cbind(atrisk=lhg, duration=lhb, last=df[, last], t.0=df[, t.0])
  }
  distr <- object$distr
  
  # Design matrices
  X <- model.matrix(attr(mf.dur, 'terms'), data=mf.dur)
  Z <- model.matrix(attr(mf.risk, 'terms'), data=mf.risk)
  
  ## Start with actual prediction
  # coefficients
  coef <- coef(object)
  coeff.b <- coef[1 : ncol(X)]
  coeff.g <- coef[(ncol(X) + 1) : (ncol(X) + ncol(Z))]
  coeff.a <- coef[(ncol(X) + ncol(Z) + 1)]
  
  # alpha
  al.hat <- exp(-coeff.a)  
  # lambda
  la.hat <- pmax(1e-10, exp(-X %*% coeff.b))  # hack to prevent NaN in ft calculation below
  
  # Unconditional cure/atrisk rate
  atrisk <- plogis(Z %*% coeff.g)
  cure <- 1 - atrisk
  if (stat=='unconditional cure') res <- cure
  if (stat=='unconditional risk') res <- atrisk
  
  # S(T)
  if (distr=='weibull') {
    st <- exp(-(la.hat * Y[,2])^al.hat)
    s0 <- exp(-(la.hat * (Y[,4]))^al.hat)
  }
  if (distr=='loglog') {
    st <- 1/(1+(la.hat * Y[,2])^al.hat)
    s0 <- 1/(1+(la.hat * Y[,4])^al.hat)
  }
  
  # Conditional cure/atrisk rate
  cure.t <- cure / pmax(1e-10, (st + cure * (1 - st))) # pmax to avoid dividing it by 0
  atrisk.t  <- 1 - cure.t
  if (stat=='conditional cure') res <- cure.t
  if (stat=='conditional risk') res <- atrisk.t
  
  # f(t)
  if (distr=='weibull') {
    ft <- la.hat * al.hat * (la.hat * Y[,2])^(al.hat-1) * exp(-(la.hat * Y[,2])^al.hat) 
  }
  if (distr=='loglog') {
    ft <- (la.hat * al.hat * (la.hat * Y[,2])^(al.hat-1)) / ((1 + (la.hat * Y[,2])^al.hat)^2)  
  }
  
  if (stat=='conditional failure') res <- atrisk.t * ft / pmax(1e-10, (cure.t + atrisk.t * s0)) # Pr(T=t | (T > t-1, not cured)) * Pr(not cured | T > t)
  if (stat=='conditional hazard')  res <- atrisk.t * ft / pmax(1e-10, (cure.t + atrisk.t * st)) # Pr(T=t | (T > t, not cured)) * Pr(not cured | T > t)
  if (stat=='failure')             res <- atrisk * ft / pmax(1e-10, (cure + atrisk * s0))       # Pr(T=t | (T > t-1, not cured)) * Pr(not cured)
  if (stat=='hazard')              res <- atrisk * ft / pmax(1e-10, (cure + atrisk * st))       # Pr(T=t | (T > t, not cured)) * Pr(not cured)
  
  return(res)
}

# test code
# estimate model and predict
# load('data/coups.rda')
# dur.coup <- buildDuration(coups, "succ.coup", unitID='gwcode', tID='year',
#                           freq="year")
# model.coups <- spdur(duration ~ polity2, atrisk ~ polity2, data=dur.coup)
# pred1 <- predict(model.coups)
# all.equal(c(head(pred1)), c(0.99851638891028, 0.999983390673093, 
#   0.999998875400387, 0.999999541492892, 0.161009654496433, 0.948028467976632))
# # try new data
# test.data <- dur.coup[dur.coup$year>2000, ]
# pred2 <- predict(model.coups, test.data)
# all.equal(c(head(pred2)), c(0.9999924, 0.9999812, 0.9999998, 0.9999924, 
#                             0.9999988, 0.9999995), tolerance=1e-05)
# # try complete cases only
# dur.coup3 <- dur.coup[complete.cases(dur.coup), ]
# model.coups3 <- spdur(duration ~ polity2, atrisk ~ polity2, data=dur.coup3)
# pred3.1 <- predict(model.coups3)
# all.equal(c(head(pred3.1)), c(1.000000e+00, 6.871392e-12, 9.999997e-01, 
#                               1.000000e+00, 1.000000e+00, 5.193213e-03),
#           tolerance=1e-05)
# test.data3 <- dur.coup3[dur.coup3$year>2000, ]
# pred3 <- predict(model.coups3, test.data3)
# all.equal(c(head(pred3)), c(.836345e-03, 3.218981e-12, 3.774758e-15, 
#                             2.153515e-03, 1.000000e+00, 1.000000e+00),
#           tolerance=1e-03)