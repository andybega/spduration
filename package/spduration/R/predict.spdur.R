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
    na.action <- paste0("na.", class(na.action(object)))
    df <- do.call(na.action, list(data[, vars]))
    
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
  la.hat <- exp(-X %*% coeff.b)
  
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