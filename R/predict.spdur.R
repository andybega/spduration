#' Predict fitted values for a split-population duration model
#' 
#' \code{predict} method for class ``\code{spdur}''.
#' 
#' @method predict spdur
#' 
#' @param object Object of class ``\code{spdur}''.
#' @param newdata Optional data for which to calculate fitted values, defaults to 
#' training data.
#' @param type Quantity of interest to calculate. Default conditional hazard, 
#' i.e. conditioned on observed survival up to time \code{t}. 
#' See below for list of values.
#' @param truncate For conditional hazard, truncate values greater than 1.
#' @param \dots not used, for compatability with generic function.
#' 
#' @details
#' Calculates various types of probabilities, where ``conditional'' is used in 
#' reference to conditioning on the observed survival time of a spell up to 
#' time \eqn{t}, in addition to conditioning on any variables included in the 
#' model (which is always done). Valid values for the \code{type} option 
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
#' Returns a data frame with 1 column corresponding to \code{type}, in the same 
#' order as the data frame used to estimate \code{object}.
#' 
#' @note See \code{\link{forecast.spdur}} for producing forecasts when future 
#' covariate values are unknown.
#' 
#' @examples 
#' # get model estimates
#' data(model.coups)
#' atrisk <- predict(model.coups)
#' 
#' @importFrom stats complete.cases model.frame model.response model.matrix plogis
#' @export
predict.spdur <- function(object, newdata=NULL, type="conditional hazard", 
                          truncate=TRUE, ...) {
  # Input validation
  type_choices <- c('conditional risk', 'conditional cure', 'hazard', 'failure',
                    'unconditional risk', 'unconditional cure', 
                    'conditional hazard', 'conditional failure')
                    
  if (!type %in% type_choices) stop('unknown statistic')
  
  # minimum value for P, to avoid divide by 0 errors
  p_min <- 1e-16
  
  # Get model frames
  if(is.null(newdata)) {
    # From object
    mf.dur <- object$mf.dur
    mf.risk <- object$mf.risk
    Y <- object$Y
  } else {
    # From provided data
    fmla.dur  <- terms(object$mf.dur)
    fmla.risk <- terms(object$mf.risk)
    vars <- unique(c(all.vars(fmla.dur), all.vars(fmla.risk)))
    last <- attr(object$Y, "last")
    t.0  <- attr(object$Y, "t.0")
    vars <- c(vars, last, t.0)
    
    # hack to fix bug when data are complete (missing model na.action)
    if (all(complete.cases(newdata[, vars]))==FALSE) {
      na.action <- paste0("na.", class(na.action(object)))
      if (na.action=="na.NULL") na.action <- options("na.action")[[1]]
      df <- do.call(na.action, list(newdata[, vars]))
    } else {
      df <- newdata[, vars]
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
  
  # DV
  ti <- Y[, 2]    # Time at current period
  t0 <- Y[, 4]    # Time at previous period
  
  # coefficients
  coef    <- coef(object)
  coeff.b <- coef[1 : ncol(X)]
  coeff.g <- coef[(ncol(X) + 1) : (ncol(X) + ncol(Z))]
  coeff.a <- coef[(ncol(X) + ncol(Z) + 1)]
  alpha   <- exp(-coeff.a)  
  lambda  <- pmax(p_min, exp(-X %*% coeff.b))  # hack to prevent NaN in ft calculation below
  
  ## Start with actual prediction
  
  # Unconditional cure/atrisk rate
  atrisk <- plogis(Z %*% coeff.g)
  cure   <- 1 - atrisk
  if (type=='unconditional cure') res <- cure
  if (type=='unconditional risk') res <- atrisk
  
  # S(T)
  if (distr=='weibull') {
    st <- exp(-(lambda * ti)^alpha)
    s0 <- exp(-(lambda * t0)^alpha)
  }
  if (distr=='loglog') {
    st <- 1/(1+(lambda * ti)^alpha)
    s0 <- 1/(1+(lambda * t0)^alpha)
  }
  
  # Conditional cure/atrisk rate
  cure.t <- cure / pmax(p_min, (st + cure * (1 - st))) # pmax to avoid dividing it by 0
  atrisk.t  <- 1 - cure.t
  if (type=='conditional cure') res <- cure.t
  if (type=='conditional risk') res <- atrisk.t
  
  # Calculate f(t)
  if (distr=='weibull') {
    ft <- lambda * alpha * (lambda * ti)^(alpha-1) * exp(-(lambda * ti)^alpha) 
  }
  if (distr=='loglog') {
    ft <- (lambda * alpha * (lambda * ti)^(alpha-1)) / ((1 + (lambda * ti)^alpha)^2)  
  }
  
  if (type=='failure') {  
    # Pr(T=t | (T > t-1, not cured)) * Pr(not cured)
    res <- atrisk * ft / pmax(p_min, (cure + atrisk * s0))       
  }             
  if (type=='hazard') {  
    # Pr(T=t | (T > t, not cured)) * Pr(not cured)
    res <- atrisk * ft / pmax(p_min, (cure + atrisk * st))       
  }
  if (type=='conditional failure') {  
    # Pr(T=t | (T > t-1, not cured)) * Pr(not cured | T > t)
    res <- atrisk.t * ft / pmax(p_min, (cure.t + atrisk.t * s0)) 
  }
  if (type=='conditional hazard') {
    # Pr(T=t | (T > t, not cured)) * Pr(not cured | T > t)
    res <- atrisk.t * ft / pmax(p_min, (cure.t + atrisk.t * st)) 
  }
  
  # Sometimes h(t) can > 1, truncate these values?
  if (truncate) res <- ifelse(res>1, 1, res)
  
  return(as.numeric(res))
}

