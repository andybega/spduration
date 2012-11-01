##########
# Predict and separationplot methods
# November 2012
# Andreas Beger
#
###########

## Main predict method, calls on predict functions for each distribution
predict.spdur <- function(object, data=NULL, stat=cure, ...)
{
  if(is.null(data)) {
    data <- get(paste(object$call$data), pos=1)
  }
  
  # Duration equation
  mf.dur <- model.frame(formula=object$call$duration, data=data)
  X <- model.matrix(attr(mf.dur, 'terms'), data=mf.dur)
  lhb <- model.response(mf.dur)
    
  # Risk/non-immunity equation
  mf.risk <- model.frame(formula=object$call$atrisk, data=data)
  Z <- model.matrix(attr(mf.risk, 'terms'), data=mf.risk)
  lhg <- model.response(mf.risk)
    
  # Y vectors
  censor <- data[, as.character(object$call$last)[3]]
  Y <- cbind(atrisk=lhg, duration=lhb, last=censor)

  if (object$distr=='weibull') {
    res <- pred_weibull(coef=coef(object), vcv=object$vcv, Y=Y, X=X, Z=Z, stat)
  }
  if (object$distr=='loglog') {
    res <- pred_loglog(coef=coef(object), vcv=object$vcv, Y=Y, X=X, Z=Z, stat)
  }
  
  return(res)
}

## Weibull predict function
# Input: parameters, data, and sims to run
# Output: row-matrix of predicted probability quantiles
#
pred_weibull <- function(coef, vcv, Y, X, Z, stat) {
  require(MASS)
  
  coeff.b <- coef[1 : ncol(X)]
  coeff.g <- coef[(ncol(X) + 1) : (ncol(X) + ncol(Z))]
  coeff.a <- coef[(ncol(X) + ncol(Z) + 1)]
  
  # alpha
  al.hat <- exp(-coeff.a)  
  # lambda
  la.hat <- exp(-X %*% coeff.b)
  # unconditional pr(~cure) & pr(cure)
  n.cure <- plogis(Z %*% coeff.g)
  cure <- 1 - n.cure
  # S(T)
  st <- exp(-(la.hat * Y[,2])^al.hat)
  s0 <- exp(-(la.hat * (Y[,2]-1))^al.hat)
  # conditional pr(non cure | T) = 1 - pr(non cure | T)
  cure.t <- cure / (st + cure * (1 - st))
  n.cure.t  <- 1 - cure.t
  # f(t)
  ft <- la.hat * al.hat * (la.hat * Y[,2])^(al.hat-1) * exp(-(la.hat * Y[,2])^al.hat)
  
  # quantities of interest
  pr.c.f <- n.cure.t * ft / s0 # Pr(T=t | (T > t-1, not cured)) * Pr(not cured | T > t)
  pr.c.h <- n.cure.t * ft / st # Pr(T=t | (T > t, not cured)) * Pr(not cured | T > t)
  pr.u.f <- n.cure * ft / s0 # Pr(T=t | (T > t-1, not cured)) * Pr(not cured)
  pr.u.h <- n.cure * ft / st # Pr(T=t | (T > t, not cured)) * Pr(not cured)
  
  res <- data.frame(n.cure.t)
  return(res)
}
  
  
  
#   # Empty results matrix
#   cure <- matrix(NA, nrow(y), sims)
#   
#   for (i in 1:sims){
#     draw <- mvrnorm(1, coef, vcv)
#     
#     lxbeta <- X %*% draw[1:ncol(X)]
#     pxbeta <- Z %*% draw[(ncol(X)+1):(ncol(X)+ncol(Z))]
#     alph <- 1*draw[ncol(X)+ncol(Z)+1]
#     
#     alph <- exp(-alph)
#     lyhat <- exp(-lxbeta)
#     pyhat <- 1/(1+exp(-pxbeta))
#     cure[, i]<-pyhat/((lyhat*y[,2])^alph+pyhat*(1-(lyhat*y[,2])^alph))
#   }
#   print(dim(cure))
#   predvalues <- apply(cure, 1, quantile, probs = c(0.025,0.5,0.975), na.rm=T)
#   predvalues <- t(predvalues)
#   return(predvalues)
# }

## Loglog predict function
# Input: parameters, data, and sims to run
# Output: row-matrix of predicted probability quantiles
#
pred_loglog <- function(y, X, Z, sims) {
  require(MASS)
  
  coeff.b <- coef[1 : ncol(X)]
  coeff.g <- coef[(ncol(X) + 1) : (ncol(X) + ncol(Z))]
  coeff.a <- coef[(ncol(X) + ncol(Z) + 1)]
  
  # alpha
  al.hat <- exp(-coeff.a)  
  # lambda
  la.hat <- exp(-X %*% coeff.b)
  # unconditional pr(~cure) & pr(cure)
  n.cure <- plogis(Z %*% coeff.g)
  cure <- 1 - n.cure
  # S(T)
  st <- exp(-(la.hat * Y[,2])^al.hat)
  s0 <- exp(-(la.hat * (Y[,2]-1))^al.hat)
  # conditional pr(non cure | T) = 1 - pr(non cure | T)
  cure.t <- cure / (st + cure * (1 - st))
  n.cure.t  <- 1 - cure.t.in
  # f(t)
  if (distr=="weibull"){
    ft.in <- la.hat.in * al.hat * (la.hat.in * Y[,2])^(al.hat-1) * exp(-(la.hat.in * Y[,2])^al.hat)
    ft.out <- la.hat.out * al.hat * (la.hat.out * Y.test[,2])^(al.hat-1) * exp(-(la.hat.out * Y.test[,2])^al.hat)
    ft.pred <- la.hat.pred * al.hat * (la.hat.pred * Y.pred[,2])^(al.hat-1) * exp(-(la.hat.pred * Y.pred[,2])^al.hat)
    ft.pred.2 <- la.hat.pred * al.hat * (la.hat.pred * (Y.pred[,2]+1))^(al.hat-1) * exp(-(la.hat.pred * (Y.pred[,2]+1))^al.hat)
    ft.pred.3 <- la.hat.pred * al.hat * (la.hat.pred * (Y.pred[,2]+2))^(al.hat-1) * exp(-(la.hat.pred * (Y.pred[,2]+2))^al.hat)
    ft.pred.4 <- la.hat.pred * al.hat * (la.hat.pred * (Y.pred[,2]+3))^(al.hat-1) * exp(-(la.hat.pred * (Y.pred[,2]+3))^al.hat)
    ft.pred.5 <- la.hat.pred * al.hat * (la.hat.pred * (Y.pred[,2]+4))^(al.hat-1) * exp(-(la.hat.pred * (Y.pred[,2]+4))^al.hat)
    ft.pred.6 <- la.hat.pred * al.hat * (la.hat.pred * (Y.pred[,2]+5))^(al.hat-1) * exp(-(la.hat.pred * (Y.pred[,2]+5))^al.hat)
  }
  if (distr=="loglog"){
    ft.in <- (la.hat.in * al.hat * (la.hat.in * Y[,2])^(al.hat-1)) / ((1 + (la.hat.in * Y[,2])^al.hat)^2)
    ft.out <- (la.hat.out * al.hat * (la.hat.out * Y.test[,2])^(al.hat-1)) / ((1 + (la.hat.out * Y.test[,2])^al.hat)^2)
    ft.pred <- (la.hat.pred * al.hat * (la.hat.pred * Y.pred[,2])^(al.hat-1)) / ((1 + (la.hat.pred * Y.pred[,2])^al.hat)^2)
    ft.pred.2 <- (la.hat.pred * al.hat * (la.hat.pred * (Y.pred[,2]+1))^(al.hat-1)) / ((1 + (la.hat.pred * (Y.pred[,2]+1))^al.hat)^2)
    ft.pred.3 <- (la.hat.pred * al.hat * (la.hat.pred * (Y.pred[,2]+2))^(al.hat-1)) / ((1 + (la.hat.pred * (Y.pred[,2]+2))^al.hat)^2)
    ft.pred.4 <- (la.hat.pred * al.hat * (la.hat.pred * (Y.pred[,2]+3))^(al.hat-1)) / ((1 + (la.hat.pred * (Y.pred[,2]+3))^al.hat)^2)
    ft.pred.5 <- (la.hat.pred * al.hat * (la.hat.pred * (Y.pred[,2]+4))^(al.hat-1)) / ((1 + (la.hat.pred * (Y.pred[,2]+4))^al.hat)^2)
    ft.pred.6 <- (la.hat.pred * al.hat * (la.hat.pred * (Y.pred[,2]+5))^(al.hat-1)) / ((1 + (la.hat.pred * (Y.pred[,2]+5))^al.hat)^2)
  }
  
  # quantities of interest
  ## in-sample
  pr.c.f.in <- n.cure.t.in * ft.in / s0.in # Pr(T=t | (T > t-1, not cured)) * Pr(not cured | T > t)
  pr.c.h.in <- n.cure.t.in * ft.in / st.in # Pr(T=t | (T > t, not cured)) * Pr(not cured | T > t)
  pr.u.f.in <- n.cure.in * ft.in / s0.in # Pr(T=t | (T > t-1, not cured)) * Pr(not cured)
  pr.u.h.in <- n.cure.in * ft.in / st.in # Pr(T=t | (T > t, not cured)) * Pr(not cured)
  ## out-of-sample
  pr.c.f.out <- n.cure.t.out * ft.out / s0.out
  pr.c.h.out <- n.cure.t.out * ft.out / st.out
  pr.u.f.out <- n.cure.out * ft.out / s0.out
  pr.u.h.out <- n.cure.out * ft.out / st.out
  ## forecast
  pr.u.f.pred.1 <- n.cure.t.pred.1 * ft.pred / s0.pred
  pr.u.f.pred.2 <- n.cure.t.pred.2 * ft.pred.2 / st.pred
  pr.u.f.pred.3 <- n.cure.t.pred.3 * ft.pred.3 / st.pred.2
  pr.u.f.pred.4 <- n.cure.t.pred.4 * ft.pred.4 / st.pred.3
  pr.u.f.pred.5 <- n.cure.t.pred.5 * ft.pred.5 / st.pred.4
  pr.u.f.pred.6 <- n.cure.t.pred.6 * ft.pred.6 / st.pred.5
  
  pr.in <- list(
    n.cure.t.in = n.cure.t.in, 
    n.cure.in = n.cure.in, 
    pr.c.h.in = pr.c.h.in)
  
  pr.out <- list(
    n.cure.t.out = n.cure.t.out, 
    n.cure.out = n.cure.out, 
    pr.c.h.out = pr.c.h.out)
  
  pred.tbl.ncure <- data.frame(
    month.1 = n.cure.t.pred.1, month.2 = n.cure.t.pred.2, month.3 = n.cure.t.pred.3, 
    month.4 = n.cure.t.pred.4, month.5 = n.cure.t.pred.5, month.6 = n.cure.t.pred.6)
  pred.tbl.fail <- data.frame(
    month.1 = pr.u.f.pred.1, month.2 = pr.u.f.pred.2, month.3 = pr.u.f.pred.3, 
    month.4 = pr.u.f.pred.4, month.5 = pr.u.f.pred.5, month.6 = pr.u.f.pred.6)
  rownames(pred.tbl.ncure) <- rownames(pred.tbl.fail) <- as.character(pred.data$country)
  output <- list(pr.in = pr.in, pr.out = pr.out, pred.tbl.ncure = pred.tbl.ncure, pred.tbl.fail = pred.tbl.fail)
  return(output)
}

## Separationplot method for spdur
# Input: parameters, data, and sims to run
# Output: row-matrix of predicted probability quantiles
#
separationplot.spdur <- function(object, ...)
{
  require(separationplot)
  
  # Input validation
  
  # Get predicted/observed values
  
  # Format values for separationplot
  predy.in <- spdur.csp[[3]][2, ]
  predy.in <- cbind(predy.in[dur.csp$end.spell==1], dur.csp$failure[dur.csp$end.spell==1])

  # Separationplot call
  plot <- separationplot(predy.in[,1],predy.in[,2],
                 shuffle=T, heading='', show.expected=T, newplot=F, 
                 type='line', lwd1=5, lwd2=2)
  return(plot)
}