# Playing around to get dgp right
# Andreas Beger
# July 2013

# Setup -------------------------------------------------------------------

library(survival)

# Basic Weibull regression ------------------------------------------------

# Source:
browseURL("http://www-stat.ucdavis.edu/~hiwang/teaching/10fall/R_tutorial%202.pdf")

set.seed(1)
N <- 1000
n.param <- 3
x <- cbind(x1=rnorm(N, mean=2, sd=0.5), x2=rnorm(N, mean=2, sd=0.5))
b <- rnorm(n.param, mean=1, sd=0.3)
xb <- cbind(rep(1, N), x) %*% b
mu <- exp(xb)
y <- rweibull(N, shape=5, scale=mu)

model <- survreg(Surv(y, rep(1, N)) ~ x, dist="weibull")
cbind(estimate=model$coef, dgp=b)


# Weibull regression with censoring ---------------------------------------

set.seed(1)
N <- 1000
n.param <- 3
x <- cbind(x1=rnorm(N, mean=2, sd=0.5), x2=rnorm(N, mean=2, sd=0.5))
b <- rnorm(n.param, mean=1, sd=0.3)
xb <- cbind(rep(1, N), x) %*% b
mu <- exp(xb)
y <- rweibull(N, shape=5, scale=mu)

censored.time <- rweibull(N, shape=5, scale=mean(mu))
y.cen <- pmin(y, censored.time)
di <- as.numeric(y<=censored.time)

model <- survreg(Surv(y.cen, di) ~ x, dist="weibull")
cbind(estimate=model$coef, dgp=b)

# Weibull regression with TVC ---------------------------------------------

# based on wiki
ht <- function(t, lambda, p) {
  p/lambda * (t/lambda)^(p-1)
}
ft <- function(t, lambda, p) {
  p/lambda * (t/lambda)^(p-1) * exp(-(t/lambda)^p)
}
plot(ft(0:100, lambda=10, p=exp(1)), type="l")
plot(ht(0:100, lambda=10, p=exp(1)), type="l")

# based on spdur
ht <- function(t, lambda, p) {
  exp(-p) * lambda^exp(-p) * t^(exp(-p) - 1)
}
ft <- function(t, lambda, p) {
  exp(-p) * lambda^exp(-p) * t^(exp(-p) - 1) * exp(-(lambda*t)^exp(-p))
}
plot(ft(0:100, lambda=0.1, p=-1), type="l")
plot(ht(0:100, lambda=0.1, p=-1), type="l")