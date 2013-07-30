# Playing around to get dgp right
# Andreas Beger
# July 2013

# Setup -------------------------------------------------------------------

library(survival)

# Basic Weibull regression ------------------------------------------------

# Source:
#browseURL("http://www-stat.ucdavis.edu/~hiwang/teaching/10fall/R_tutorial%202.pdf")

set.seed(1)
N <- 1000
n.param <- 3
x <- cbind(x1=rnorm(N, mean=2, sd=0.5), x2=rnorm(N, mean=2, sd=0.5))
b <- rnorm(n.param, mean=1, sd=0.3)
xb <- cbind(rep(1, N), x) %*% b
mu <- exp(xb)
y <- rweibull(N, shape=5, scale=mu)

model <- survreg(Surv(y, rep(1, N)) ~ x, dist="weibull")
summary(model)


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
summary(model)

# Weibull regression with TVC ---------------------------------------------

