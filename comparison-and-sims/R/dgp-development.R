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
x <- cbind(x1=rnorm(N), x2=rnorm(N))
b <- rnorm(n.param)
xb <- cbind(rep(1, N), x) %*% b
mu <- exp(xb)
y <- rweibull(N, shape=2, scale=mu)
model <- survreg(Surv(y, rep(1, N)) ~ x, dist="weibull")
summary(model)


# Weibull regression with censoring ---------------------------------------


# Weibull regression with TVC ---------------------------------------------

