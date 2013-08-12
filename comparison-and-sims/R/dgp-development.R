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

set.seed(1)
N <- 100
t <- 0:(N-1)
alpha <- 1
p <- -log(alpha)
x <- matrix(rep(1, N))  #rnorm(100, mean=-3, sd=0.1)
b <- matrix(2) #rnorm(1)
lambda <- exp(-x%*%b)
ht <- alpha * lambda^alpha * t^(alpha - 1)
p <- runif(N)
d <- (ht > p)
spell <- data[1:match(1, d)]

# different approach
# seems to give the same


GenSpell <- function(t.max, alpha, b, seed) {
  set.seed(seed)
  t <- seq(0, (t.max - 1))
  x1 <- rep(1, t.max)
  lambda <- exp(-cbind(rep(1, t.max), x1) %*% b)
  y.true <- rweibull(t.max, shape=alpha, scale=1/lambda)
  d <- as.integer(t>=y.true)
  data <- data.frame(t, x1, lambda, y.true, d)
  spell <- data[1:match(1, data$d), ]
  print(spell)
  return(max(spell$t))
}

t.max <- 100
alpha <- 2
p <- -log(alpha)
b <- matrix(c(0, 2), ncol=1) #rnorm(1)
GenSpell(t.max, alpha, b, 6)

seeds <- as.integer(runif(1000, min=1, max=100000))

results <- sapply(seeds, GenSpell, t.max=t.max, alpha=alpha, b=b)
hist(rweibull(t.max, shape=alpha, scale=1/lambda))
hist(results)

# Different parametrizations ----------------------------------------------
# Looks like MG's notes are different, mainly in that they have (lt)^p instead
# of l*t^p. So, are there parentheses or not? In practice they may not affect
# predictive performance, just comparison of models, and interpretation.

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
plot(ft(0:100, lambda=0.1, p=-log(1)), type="l")
plot(ht(0:100, lambda=0.1, p=-log(1)), type="l")

# Survival parametrization
ht <- function(t, lambda, p) {
  p * lambda * t^(p-1)
}
ft <- function(t, lambda, p) {
  p * lambda * t^(p-1) * exp(-lambda * t^(p))
}
plot(ft(0:500, lambda=0.1, p=1.5), type="l")
plot(ht(0:500, lambda=0.1, p=1.5), type="l")

# now in MG style
# Survival parametrization
ht <- function(t, lambda, p) {
  p * lambda^p * t^(p-1)
}
ft <- function(t, lambda, p) {
  p * lambda^p * t^(p-1) * exp(-(lambda*t)^p)
}
plot(ft(0:500, lambda=0.1, p=1.5), type="l")
plot(ht(0:500, lambda=0.1, p=1.5), type="l")