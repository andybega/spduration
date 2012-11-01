\name{spweibull}
\alias{spweibull}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fit Split-Pop Weibull Model
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
spweibull(Y, X, Z, max.iter)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
%%     ~~Describe \code{Y} here~~
}
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{Z}{
%%     ~~Describe \code{Z} here~~
}
  \item{max.iter}{
%%     ~~Describe \code{max.iter} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (Y, X, Z, max.iter) 
{
    weib.lik <- function(theta, y, X) {
        beta <- theta[1:ncol(X)]
        p <- theta[ncol(X) + 1]
        d <- y[, 1]
        ti <- y[, 2]
        t0 <- y[, 2] - 1
        ly <- y[, 3]
        lambda <- exp(-X \%*\% beta)
        alpha <- exp(-p)
        haz <- log(alpha) + (alpha) * log(lambda) + (alpha - 
            1) * log(ti)
        su <- log(exp(-(lambda * ti)^alpha)/exp(-(lambda * t0)^alpha))
        cens <- ifelse((d == 1) & (ly == 0) | (d == 0), su, 0)
        nocens <- ifelse((d == 1) & (ly == 1), haz + su, 0)
        logl <- sum(cens + nocens)
        return(-logl)
    }
    base.inits <- c(rep(0, ncol(X)), 0)
    cat("Fitting base weibull...\n")
    base <- optim(base.inits, weib.lik, method = "BFGS", control = list(maxit = max.iter), 
        hessian = T, y = Y, X = X)
    spweib.lik <- function(theta, y, X, Z) {
        rx <- ncol(X)
        rz <- ncol(Z)
        beta <- theta[1:rx]
        gamma <- theta[(rx + 1):(rx + rz)]
        p <- theta[rx + rz + 1]
        d <- y[, 1]
        ti <- y[, 2]
        t0 <- y[, 2] - 1
        ly <- y[, 3]
        lambda <- exp(-X \%*\% beta)
        alpha <- exp(-p)
        pr1 <- plogis(Z \%*\% gamma)
        pr0 <- plogis(Z \%*\% gamma, lower.tail = F)
        haz <- log(alpha) + (alpha) * log(lambda) + (alpha - 
            1) * log(ti)
        su <- log(exp(-(lambda * ti)^alpha)/exp(-(lambda * t0)^alpha))
        su.exp <- exp(su)
        cens <- ifelse((d == 1) & (ly == 0) | (d == 0), log(pr0 + 
            pr1 * su.exp), 0)
        nocens <- ifelse((d == 1) & (ly == 1), log(pr1) + haz + 
            log(su.exp), 0)
        logl <- sum(cens + nocens)
        return(-logl)
    }
    x.inits <- base$par[1:ncol(X)]
    a.init <- base$par[ncol(X) + 1]
    cat("Fitting split weibull...\n")
    est <- optim(c(x.inits, rep(0, ncol(Z)), a.init), spweib.lik, 
        method = "BFGS", control = list(trace = T, maxit = max.iter), 
        hessian = T, y = Y, X = X, Z = Z)
    if (est$convergence != 0) 
        stop("Model did not converge")
    coef <- est$par
    vcv <- solve(est$hessian)
    return(list(coefficients = coef, vcv = vcv))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
