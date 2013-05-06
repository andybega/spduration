\name{sploglog}
\alias{sploglog}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fit Split-Pop LogLog Model
}
\description{
For internal use only.
}
\usage{
sploglog(Y, X, Z, max.iter)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
%%     ~~Describe \code{y} here~~
}
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{Z}{
%%     ~~Describe \code{Z} here~~
}
  \item{max.iter}{
%%     ~~Describe \code{y.test} here~~
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
function (y, X, Z, y.test, X.test, Z.test, iter, sims) 
{
    X <- as.matrix(cbind(1, X))
    Z <- as.matrix(cbind(1, Z))
    X.test <- as.matrix(cbind(1, X.test))
    Z.test <- as.matrix(cbind(1, Z.test))
    base <- optim(c(rep(1, ncol(X) + 1)), loglog.lik, method = "BFGS", 
        control = list(maxit = iter), hessian = T, y = y, X = X)
    x.inits <- base$par[1:ncol(X)]
    a.init <- base$par[ncol(X) + 1]
    est <- optim(c(x.inits, rep(1, ncol(Z)), a.init), sploglog.lik, 
        method = "BFGS", control = list(trace = T, maxit = iter), 
        hessian = T, y = y, X = X, Z = Z)
    if (est$convergence != 0) 
        stop("Model did not converge")
    coeff <- est$par
    vcv <- solve(est$hessian)
    se <- sqrt(diag(vcv))
    zstat <- coeff/se
    pval <- 2 * (1 - pnorm(abs(zstat)))
    print(results <- cbind(coeff, se, zstat, pval))
    X.train <- as.matrix(X[train$last.observation == 1, ])
    Z.train <- as.matrix(Z[train$last.observation == 1, ])
    y.train <- as.matrix(y[train$last.observation == 1, ])
    X <- as.matrix(X.test[test$last.observation == 1, ])
    Z <- as.matrix(Z.test[test$last.observation == 1, ])
    y <- as.matrix(y.test[test$last.observation == 1, ])
    reps <- sims
    cyhat.train <- matrix(NA, nrow(X.train), reps)
    cyhat <- matrix(NA, nrow(X), reps)
    for (i in 1:reps) {
        draw <- mvrnorm(1, coeff, vcv)
        lxbeta <- X.train \%*\% draw[1:ncol(X.train)]
        pxbeta <- Z.train \%*\% draw[(ncol(X.train) + 1):(ncol(X.train) + 
            ncol(Z.train))]
        alph <- 1 * draw[ncol(X.train) + ncol(Z.train) + 1]
        alph <- exp(-alph)
        lyhat <- exp(-lxbeta)
        pyhat <- 1/(1 + exp(-pxbeta))
        cyhat.train[, i] <- pyhat/(1/(1 + (lyhat * y.train[, 
            2])^alph) + pyhat * (1 - 1/(1 + (lyhat * y.train[, 
            2])^alph)))
        lxbeta <- X \%*\% draw[1:ncol(X)]
        pxbeta <- Z \%*\% draw[(ncol(X) + 1):(ncol(X) + ncol(Z))]
        alph <- 1 * draw[ncol(X) + ncol(Z) + 1]
        alph <- exp(-alph)
        lyhat <- exp(-lxbeta)
        pyhat <- 1/(1 + exp(-pxbeta))
        cyhat[, i] <- pyhat/(1/(1 + (lyhat * y[, 2])^alph) + 
            pyhat * (1 - 1/(1 + (lyhat * y[, 2])^alph)))
    }
    predvalues.train <- apply(cyhat.train, 1, quantile, probs = c(0.025, 
        0.5, 0.975), na.rm = T)
    predvalues <- apply(cyhat, 1, quantile, probs = c(0.025, 
        0.5, 0.975), na.rm = T)
    invisible(return(list(results, vcv, predvalues.train, predvalues, 
        pyhat)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
