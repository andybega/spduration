\name{pred_weibull}
\alias{pred_weibull}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Predict fitted values for Weibull model
}
\description{
For internal use only.
}
\usage{
pred_weibull(coef, vcv, Y, X, Z, stat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{coef}{
%%     ~~Describe \code{coef} here~~
}
  \item{vcv}{
%%     ~~Describe \code{vcv} here~~
}
  \item{Y}{
%%     ~~Describe \code{Y} here~~
}
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{Z}{
%%     ~~Describe \code{Z} here~~
}
  \item{stat}{
%%     ~~Describe \code{stat} here~~
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
function (coef, vcv, Y, X, Z, stat) 
{
    require(MASS)
    coeff.b <- coef[1:ncol(X)]
    coeff.g <- coef[(ncol(X) + 1):(ncol(X) + ncol(Z))]
    coeff.a <- coef[(ncol(X) + ncol(Z) + 1)]
    al.hat <- exp(-coeff.a)
    la.hat <- exp(-X \%*\% coeff.b)
    n.cure <- plogis(Z \%*\% coeff.g)
    cure <- 1 - n.cure
    st <- exp(-(la.hat * Y[, 2])^al.hat)
    s0 <- exp(-(la.hat * (Y[, 2] - 1))^al.hat)
    cure.t <- cure/(st + cure * (1 - st))
    n.cure.t <- 1 - cure.t
    ft <- la.hat * al.hat * (la.hat * Y[, 2])^(al.hat - 1) * 
        exp(-(la.hat * Y[, 2])^al.hat)
    pr.c.f <- n.cure.t * ft/s0
    pr.c.h <- n.cure.t * ft/st
    pr.u.f <- n.cure * ft/s0
    pr.u.h <- n.cure * ft/st
    res <- data.frame(n.cure.t)
    return(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
