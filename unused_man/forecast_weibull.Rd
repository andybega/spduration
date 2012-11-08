\name{forecast_weibull}
\alias{forecast_weibull}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Forecast with weibull spdur model.
}
\description{
For internal use only.
}
\usage{
forecast_weibull(coef, vcv, Y, X, Z, stat, npred)
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
  \item{npred}{
%%     ~~Describe \code{npred} here~~
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
Andreas Beger
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
function (coef, vcv, Y, X, Z, stat, npred) 
{
    coeff.b <- coef[1:ncol(X)]
    coeff.g <- coef[(ncol(X) + 1):(ncol(X) + ncol(Z))]
    coeff.a <- coef[(ncol(X) + ncol(Z) + 1)]
    al.hat <- exp(-coeff.a)
    la.hat.pred <- exp(-X \%*\% coeff.b)
    n.cure.pred <- plogis(Z \%*\% coeff.g)
    cure.pred <- 1 - n.cure.pred
    rows <- length(la.hat.pred)
    s0 <- exp(-(la.hat.pred * (Y[, 2] - 1))^al.hat)
    st <- matrix(nrow = rows, ncol = npred)
    cure.t <- matrix(nrow = rows, ncol = npred)
    for (i in 1:npred) {
        st[, i] <- exp(-(la.hat.pred * (Y[, 2] + (i - 1)))^al.hat)
        cure.t[, i] <- cure.pred/(st[, i] + cure.pred * (1 - 
            st[, i]))
    }
    n.cure.t <- 1 - cure.t
    if (stat == "cure") 
        res <- cure.t
    if (stat == "atrisk") 
        res <- n.cure.t
    if (!stat \%in\% c("cure", "atrisk")) {
        ft <- matrix(nrow = rows, ncol = npred)
        u.f <- matrix(nrow = rows, ncol = npred)
        for (i in 1:npred) {
            ft[, i] <- la.hat.pred * al.hat * (la.hat.pred * 
                (Y[, 2] + (i - 1)))^(al.hat - 1) * exp(-(la.hat.pred * 
                (Y[, 2] + (i - 1)))^al.hat)
            if (i == 1) 
                u.f[, i] <- n.cure.t[, i] * ft[, i]/s0
            if (i > 1) 
                u.f[, i] <- n.cure.t[, i] * ft[, i]/st[, (i - 
                  1)]
        }
        if (stat == "u.f") 
            res <- u.f
    }
    return(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
