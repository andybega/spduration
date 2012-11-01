\name{spdur.default}
\alias{spdur.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Split-Pop Duration Model Default Method
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
spdur.default(Y, X, Z, distr, max.iter = 100)
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
  \item{distr}{
%%     ~~Describe \code{distr} here~~
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
function (Y, X, Z, distr, max.iter = 100) 
{
    if (distr == "weibull") {
        est <- spweibull(Y, X, Z, max.iter)
    }
    if (distr == "loglog") {
        est <- sploglog(Y, X, Z, max.iter)
    }
    varnames <- c(paste(unlist(attr(X, "dimnames")[2])), paste(unlist(attr(Z, 
        "dimnames")[2])), "alpha")
    attr(est$coefficients, "names") <- varnames
    colnames(est$vcv) <- rownames(est$vcv) <- varnames
    est$se <- sqrt(diag(est$vcv))
    est$zstat <- with(est, coefficients/se)
    est$pval <- 2 * (1 - pnorm(abs(est$zstat)))
    est$call <- match.call()
    est$distr <- distr
    class(est) <- "spdur"
    return(est)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
