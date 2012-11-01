\name{spdur.formula}
\alias{spdur.formula}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Split-Pop Duration Model Formula Method
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
spdur.formula(duration, atrisk, data = list(), last, distr = "weibull", max.iter = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{duration}{
%%     ~~Describe \code{duration} here~~
}
  \item{atrisk}{
%%     ~~Describe \code{atrisk} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{last}{
%%     ~~Describe \code{last} here~~
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
function (duration, atrisk, data = list(), last, distr = "weibull", 
    max.iter = 100) 
{
    mf.dur <- model.frame(formula = duration, data = data)
    X <- model.matrix(attr(mf.dur, "terms"), data = mf.dur)
    lhb <- model.response(mf.dur)
    mf.risk <- model.frame(formula = atrisk, data = data)
    Z <- model.matrix(attr(mf.risk, "terms"), data = mf.risk)
    lhg <- model.response(mf.risk)
    censor <- last
    Y <- cbind(atrisk = lhg, duration = lhb, last = censor)
    est <- spdur.default(Y, X, Z, distr, max.iter)
    est$call <- match.call()
    return(est)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
