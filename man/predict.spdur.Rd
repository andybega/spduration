\name{predict.spdur}
\alias{predict.spdur}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Predict fitted values for spdur object
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
predict.spdur(object, data = NULL, stat = cure, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{object} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{stat}{
%%     ~~Describe \code{stat} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (object, data = NULL, stat = cure, ...) 
{
    if (is.null(data)) {
        data <- get(paste(object$call$data), pos = 1)
    }
    mf.dur <- model.frame(formula = object$call$duration, data = data)
    X <- model.matrix(attr(mf.dur, "terms"), data = mf.dur)
    lhb <- model.response(mf.dur)
    mf.risk <- model.frame(formula = object$call$atrisk, data = data)
    Z <- model.matrix(attr(mf.risk, "terms"), data = mf.risk)
    lhg <- model.response(mf.risk)
    censor <- data[, as.character(object$call$last)[3]]
    Y <- cbind(atrisk = lhg, duration = lhb, last = censor)
    if (object$distr == "weibull") {
        res <- pred_weibull(coef = coef(object), vcv = object$vcv, 
            Y = Y, X = X, Z = Z, stat)
    }
    if (object$distr == "loglog") {
        res <- pred_loglog(coef = coef(object), vcv = object$vcv, 
            Y = Y, X = X, Z = Z, stat)
    }
    return(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
