\name{logLik.spdur}
\alias{logLik.spdur}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Log-Likelihood of an spdur Object
}
\description{
Returns the log-likelihood of the split duration model represented by \code{object} evaluated 
at the estimated coefficients.
}
\usage{
\method{logLik}{spdur}(object, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
an object inheriting from class \code{spdur}.
}
  \item{\dots}{
not used in this method.
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
function (object) 
{
    p <- nobs(object) - length(object$coefficients)
    val <- object$logL
    attr(val, "nobs") <- nobs(object)
    attr(val, "df") <- p
    class(val) <- "logLik"
    return(val)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
