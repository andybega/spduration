\name{panelLag}
\alias{panelLag}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Lag panel data
}
\description{
A function that correctly lags panel data where units are identified by \code{id} and
time periods are identified with \code{t}.
}
\usage{
panelLag(x, id, t, lag = 1, data = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{id}{
%%     ~~Describe \code{id} here~~
}
  \item{t}{
%%     ~~Describe \code{t} here~~
}
  \item{lag}{
%%     ~~Describe \code{lag} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
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
function (x, id, t, lag = 1, data = NULL) 
{
    if (is.null(data) == T) 
        data <- data.frame(x = x, id = id, t = t)
    k <- lag
    lags <- function(x, k) {
        c(rep(NA, k), x[1:(length(x) - k)])
    }
    data <- data[order(data[, id], data[, t]), ]
    result <- unlist(by(data[, x], data[, id], lags, k = k))
    return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
