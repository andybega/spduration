\name{spdur.icews}
\alias{spdur.icews}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Split-Pop Duration Model Wrapper for CRISP/ICEWS
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
spdur.icews(formula, cure = formula2, data = NULL, test = NULL, last = NULL, distr = "", re = "", iter = NULL, sims = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
%%     ~~Describe \code{formula} here~~
}
  \item{cure}{
%%     ~~Describe \code{cure} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{test}{
%%     ~~Describe \code{test} here~~
}
  \item{last}{
%%     ~~Describe \code{last} here~~
}
  \item{distr}{
%%     ~~Describe \code{distr} here~~
}
  \item{re}{
%%     ~~Describe \code{re} here~~
}
  \item{iter}{
%%     ~~Describe \code{iter} here~~
}
  \item{sims}{
%%     ~~Describe \code{sims} here~~
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
function (formula, cure = formula2, data = NULL, test = NULL, 
    last = NULL, distr = "", re = "", iter = NULL, sims = NULL) 
{
    if (is.null(data)) 
        stop("No data provided")
    if (is.null(last)) 
        stop("Must specify censoring variable")
    if (distr == "") 
        stop("Must specify distribution")
    a <- as.character(formula)
    b <- as.character(cure)
    lhb <- a[2]
    rhb <- strsplit(a[3], split = " + ", fixed = T)[[1]]
    lhg <- b[2]
    rhg <- strsplit(b[3], split = " + ", fixed = T)[[1]]
    X <- data[rhb]
    Z <- data[rhg]
    Y <- cbind(data[lhg], data[lhb], last)
    X.test <- test[rhb]
    Z.test <- test[rhg]
    Y.test <- cbind(test[lhg], test[lhb])
    if (is.null(iter)) 
        iter <- 100
    if (is.null(sims)) 
        sims <- 1000
    if (distr == "weibull") {
        model <- spweibull(Y, X, Z, Y.test, X.test, Z.test, iter, 
            sims)
    }
    if (distr == "loglog") {
        model <- sploglog(Y, X, Z, Y.test, X.test, Z.test, iter, 
            sims)
    }
    rownames(model$results) <- c("duration const", rhb, "risk constant", 
        rhg, "alpha")
    print(model$results, digits = 4)
    invisible((model))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
