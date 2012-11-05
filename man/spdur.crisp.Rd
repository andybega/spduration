\name{spdur.crisp}
\alias{spdur.crisp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Split-Pop Duration Model Wrapper for CRISP/ICEWS
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
spdur.crisp(duration = formula, atrisk = formula2, data = NULL, test = NULL, last = NULL, distr = NULL, stat = cure, iter = 100, ...)
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
  \item{test}{
%%     ~~Describe \code{test} here~~
}
  \item{last}{
%%     ~~Describe \code{last} here~~
}
  \item{distr}{
%%     ~~Describe \code{distr} here~~
}
  \item{stat}{
%%     ~~Describe \code{stat} here~~
}
  \item{iter}{
%%     ~~Describe \code{iter} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns an object of class \code{crisp}, which inherits from \code{spdur}.
\item{model }{Model estimates with class \code{spdur}.}
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
function (duration = formula, atrisk = formula2, data = NULL, 
    test = NULL, last = NULL, distr = NULL, stat = cure, iter = 100, 
    ...) 
{
    if (is.null(data)) 
        stop("No data provided")
    if (is.null(last)) 
        stop("Must specify censoring variable")
    if (distr == NULL) 
        stop("Must specify distribution")
    model <- spdur(duration, atrisk, data = data, last, distr = distr, 
        max.iter = 100)
    train <- predict(model, stat = stat)
    test <- predict(model, data = test, stat = stat)
    pred <- forecast(model, npred = 6)
    print(model)
    res <- model
    res$train <- train
    res$test <- test
    res$pred <- pred
    class(res) <- c("crisp", "spdur")
    return(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
