\name{forecast.spdur}
\alias{forecast.spdur}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Forecast with split-duration model estimates
}
\description{
\code{forecast} method for class ``\code{spdur}''.
}
\usage{
\method{forecast}{spdur}(object, \dots, pred.data = NULL, stat = 'conditional hazard', npred = 6)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
And object of class \code{spdur}.
}
  \item{\dots}{
Optional parameters passed to forecast method.
}
  \item{pred.data}{
A data frame from which forecasts are made. Should be slice of last time period observed.
}
  \item{stat}{
Type of forecast quantity, e.g. probability of being ``at risk''.
}
  \item{npred}{
How many time periods to predict forward. Default is 6.
}

}
\details{
This function calculates out-of-sample forecasts of various split-duration model related 
quantities of interest when information on the model covariates is not available. If covariates
are observed for the period for which you want to create predictions, use \code{\link{predict.spdur}}
instead.
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
Andreas Beger, Daina Chiba
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
function (object, npred = 1, ...) 
{
    NULL
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
