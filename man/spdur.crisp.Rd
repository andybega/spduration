\name{spdur.crisp}
\alias{spdur.crisp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Split-Pop Duration Model Wrapper for CRISP/ICEWS
}
\description{
This is a wrapper for the CRISP and ICEWS packages that replicated the original functionality
of the \code{spdur} command by estimating a model, calculating training and validation set fitted values,
and 6-month forecasts.
}
\usage{
spdur.crisp(duration, atrisk, train = NULL, test = NULL, pred = NULL, last = NULL, distr = NULL, stat = 'atrisk', iter = 100, npred = 6, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{duration}{
%%     ~~Describe \code{duration} here~~
}
  \item{atrisk}{
%%     ~~Describe \code{atrisk} here~~
}
  \item{train}{
Training data.
}
  \item{test}{
Validation data (out-of-sample).
}
  \item{pred}{
A data frame from which forecasts are made. Should be slice of last time period observed.
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
Maximum iterations to use in optimization.
}
  \item{npred}{
Number of months to predict out.
}
  \item{\dots}{
Optional arguments; not used currently.
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
Andreas Beger
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{spdur}}
}
\examples{
# Prepare data
data(insurgency)
duration.ins <- buildDuration(insurgency, 'insurgency', unitID='ccode', tID='date')

# Estimate model
model3 <- spdur.crisp(
  duration ~ low_intensity + high_neighbors + exclpop.l1,
  atrisk ~ excl_groups_count.l1 + high_neighborhood + high_intensity + exclpop.l1 + lgdppc.l1,
  last='end.spell', train=duration.ins, test=duration.ins[1,], pred=duration.ins[1,], distr="weibull", iter=300)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
