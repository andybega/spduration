\name{spdur}
\alias{spdur}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Split-population duration (cure) regression
}
\description{
This function estimates a split-population duration model and returns a object of class ``\code{spdur}''.
}
\usage{
spdur(duration, atrisk, data = NULL, last, distr = "weibull", max.iter = 100, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{duration}{
A formula of the form Y ~ X1 + X2 \dots, where Y is duration until failure or censoring.
}
  \item{atrisk}{
A formula of the form C ~ Z1 + Z2 \dots, where C is a binary indicator of risk (1 - cure).
}
  \item{data}{
A data frame containing the variables in formula and formula2.
}
  \item{last}{
A string identifying the vector in \code{data} that indicates when a spell ends due to failure or right-censoring.
}
  \item{distr}{
The type of distribution to use in the hazard rate. Valid options are ``weibull'' or ``loglog''.
}
  \item{max.iter}{
Maximum number of iterations to use in the likelihood maximization.
}
  \item{\dots}{
Additional parameters passed to spdur.
}
}
\details{
See \code{\link{summary.spdur}}, \code{\link{predict.spdur}} and \code{\link{plot.spdur}}
}
\value{
Returns an object of class \code{spdur}, with attributes:
\item{coefficients }{A named vector of coefficient point estimates.}
\item{vcv }{Estimated covariance matrix.}
\item{se }{Standard error estimates.}
\item{zstat }{Z-statistic values.}
\item{pval }{P-values.}
\item{call }{The original, unevaluated \code{spdur} call.}
\item{distr }{Distribution used for the hazard rate.}
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Andreas Beger
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# Prepare data
data(insurgency)
duration.ins <- buildDuration(insurgency, 'insurgency', unitID='ccode', tID='date')

# Estimate model
model <- spdur(
  duration ~ low_intensity + high_neighbors + exclpop.l1,
  atrisk ~ excl_groups_count.l1 + high_neighborhood + high_intensity + exclpop.l1 + lgdppc.l1,
  last='end.spell', data=duration.ins, distr="weibull", max.iter=300)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spduration}
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
