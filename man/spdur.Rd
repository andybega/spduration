\name{spdur}
\alias{spdur}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Split-population duration (cure) regression
}
\description{
This function estimates a split-population duration model and returns a ``spdur'' class object.
}
\usage{
spdur(duration, atrisk, data = list(), last, distr = "weibull", max.iter = 100, ...)
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
A vector of binary indicators that equal 1 if failure occurs or the spell is right-censored.
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
See \code{\link{summary.spdur}}, \code{\link{predict.spdur}} and \code{\link{separationplot.spdur}}
}
\value{
\item{coefficients }{A named vector of coefficient point estimates.}
\item{vcv }{Estimated covariance matrix.}
\item{se }{Standard error estimates.}
\item{distr }{Distribution used for the hazard rate.}
\item{call }{The original spdur call.}
...
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
#data(xx)

#model <-
#  spdur()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
