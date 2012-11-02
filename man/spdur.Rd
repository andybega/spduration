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
spdur(duration = formula, atrisk = formula2, data = NULL, last = NULL, distr = NULL, ...)
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
Additional parameters passed to the appropriate spdur method.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{coefficients }{A named vector of coefficient point estimates.}
\item{vcv }{Estimated covariance matrix.}
\item{se }{Standard error estimates.}
\item{distr }{Distribution used for the hazard rate.}
\item{call }{The original spdur call.}
...
}
\author{
Andreas Beger
}
\examples{
#data(xx)

#model <-
#  spdur()
}