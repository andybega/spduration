\name{spdurCrisp}
\alias{spdurCrisp}
\title{Split-Pop Duration Model Wrapper for CRISP/ICEWS}
\usage{
spdurCrisp(duration, atrisk, train = train, test = test, pred = pred,
  last = "end.spell", t.0 = "t.0", distr = "weibull",
  stat = "conditional risk", iter = 100, npred = 6, ...)
}
\arguments{
  \item{duration}{A formula of the form Y ~ X1 + X2 \dots,
  where Y is duration until failure or censoring.}

  \item{atrisk}{A formula of the form C ~ Z1 + Z2 \dots,
  where C is a binary indicator of risk (1 - cure).}

  \item{train}{Object containing the training data used to
  estimate the model.}

  \item{test}{Object containing the test data used to
  validate the model.}

  \item{pred}{Object containing the data to use for
  forecasting.}

  \item{last}{A string identifying the vector in
  \code{data} that indicates when a spell ends due to
  failure or right-censoring.}

  \item{t.0}{The starting point for time-varying covariate
  intervals, by default \code{duration-1} when using
  \code{\link{buildDuration}}.}

  \item{distr}{The type of distribution to use in the
  hazard rate. Valid options are ``weibull'' or
  ``loglog''.}

  \item{stat}{See \code{\link[spduration]{predict.spdur}}}

  \item{iter}{Maximum number of iterations to use in the
  likelihood maximization.}

  \item{npred}{Number of months to predict out.}

  \item{\dots}{Optional arguments; not used currently.}
}
\value{
Returns an object of class \code{crisp}, which inherits
from \code{spdur}.
}
\description{
This is a wrapper for the CRISP and ICEWS packages that
replicated the original functionality of the \code{spdur}
command by estimating a model, calculating training and
validation set fitted values and 6-month forecasts.
}
\examples{
\dontrun{
# Prepare data
data(coups)
dur.coups <- buildDuration(coups, "succ.coup", unitID="gwcode", tID="year",
                           freq="year")

# Estimate model
model3 <- spdurCrisp(
  duration ~ polity2,
  atrisk ~ polity2,
  train=dur.coups, test=dur.coups[1,],
  pred=dur.coups[1,])
}
}
\author{
Andreas Beger
}
\seealso{
\code{\link{spdur}}
}

