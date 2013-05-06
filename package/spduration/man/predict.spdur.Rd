\name{predict.spdur}
\alias{predict.spdur}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Predict fitted values for a split-population duration model
}
\description{
\code{predict} method for class ``\code{spdur}''.
}
\usage{
\method{predict}{spdur}(object, data = NULL, stat = "conditional risk", \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Object of class ``\code{spdur}''.
}
  \item{data}{
Optional data for which to calculate fitted values, defaults to training data.
}
  \item{stat}{
Quantity of interest to calculate. Default conditional probability of being at risk, i.e. 
conditioned on observed survival up to time \code{t}. See below for list of values.
}
  \item{\dots}{
Optional arguments to pass to \code{predict} function.
}
}
\details{
Calculates various types of probabilities, where ``conditional'' is used in reference to
conditioning on the observed survival time of a spell up to time \eqn{t}, in addition to
conditioning on any variables included in the model (which is always done). Valid values 
for the \code{stat} option include:
\itemize{
	\item ``conditional risk'': \eqn{Pr(Cure=0|Z\gamma, T>t)}{Pr(Cure=0|Z*gamma, T>t)}
	\item ``conditional cure'': \eqn{Pr(Cure=1|Z\gamma, T>t)}{Pr(Cure=1|Z*gamma, T>t)}
	\item ``hazard'': \eqn{Pr(T=t|T>t, C=0, X\beta) * Pr(Cure=0|Z\gamma)}{Pr(T=t|T>t, C=0, X*beta) * Pr(Cure=0|Z*gamma)}
	\item ``failure'': \eqn{Pr(T=t|T>t-1, C=0, X\beta) * Pr(Cure=0|Z\gamma)}{Pr(T=t|T>t-1, C=0, X*beta) * Pr(Cure=0|Z*gamma)}
	\item ``unconditional risk'': \eqn{Pr(Cure=0|Z\gamma)}{Pr(Cure=0|Z*gamma)}
	\item ``unconditional cure'': \eqn{Pr(Cure=1|Z\gamma)}{Pr(Cure=1|Z*gamma)}
	\item ``conditional hazard'': \eqn{Pr(T=t|T>t, C=0, X\beta) * Pr(Cure=0|Z\gamma, T>t)}{Pr(T=t|T>t, C=0, X*beta) * Pr(Cure=0|Z*gamma, T>t)}
	\item ``conditional failure'': \eqn{Pr(T=t|T>t-1, C=0, X\beta) * Pr(Cure=0|Z\gamma, T>t)}{Pr(T=t|T>t-1, C=0, X*beta) * Pr(Cure=0|Z*gamma, T>t)}
	}
The vector \eqn{Z\gamma}{Z*gamma} indicates the cure/at risk equation covariate vector, while 
\eqn{X\beta}{X*beta} indicates the duration equation covariate vector.
}
\value{
Returns a data frame with 1 column corresponding to \code{stat}, in the same order as the
data frame used to estimate \code{object}.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Andreas Beger, Daina Chiba, Daniel W. Hill, Nils Metternich
}
\note{
See \code{\link{forecast.spdur}} for producing forecasts when future covariate values
are unknown.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# build data
data(insurgency)
duration.ins <- buildDuration(insurgency, 'insurgency', unitID='ccode', tID='date')

# get model estimates
data(model.ins)
atrisk <- predict(model.ins)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
