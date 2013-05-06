\name{plot.spdur}
\alias{plot.spdur}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot split-duration model fit.
}
\description{
\code{plot} method for class ``\code{spdur}''.
}
\usage{
\method{plot}{spdur}(x, \dots, failure = "failure", endSpellOnly = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object of class "\code{spdur}".
}
  \item{\dots}{
Optional parameters passed to \code{predict.spdur}, e.g. type of statistic to calculate.
}
  \item{failure}{
The variable indicating that a failure event has occurred at time \code{t}.
}
  \item{endSpellOnly}{
Should only the last observation in each spell be kept? \code{TRUE} by default.
}
}
\details{
Creates a \code{\link{separationplot}} of fitted values from split-duration model results using
\code{\link{predict.spdur}}.
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
Andreas Beger
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{separationplot}}, \code{\link{predict.spdur}}
}
\examples{
# build data
data(insurgency)
duration.ins <- buildDuration(insurgency, 'insurgency', unitID='ccode', tID='date')

# get model estimates
data(model.ins)

# plot
p <- plot(model.ins)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spdur}
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
