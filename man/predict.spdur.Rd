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
\method{predict}{spdur}(object, data = NULL, stat = "atrisk", \dots)
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
Quantity of interest to calculate. Default is probability of being at risk.
}
  \item{\dots}{
Optional arguments to pass to \code{predict} function.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a data frame with 1 column corresponding to \code{stat}.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Andreas Beger, Daina Chiba, Daniel W. Hill, Nils Metternich
}
\note{
%%  ~~further notes~~
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
