\name{panelLag}
\alias{panelLag}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Lag panel data
}
\description{
A function that correctly lags panel data where units are identified by \code{id} and
time periods are identified with \code{t}. Results are in same order as \code{data}.
}
\usage{
panelLag(x, id, t, lag = 1, data = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Quoted string identifying the vectors to be lagged in \code{data}.
}
  \item{id}{
Quoted string identifying the unit identifier in \code{data}.
}
  \item{t}{
Quoted string identifying the time identifier in \code{data}.
}
  \item{lag}{
Lag order, i.e. by how many time periods should \code{x} be lagged?
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A vector of same length as \code{x} representing lagged values with leading \code{NA}'s.
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(insurgency)

# No need to order before using panelLag, just do it here so we can compare results below.
insurgency <- insurgency[order(insurgency$ccode, insurgency$date), ]
test <- panelLag('lgdppc.l1', 'ccode', 'date', data=insurgency)

# Compare output
head(test)
head(insurgency$lgdppc.l1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
