\name{print.summary.spdur}
\alias{print.summary.spdur}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Display split-population duration model result summary
}
\description{
\code{plot} method for class ``\code{summary, spdur}''.
}
\usage{
print.summary.spdur(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A object of class \code{summary, spdur}.
}
  \item{\dots}{
Optional arguments passed to the \code{print} method.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
None, prints coefficient matrix.
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
\code{\link{summary.spdur}}
}
\examples{
data(model.ins)
summary(model.ins)
class(summary(model.ins))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spdur}
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
