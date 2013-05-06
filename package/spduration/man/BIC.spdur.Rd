\name{BIC.spdur}
\alias{BIC.spdur}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
BIC an spdur Object
}
\description{
Computes the Bayesian Information Criterion for an \code{spdur} class object.
}
\usage{
\method{BIC}{spdur}(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
an object inheriting from class \code{spdur}.
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
Computed as \code{AIC(object, k = log(nobs(object)))}.
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
\code{\link{BIC}}, \code{\link{AIC.spdur}}
}
\examples{
data(model.ins)
BIC(model.ins)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
