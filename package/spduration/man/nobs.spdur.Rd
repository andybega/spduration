\name{nobs.spdur}
\alias{nobs.spdur}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract the number of observations from a spdur object.
}
\description{
Extract the number of observations from an object of class \code{spdur}. Used when 
calculating AIC and BIC.
}
\usage{
\method{nobs}{spdur}(object, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
object of class \code{spdur}.
}
\item{\dots}{
not used in this method.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
\code{\link{nobs}}
}
\examples{
data(model.ins)
nobs(model.ins)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
