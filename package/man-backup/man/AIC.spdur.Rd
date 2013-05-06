\name{AIC.spdur}
\alias{AIC.spdur}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
AIC an spdur Object
}
\description{
Computes the Akaike Information Criterion for an \code{spdur} class object.
}
\usage{
\method{AIC}{spdur}(object, ..., k = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
an object inheriting from class \code{spdur}.
}
  \item{\dots}{
not used for this method.
}
  \item{k}{
the penalty parameter, by default 2.
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
\code{\link{AIC}}, \code{\link{BIC.spdur}}
}
\examples{
data(model.ins)
AIC(model.ins)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
