\name{summary.spdur}
\alias{summary.spdur}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summarize split-population duration results
}
\description{
\code{summary} method for class ``\code{spdur}''.
}
\usage{
\method{summary}{spdur}(object, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
An object with class \code{spdur}
}
  \item{\dots}{
Additional options passed to \code{summary} function.
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(model.ins)
s <- summary(model.ins)
class(s)
print(s)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spdur}
\keyword{summary}% __ONLY ONE__ keyword per line
