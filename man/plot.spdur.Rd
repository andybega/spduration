\name{plot.spdur}
\alias{plot.spdur}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot method for \code{spdur} class object
}
\description{
Creates a separation plot of risk/cure probabilities.
}
\usage{
plot.spdur(object, failure = "failure", endSpellOnly = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
%%     ~~Describe \code{object} here~~
}
  \item{failure}{
The variable indicating that a failure event has occurred at time \code{t}.
}
  \item{endSpellOnly}{
Should only the last observation in each spell be kept? \code{TRUE} by default.
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (object, ...) 
{
    require(separationplot)
    predy.in <- spdur.csp[[3]][2, ]
    predy.in <- cbind(predy.in[dur.csp$end.spell == 1], dur.csp$failure[dur.csp$end.spell == 
        1])
    plot <- separationplot(predy.in[, 1], predy.in[, 2], shuffle = T, 
        heading = "", show.expected = T, newplot = F, type = "line", 
        lwd1 = 5, lwd2 = 2)
    return(plot)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line