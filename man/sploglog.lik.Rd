\name{sploglog.lik}
\alias{sploglog.lik}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Split-pop loglog likelihood
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sploglog.lik(theta, y, X, Z)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
%%     ~~Describe \code{theta} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{Z}{
%%     ~~Describe \code{Z} here~~
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
function (theta, y, X, Z) 
{
    rx <- ncol(X)
    rz <- ncol(Z)
    beta <- theta[1:rx]
    gamma <- theta[(rx + 1):(rx + rz)]
    g <- theta[rx + rz + 1]
    d <- y[, 1]
    ti <- y[, 2]
    t0 <- y[, 2] - 1
    ly <- y[, 3]
    lambda <- exp(-X \%*\% beta)
    alpha <- exp(-g)
    pr1 <- plogis(Z \%*\% gamma)
    pr0 <- plogis(Z \%*\% gamma, lower.tail = F)
    su <- log((1 + (lambda * t0)^g)) - log((1 + (lambda * ti)^g))
    haz <- log(g) + g * log(lambda) + (g - 1) * log(ti) - log(1 + 
        (lambda * ti)^g)
    su.exp <- exp(su)
    cens <- ifelse((d == 1) & (ly == 0) | (d == 0), log(pr0 + 
        pr1 * su.exp), 0)
    nocens <- ifelse((d == 1) & (ly == 1), log(pr1) + haz + log(su.exp), 
        0)
    logl <- sum(cens + nocens)
    return(-logl)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
