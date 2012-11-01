\name{buildDuration}
\alias{buildDuration}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Build duration data
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
buildDuration(data, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
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
function (data, y) 
{
    require(plyr)
    data <- data[order(data$ccode, data$date), ]
    failure <- function(x) return(c(0, pmax(0, diff(x))))
    data$failure <- unlist(by(data[, y], data$ccode, failure))
    data <- subset(data, !(get(y) == 1 & failure == 0))
    data <- ddply(data, .(ccode), transform, end = max(date))
    data$end.spell <- ifelse(format(data$date, "\%Y-\%m") == format(as.Date(data$end), 
        "\%Y-\%m"), 1, 0)
    data$end.spell <- ifelse(data$failure == 1, 1, data$end.spell)
    data$spellID <- rev(cumsum(rev(data$end.spell)))
    failedspells <- data$spellID[data$failure == 1]
    helper <- cbind(failedspells, 0)
    colnames(helper) <- c("spellID", "cured")
    data <- merge(data, helper, by = ("spellID"), all.x = TRUE)
    data$cured[is.na(data$cured)] <- 1
    data$atrisk <- 1 - data$cured
    data$censor <- ifelse(with(data, end.spell == 1 & failure == 
        0), 1, 0)
    helper <- rep(1, dim(data)[1])
    data <- data[order(data$spellID, data$date), ]
    data$duration <- unlist(by(helper, data$spellID, cumsum))
    data <- data[order(data$ccode, data$date), ]
    return(data)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
