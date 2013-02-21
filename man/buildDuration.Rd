\name{buildDuration}
\alias{buildDuration}

\title{
Build duration version of panel data
}

\description{
Builds a duration version of a data frame representing panel data.
}

\usage{
buildDuration(data, y, unitID, tID, freq="monthly", slice.last = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Data frame representing panel data.
}
  \item{y}{
A binary indicator of the incidence of some event, e.g. insurgency.
}
  \item{unitID}{
Name of the variable in the data frame identifying the cross-sectional units, e.g. countries.
}
  \item{tID}{
Name of the variable in the data frame identifying the time unit, in date format.
}
  \item{freq}{
Frequency by which data is observed in \code{tID}, i.e. "\code{monthly}" or "\code{yearly}".  
}
  \item{slice.last}{
Set to \code{TRUE} to create a slice of the last time period; used with \code{\link{forecast.spdur}}. For compatibility with CRISP and ICEWS projects.
}
}

\details{
This function processes a panel data frame by creating a failure variable from \code{y} and corresponding
duration counter, as well as risk/immunity indicators. In case \code{y} already is a binary indicator
of failure, the returned data frame should have the same number of rows at the original. If \code{y} is
an indicator of the incidence of some event, then ongoing spells of failure beyond the initial event are dropped
(e.g. 000111 becomes a spell of 0001), and the number of observation in the returned data frame may be lower.

There cannot be missing values ("\code{NA}") in any of the key variables specified in the function call above; they will stop the function.

Furthermore, series that start with an event, e.g. (100), are treated as experiencing failure in the first time period. If those events are in fact ongoing, e.g. the last year of a war that started before the start time of the dataset, they should be dropped manually before using \code{buildDuration()}.
}

\value{
Returns the original data frame less ongoing spells, with several duration specific variables:
  \item{duration}{Counter of time to failure, in units determined by \code{tID}.}
  \item{failure}{Indicator of failure event.}
  \item{censor}{Indicates right-censoring of a spell.}
  \item{end.spell}{Indicator that a spell has ended due to failure \emph{or} right-censoring.}
  \item{end}{Date at which a given spell ends.}
  \item{atrisk}{Binary indicator of whether a spell is at risk of failure. Equals 1 - \code{cure}.}
  \item{cure}{Binary indicator of an immune spell not at risk of failure. Equals 1 - \code{atrisk}.}
}

\author{
Andreas Beger
}

\seealso{
\code{\link{panelLag}} for lagging variables in a panel data frame before building duration data.
}

\examples{
# Monthly data
data(insurgency)
duration.ins <- buildDuration(insurgency, 'insurgency', unitID='ccode', 
                              tID='date', freq="monthly")

# Yearly example
yearly <- data.frame(event=c(0,0,0,1, 0,0,1,0), ccode=c(rep(1,4), rep(2,4)), 
  year=as.Date(rep(c("2001-01-01","2002-01-01","2003-01-01","2004-01-01"),2)),       
  stringsAsFactors=F)
buildDuration(yearly, "event", "ccode", "year", "yearly")
}

\keyword{spdur}
\keyword{spduration}
