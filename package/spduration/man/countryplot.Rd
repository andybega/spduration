\name{countryplot}
\alias{countryplot}
\title{Plot predicted values for a country}
\usage{
countryplot(eoi = "insurgency", model, country, lastMonth, fcastMonths = 24,
  cumul = T, yext = T)
}
\arguments{
  \item{eoi}{Name of variable of interest as quoted
  string.}

  \item{model}{An object of class \code{spdur}.}

  \item{country}{Name of the country to plot.}

  \item{lastMonth}{Month from which to forecast from, as a
  date object e.g. \code{as.Date("2001-03-01")}}

  \item{fcastMonths}{Number of months to forecast ahead.}

  \item{cumul}{Show cumulative or absolute probabilities of
  an event? (T/F)}

  \item{yext}{Extend y-axis to theoretical maximum of 1?
  (T/F)}
}
\description{
For use with CRISP/ICEWS.
}
\author{
Shahryar Minhas, Andreas Beger
}

