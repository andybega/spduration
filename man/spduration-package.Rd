\name{spduration-package}
\alias{spduration-package}
\alias{spduration}
\docType{package}
\title{
Split-Population Duration (Cure) Regression Models
}
\description{
The spduration package provides functions to estimate split-population duration regression 
models in which only a subset of the population is at risk for failure, while the remainder
is immune, or cured, from the possibility of experiencing a failure event. In practice, 
this class of models also may produce better performance in sparse data with few actual
failure events.
}
\details{
\tabular{ll}{
Package: \tab spdur\cr
Type: \tab Package\cr
Version: \tab 0.1 (pre-release)\cr
Date: \tab 2012-11-01\cr
License: \tab GPL 3.0\cr
}
The main function spdur is used to estimate the model objects with class ``spdur''. 

Postestimation tools include predict.spdur, for calculating fitted values with arbitrary
data and for several probabilities that might be of interest, as well as separationplot.spdur
for visual display of model fit. 
}
\author{
Andreas Beger, Daina Chiba, Daniel W. Hill, Nils W. Metternich, and Michael D. Ward

Maintainer: Andreas Beger <andreas.beger@duke.edu>
}
\references{
Leisch, Friedrich. 2009. ``Creating R Packages: A Tutorial.''
}
\examples{
#data(xx)

# Model
#model <- spdur(duration ~ ,
#               atrisk ~ ,
#               data = , last = , max.iter = 200)
}
