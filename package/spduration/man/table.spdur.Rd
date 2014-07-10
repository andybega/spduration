\name{table.spdur}
\alias{table.spdur}
\title{Create a simple table of split-population duration results}
\usage{
table.spdur(object, row.names = TRUE)
}
\arguments{
  \item{object}{An object with class \code{spdur}.}

  \item{row.names}{Indicates whether parameter names should
  be added as row names to the data frame returned, or as a
  separate column with blank row row names.}
}
\value{
An data frame with model coefficients and p-values.
}
\description{
\code{table}-like function for class ``\code{spdur}''.
}
\details{
This will create a data frame containing the estimated
coefficients and standard errors for the risk and duration
equations of a split-population duration model. It's
intented purpose is to help create larger tables combining
several model results.
}
\examples{
data(model.coups)
tbl <- table.spdur(model.coups)
tbl
}
\seealso{
\code{\link{xtable.spdur}} for formatting a single model to
Latex output.
}

