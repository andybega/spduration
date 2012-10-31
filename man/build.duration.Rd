\name{build.duration}
\Rdversion{1.0}
\alias{build.duration}
\title{
Build duration version of country-month data
}
\description{
This function allows the user to build a duration version of the CRISP data for a given event of interest, and will automatically create training, test, and prediction datasets.
}
\usage{
build.duration(data, y, trainingend, teststart, dataend)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{data}{
	The untransformed CRISP data in country-month format.
	}
	\item{y}{
	The name of the event of interest for which to build duration data, given as a character vector (e.g., \dQuote{insurgency}). Must be one of insurgency, rebellion, dpc, erv, or ic.
	}
	\item{trainingend}{
	The end of the training period, given as character vector with format \dQuote{yyyy-mm-dd} (e.g. \dQuote{2011-06-01}).
	}
	\item{teststart}{
	Beginning of the test period, i.e. one month after training end, given as a character vector in with format \dQuote{yyyy-mm-dd} (e.g. \dQuote{2011-07-01}).
	}
	\item{dataend}{
	Last month of available data, i.e. end of the test period, given as a character vector in with format \dQuote{yyyy-mm-dd} (e.g. \dQuote{2011-07-01}). This is used to create the prediction data, which include ongoing spells of an event in order to be able to create forecasts for all countries (including those currently experiencing an event).
	}
}

\details{

This function builds duration data for an event of interest from the CRISP data.

}


\author{
Michael D. Ward (PI) and Andreas Beger}

\examples{\dontrun{
data(crisp.data)

# build duration data for insurgency
duration.ins <- build.duration(data=crisp.data, y="insurgency", 
  trainingend="2011-11-01", teststart="2011-12-01", 
  dataend="2012-09-01")

# extract training data, test data, prediction data
train <- duration.ins$training
test <- duration.ins$test
pred.data <- duration.ins$pred.data
}}
\keyword{build.duration}
