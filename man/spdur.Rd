\name{spdur}
\alias{spdur}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Split-population duration model
}
\description{
%%  This function estimates parametric split-population duration models, including Weibull and log-logistic models. 
The function automatically generates predicted (cumulative) hazards for each observation in the estimation sample, as well as predicted hazards for a 
user-specified test dataset not used for estimation.
}
\usage{
spdur(formula, cure = formula2, data = NULL, test = NULL, last = NULL, distr = "", iter = NULL, sims = NULL)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{A formula of the form Y~X1+X2..., where Y is the duration.}
  \item{cure}{Specification of the splitting equation, where formula2 is another formula of the form Y~X1+X2..., where Y is a binary variable equal to one
  for cases that eventually experience the event.}
  \item{data}{A data frame containing the variables used in formula and formula2.}
  \item{test}{A test dataset used for prediction but not for estimation. }
  \item{last}{A binary variable equal to one if the event occurs or the censoring time has been reached.}
  \item{distr}{The type of distribution to use for the hazard rate. Options are ``weibull'' or ``loglog''}
  \item{iter}{The maximum number of iterations to use for optimization.}
  \item{sims}{The number of simulated parameter values to use for calculating predicted hazards.}
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{model1 }{Coefficients, standard errors, p values and z statistics}
\item{model2 }{Variance-covariance matrix}
\item{model3 }{Predicted hazards and 95\% confidence intervals for estimation sample}
\item{model4 }{Predicted hazards and 95\% confidence intervals for test data}
\item{model4 }{Predicted probabilities from the splitting equation for the estimation sample (i.e. the probability that an observation 
is at-risk for experiencing the event)}
%% ...
}

\author{
Daniel W. Hill, Jr. and Nils Metternich
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(formula, cure=formula2, data=NULL, test=NULL, last=NULL, distr="", iter=NULL, sims=NULL) {

if (is.null(data)) stop("No data provided")
if (is.null(last)) stop("Must specify censoring variable")
if (distr=="") stop("Must specify distribution")

a<-as.character(formula)
b<-as.character(cure)

lhb<-a[2]
rhb<-strsplit(a[3], split=" + ", fixed=T)[[1]]

lhg<-b[2]
rhg<-strsplit(b[3], split=" + ", fixed=T)[[1]]

X<-data[rhb]
Z<-data[rhg]
Y<-cbind(data[lhg], data[lhb], last)

X.test<-test[rhb]
Z.test<-test[rhg]
Y.test<-cbind(test[lhg], test[lhb])

if (is.null(iter)) iter<-100
if (is.null(sims)) sims<-1000

    if (distr=="weibull") {
        model<-spweibull(Y, X, Z, Y.test, X.test, Z.test, iter, sims)
        }

    if (distr=="loglog") {
        model<-sploglog(Y, X, Z, Y.test, X.test, Z.test, iter, sims)
        }
    
invisible((model))

  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{spdur}

