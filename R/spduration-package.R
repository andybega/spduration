#' Split-Population Duration (Cure) Regression Models
#' 
#' @description The spduration package provides functions to estimate 
#'     split-population duration regression models in which only a subset of the 
#'     population is at risk for failure, while the remainder is immune, or 
#'     cured, from the possibility of experiencing a failure event. In practice, 
#'     this class of models also may produce better performance in sparse data 
#'     with few actual failure events.
#'     
#' @details The main function \code{\link{spdur}} is used to estimate the model 
#'     objects with class \code{spdur}.
#'     
#'     Postestimation tools include \code{\link{predict.spdur}}, for calculating 
#'     fitted values with arbitrary data and for several probabilities that 
#'     might be of interest, as well as \code{\link{plot.spdur}} for visual 
#'     display of model fit. 
#' 
#' @references
#'     Leisch, Friedrich. 2009. ``Creating R Packages: A Tutorial.''
#'     
#'     Svolik, Milan. 2008. ``Authoritarian Reversals and Democratic 
#'         Consolidation.'' American Political Science Review.
#' 
#' @name spduration
#' @docType package
#' @useDynLib spduration
#' @importFrom Rcpp sourceCpp
NULL
globalVariables("temp.t")

#' @docType data
#' 
#' @name coups
#' 
#' @title Global coups, 1979 to 2010
#' 
#' @description Data on global coups from 1979 to 2010 from Powell & Thyne
#' 
#' @usage coups
#' 
#' @format A data frame with 5828 observations of 9 variables:
#' \describe{
#'  \item{\code{gwcode}}{Gleditsch and Ward country codes.}
#'  \item{\code{year}}{Year, in date format.}
#'  \item{\code{coup1}}{}
#'  \item{\code{succ.coup}}{Successful coup, 0/1.}
#'  \item{\code{democ}}{Polity democracy score (0-10).}
#'  \item{\code{autoc}}{Polity autocracy score (0-10).}
#'  \item{\code{polity}}{Polity score (democ-autoc).}
#'  \item{\code{polity2}}{Polity score with correction for regime transitions.}
#'  \item{\code{regtrans}}{Regime transitions.}
#'}
#' 
#' @source 
#' Powell, Jonathan M. and Clayton L. Thyne. ``Global instances of coups from 
#' 1950 to 2010: A new dataset.'' Journal of Peace Research Vol. 48 No. 2.
#' 
#' Gleditsch, Kristian S. and Michael D. Ward. 1999. ``Interstate System 
#' Membership: A Revised List of the Independent States since 1816." 
#' International Interactions 25.
#' 
#' @examples
#' data(coups)
#' table(coups$succ.coup)
"coups"