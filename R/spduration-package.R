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
#'     Boag, J.W. 1949. ``Maximum Likelihood Estimates of the Proportion of 
#'       Patients Cured by Cancer Therapy.'' <https://www.jstor.org/stable/2983694>
#' 
#' 	   Berkson, J. and Gage, R.P. ``Survival Curve for Cancer Patients Following 
#' 	     Treatment.'' <https://www.jstor.org/stable/2281318>
#' 
#'     Leisch, Friedrich. 2009. ``Creating R Packages: A Tutorial.''
#' 
#'     Schmidt, Peter and Witte, Ann Dryden. 1989. ``Predicting Criminal 
#'       Recidivism Using "Split Population" Survival Time Models.'' 
#'       <doi:10.1016/0304-4076(89)90034-1>    
#' 
#'     Svolik, Milan. 2008. ``Authoritarian Reversals and Democratic 
#'         Consolidation.'' American Political Science Review.
#' 
#' @name spduration
#' @useDynLib spduration, .registration = TRUE
#' @importFrom Rcpp sourceCpp
"_PACKAGE"

globalVariables("temp.t")

