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