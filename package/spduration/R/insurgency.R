#' @docType data
#' 
#' @name insurgency
#' 
#' @title Global insurgencies, 2001 to 2011
#' 
#' @description Data on global insurgencies between 2001 and 2011 from the 
#' CRISP/ICEWS project, in country-month format.
#' 
#' @usage insurgency
#' 
#' @format A data frame with 23213 observations on the following 11 variables.
#' \describe{
#'  \item{\code{ccode}}{Correlates of War country codes.}
#'  \item{\code{country}}{Country names.}
#'  \item{\code{date}}{Date identifier, in "YYYY-MM-DD" format.}
#'  \item{\code{insurgency}}{Binary indicator of insurgency incidence.}
#'  \item{\code{low_intensity}}{a numeric vector}
#'  \item{\code{high_intensity}}{a numeric vector}
#'  \item{\code{high_neighborhood}}{a numeric vector}
#'  \item{\code{high_neighbors}}{a numeric vector}
#'  \item{\code{lgdppc.l1}}{Lagged log of GDP per capita.}
#'  \item{\code{exclpop.l1}}{Proportion of excluded minorities.}
#'  \item{\code{excl_groups_count.l1}}{Lagged number of excluded minority groups.}
#'}
#' 
#' @source Duke University CRISP/ICEWS projects.
#' 
#' @examples
#' data(insurgency)
#' table(insurgency$insurgency)
NULL