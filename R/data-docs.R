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

#' @docType data
#' 
#' @name bscoup
#' 
#' @title B&S 2003 coup data
#' 
#' @description Replication data from Belkin and Schofer's 2003 paper on coups.
#' 
#' @usage bscoups
#' 
#' @format A data frame with 5828 observations of 9 variables:
#' \describe{
#'  \item{\code{countryid}}{Gleditsch and Ward country codes.}
#'  \item{\code{year}}{Year}
#'  \item{\code{couprisk}}{Structural coup risk index, see paper for details.}
#'  \item{\code{recentcoups}}{Alternative coup risk measure, running count of coups in past 10 years.}
#'  \item{\code{rwar}}{Country participated in war in past 10 years.}
#'  \item{\code{milreg}}{1=Military regime, 0=other}
#'  \item{\code{wealth}}{log of GDP per capita}
#'  \item{\code{instab}}{Domestic instability and violence.}
#'  \item{\code{coup}}{Indicator for successful coup.}
#'  \item{\code{africa}}{Indicator for countries in Africa.}
#'  \item{\code{eurnam}}{Indicator for countries in Europe and N. America.}
#'  \item{\code{samerica}}{Indicator for countries in South America.}
#'  \item{\code{camerica}}{Indicator for countries in Central America.}
#'  \item{\code{regconf}}{Regional conflict.}
#'}
#' 
#' @source 
#' Belkin, Aaron and Evan Schofer. 2003. ``Toward a structural understanding of 
#' coup risk.'' Journal of Conflict Resolution Vol. 47 No. 5.
#' 
#' @examples
#' data(bscoup)
#' table(bscoup$coup)
#' range(bscoup$year)
"bscoup"

#' @docType data
#' 
#' @name model.coups
#' 
#' @title Model of global coups from 1979 to 2010
#' 
#' @description This is a model object for a split-duration model of the 
#' Powell & Thyne coups. It is used in several example code sections to speed
#' up package testing by elminiating the need to re-estimate a model each time.
#' 
#' @usage model.coups
#' 
#' @format An object of class \code{spdur}.
#' 
#' @source 
#' For information on the data used in this model, see the data documentation,
#' \code{\link{coups}}.
#' 
#' @examples
#' data(model.coups)
#' str(model.coups)
NULL