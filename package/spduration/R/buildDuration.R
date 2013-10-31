#' Build duration version of panel data
#' 
#' Builds a duration version of a data frame representing panel data.
#' 
#' @param data Data frame representing panel data.
#' @param y A binary indicator of the incidence of some event, e.g. a coup.
#' @param unitID Name of the variable in the data frame identifying the 
#' cross-sectional units, e.g. \code{"country"}.
#' @param tID Name of the variable in the data frame identifying the time unit, 
#' preferably as class \code{\link{Date}}. E.g. \code{"year"}.
#' @param freq Frequency at which units are measured in \code{tID}. Currently 
#' yearly, monthly, and daily data are supported, i.e. \code{"year"}, 
#' \code{"month"}, or \code{"day"}.
#' @param sort Sort data by unit and time? Default is \code{FALSE}, i.e. return
#' data in original order.
#' @param slice.last Set to \code{TRUE} to create a slice of the last time 
#' period; used with \code{\link{forecast.spdur}}. For compatibility with CRISP 
#' and ICEWS projects.
#' 
#' @details 
#' This function processes a panel data frame by creating a failure 
#' variable from \code{y} and corresponding duration counter, as well as 
#' risk/immunity indicators. Supported time resolutions are year, month, and
#' day, and input data should be (dis-)aggregated to one of these levels.
#' 
#' The returned data frame should have the same number of rows at the original. 
#' If \code{y} is an indicator of the incidence of some event, rather than an 
#' onset indicator, then ongoing spells of failure beyond the initial event are 
#' coded as NA (e.g. 000111 becomes a spell of 0001 NA NA). This is to preserve
#' compatability with the base dataset. Note that the order of rows may be 
#' different though.
#' 
#' There cannot be missing values ("\code{NA}") in any of the key variables 
#' \code{y}, \code{unitID}, or \code{tID}; they will stop the function.
#' 
#' Furthermore, series that start with an event, e.g. (100), are treated as 
#' experiencing failure in the first time period. If those events are in fact 
#' ongoing, e.g. the last year of a war that started before the start time of 
#' the dataset, they should be dropped manually before using 
#' \code{buildDuration()}.
#' 
#' \code{t.0} is the starting time of the period of observation at \code{tID}. 
#' It is by default set as \code{duration - 1} and currenlty only serves as a 
#' placeholder to allow future expansion for varying observation times.
#' 
#' @return
#' Returns the original data frame with 9 duration-specific additional 
#' variables:
#' \item{spellID}{Unique key for spells.}
#' \item{failure}{Binary indicator of an event.}
#' \item{ongoing}{Binary indicator for ongoing events, not counting the inital 
#' failure time.}
#' \item{end.spell}{Binary indicator for the last observation in a spell, either
#' due to censoring or failure.}
#' \item{cured}{Binary indicator for spells that are coded as cured, or immune
#' from failure. Equal to 1 - \code{atrisk}.}
#' \item{atrisk}{Binary indicator for spells that are coded as at risk for 
#' failure. Equal to 1 - \code{cured}.}
#' \item{censor}{Binary indicator for right-censored spells.}
#' \item{duration}{\code{t}, counter for how long a spell has survived without
#' failure.}
#' \item{t.0}{Starting time for period observed during \code{t}, by default
#' equals \code{duration} - 1.}
#' 
#' @author Andreas Beger
#' 
#' @seealso \code{\link{panelLag}} for lagging variables in a panel data frame 
#' before building duration data.
#' 
#' @examples
#' # Yearly data
#' data <- data.frame(y=c(0,0,0,1,0), 
#'                    unitID=c(1,1,1,1,1), 
#'                    tID=c(2000, 2001, 2002, 2003, 2004))
#' dur.data <- buildDuration(data, "y", "unitID", "tID", freq="year")
#' dur.data
#'
#' @export
#' @importFrom plyr ddply
buildDuration <- function(data, y, unitID, tID, freq="month", sort=FALSE,
                          slice.last=FALSE) {
  
  ##    Check input
  
  # valid frequency
  supported.freq <- c("day", "month", "year")
  if (!freq %in% supported.freq) {
    stop("frequency must be 'day', 'month' or 'year'")
  }
  
  # convert to date if possible
  if (class(data[, tID])!="Date") {  
    data[, tID] <- as.character(data[, tID])
    if (freq=="year") {
      tryCatch(data[, tID] <- as.Date(data[, tID], format="%Y"))
    } else if (freq=="month") {
      tryCatch(data[, tID] <- as.Date(data[, tID], format="%Y-%m"))
      tryCatch(data[, tID] <- as.Date(data[, tID], format="%Y/%m"))
    } else if (freq=="day") {
      ## need to fill in
    }
    # Throw error if unsuccessfull
    if (class(data[, tID])=="Date") {
      warning(paste("Temporarily converted", tID, "to 'Date' type."))
    } else {
      stop(paste(tID, "is not class 'Date' (?as.Date())"))  
    }
  }
  
  # check for missing keys
  if (any(is.na(data[, c(y, unitID, tID)]))) {
    stop("There are NA's in y, unitID, or tID")
  }
  
  # check that y is binary and has failures
  if (!all(unique(data[, y]) %in% c(0, 1))) {
    stop(paste(y, "must be binary (0, 1)"))
  } else if (all(data[, y]==0)) {
    stop(paste0("No failures occur in data (", substitute(data), "[, \"", y, 
                "\"])"))
  }
  
  ##    Begin duration build code
  
  # Keep track of original order
  data$orig_order_track <- 1:nrow(data)
  
  # Need to order by date to id and drop ongoing spells
  res <- data[order(data[, unitID], data[, tID]), ]
  
  # Mark failure (0, 1, NA for ongoing)
  failure <- function(x) return(c(x[1], pmax(0, diff(x))))
  res$failure <- unlist(by(res[, y], res[, unitID], failure))
  res$ongoing <- ifelse(res[, y]==1 & res$failure==0, 1, 0)
  res$failure <- ifelse(res$ongoing==1, NA, res$failure)
  
  # Mark end of a spell and create unique ID
  # A spell can end 3 ways: failure, right-censor because last observation
  # period, or right censor because state ceased to exist.
  res$temp.t <- res[, tID]
  res <- ddply(res, .variables=unitID, transform, end=max(temp.t)) 
  res <- res[, !(colnames(res) %in% 'temp.t')]
  if (freq=="year") {
    cond <- (format(res[, tID], '%Y')==format(as.Date(res$end), '%Y'))
  } else if (freq=="month") {
    cond <- (format(res[, tID], '%Y-%m')==format(as.Date(res$end), '%Y-%m'))
  } else if (freq=="day") {
    cond <- (format(res[, tID], '%Y-%m-%d')==
               format(as.Date(res$end), '%Y-%m-%d'))
  }
  res$end.spell <- ifelse(cond, 1, 0)
  res$end.spell <- ifelse(res$failure==1, 1, res$end.spell)
  
  # Hack to correct ongoing spells, then create spellID
  spellIDhelper <- ifelse(is.na(res$end.spell), 0, res$end.spell)
  res$spellID <- rev(cumsum(rev(spellIDhelper)))
  res$spellID <- ifelse(res$ongoing==1, NA, res$spellID)
  
  # Was there a failure at the end of a spell?
  failedspells <- res$spellID[res$failure==1 & res$ongoing==0]
  helper <- cbind(failedspells, 0)
  colnames(helper) <- c("spellID", "cured")
  
  # Create cure (c) variable
  res <- merge(res, helper, by=("spellID"), all.x=TRUE)
  res$cured[is.na(res$cured) & res$ongoing==0] <- 1
  res$atrisk <- 1 - res$cured
  
  # Create censor variable
  res$censor <- ifelse(with(res, end.spell==1 & failure==0), 1, 0)
  
  # Create duration variable, need to order by spell ID and date!!!!
  helper <- 1 - res$ongoing
  res <- res[order(res$spellID, res[, tID]), ]
  res$duration[!is.na(res$spellID)] <- unlist(
    by(helper[!is.na(res$spellID)], res$spellID[!is.na(res$spellID)], cumsum)
    )
  res$t.0 <- res$duration - 1 ## previous duration
  
  ##    Slice option for forecast data; for CRISP/ICEWS
  
  if (slice.last==TRUE) {
    dataend = max(data[, tID])
    pred.data <- res[res[, tID]==dataend, ]
    missing <- setdiff(unique(data[, unitID]), unique(pred.data[, unitID]))
    if (length(missing)==0) {
      missing <- data[data[, tID]==dataend & data[, unitID] %in% missing, ]
      missing <- cbind(spellID=rep(NA, dim(missing)[1]), missing)
      missing$failure <- 0
      missing$ongoing <- 1
      missing$end <- NA
      missing$end.spell <- NA
      missing$cured <- 0
      missing$atrisk <- 1
      missing$censor <- 1
      missing$duration <- 1
      res <- rbind(pred.data, missing)
    }
  }
  
  ##    Clean up result
  
  # Reformat dates
  if (freq=="year")  res[, tID] <- format(res[, tID], format="%Y")
  if (freq=="month") res[, tID] <- format(res[, tID], format="%Y-%m")
  if (freq=="day")   res[, tID] <- format(res[, tID], format="%Y-%m-%d")
  
  # Sort results
  if (sort==F) {
    res <- res[order(res$orig_order_track), ]       # original order
  } else {
    res <- res[order(res[, unitID], res[, tID]), ]  # Reorder res by ccode/date
    warning(paste0("Data is sorted by ", unitID, ", ", tID))
  }
  
  # Take out utility columns
  res <- res[, !colnames(res) %in% c("end", "orig_order_track")]
  return(res)
}

# Test for yearly data
# data <- data.frame(y=c(0,0,0,1,0), 
#                     unitID=c(1,1,1,1,1), 
#                     tID=c(2000, 2001, 2002, 2003, 2004))
# test <- buildDuration(data, "y", "unitID", "tID", freq="year")
# goal <- structure(
#   list(spellID = c(2, 2, 2, 2, 1), 
#       y = c(0, 0, 0, 1, 0), 
#         unitID = c(1, 1, 1, 1, 1), 
#         tID = c("2000", "2001", "2002", "2003", "2004"), 
#         failure = c(0, 0, 0, 1, 0), 
#         ongoing = c(0, 0, 0, 0, 0), 
#         end.spell = c(0, 0, 0, 1, 1), 
#         cured = c(0, 0, 0, 0, 1), 
#         atrisk = c(1, 1, 1, 1, 0), 
#         censor = c(0, 0, 0, 0, 1), 
#         duration = c(1, 2, 3, 4, 1), 
#         t.0 = c(0, 1, 2, 3, 0)), 
#    .Names = c("spellID", "y", "unitID", "tID", "failure", "ongoing", 
#               "end.spell", "cured", "atrisk", "censor", "duration", "t.0"), 
#    class = "data.frame", row.names = c(2L, 3L, 4L, 5L, 1L))
# all(test==goal)
# 
# # Test for daily data
# data <- data.frame(
#   tID=c(rep(seq.Date(as.Date("2000-01-01"), by="day", length.out=10), 2)),
#   unitID=c(rep(1, 10), rep(2, 10)),
#   y=c(rep(0, 8),1,0, 0,1,1,rep(0, 6),1)
#   )
# test <- buildDuration(data, "y", "unitID", "tID", freq="day")
# goal <- structure(list(
#   spellID = c(4, 4, 4, 4, 4, 4, 4, 4, 4, 3, 2, 2, NA, 1, 1, 1, 1, 1, 1, 1), 
#   tID = c("2000-01-01", "2000-01-02", "2000-01-03", "2000-01-04", "2000-01-05", 
#           "2000-01-06", "2000-01-07", "2000-01-08", "2000-01-09", "2000-01-10", 
#           "2000-01-01", "2000-01-02", "2000-01-03", "2000-01-04", "2000-01-05", 
#           "2000-01-06", "2000-01-07", "2000-01-08", "2000-01-09", "2000-01-10"), 
#   unitID = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2), 
#   y = c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1), 
#   failure = c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, NA, 0, 0, 0, 0, 0, 0, 1), 
#   ongoing = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0), 
#   end.spell = c(0,  0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, NA, 0, 0, 0, 0, 0, 0, 1), 
#   cured = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, NA, 0, 0, 0, 0, 0, 0, 0), 
#   atrisk = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, NA, 1, 1, 1, 1, 1, 1, 1), 
#   censor = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, NA, 0, 0, 0, 0, 0, 0, 0), 
#   duration = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 1, 2, NA, 1, 2, 3, 4, 5, 6, 7), 
#   t.0 = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 0, 0, 1, NA, 0, 1, 2, 3, 4, 5, 6)), 
#   .Names = c("spellID", "tID", "unitID", "y", "failure", "ongoing", "end.spell", 
#              "cured", "atrisk", "censor", "duration", "t.0"), 
#   class = "data.frame", 
#   row.names = c(13L, 14L, 11L, 12L, 17L, 18L, 15L, 16L, 19L, 10L, 8L, 9L, 20L, 
#                 1L, 2L, 3L, 4L, 5L, 6L, 7L))
# all(goal==test, na.rm=T)

# Test sorting
# data <- data.frame(
#   y=c(0,0,0,1,0), 
#   unitID=c(1,1,1,1,1), 
#   tID=c(2000, 2001, 2002, 2003, 2004))
# data <- data[sample(1:5), ]
# test <- buildDuration(data, "y", "unitID", "tID", freq="year")
# goal <- data[, c("unitID", "tID")]
# all(goal==test[, c("unitID", "tID")])