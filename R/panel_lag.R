#' Lag panel data
#' 
#' A function that correctly lags panel data where units are identified by 
#' \code{id} and time periods are identified with \code{t}. Results are in same 
#' order as \code{data} and are padded with \code{NA} as needed.
#' 
#' @param x String identifying the vectors to be lagged in \code{data}.
#' @param id String identifying the unit (e.g. country) identifier in 
#' \code{data}.
#' @param t String identifying the time identifier in \code{data}.
#' @param lag Lag order, i.e. by how many time periods should \code{x} be 
#' lagged? Unlike the default \code{\link{lag}}, positive values indicate
#' that past data is used for the current time period.
#' @param data A data frame. If not provided, a new one will be constructed
#' with the vectors supplied for the other parameters.
#' 
#' @return A vector of same length as \code{x} representing lagged values with 
#' leading \code{NA}'s.
#' 
#' @examples
#' data(coups)
#' # No need to order before using panelLag, just do it here so we can compare results below.
#' coups <- coups[order(coups$gwcode, coups$year), ]
#' test <- panel_lag("polity2", "gwcode", "year", data=coups)
#' 
#' # Compare output
#' head(coups$polity2)
#' head(test)
#' 
#' @export
panel_lag <- function(x, id, t, lag=1, data=NULL) {
  ## Returns a version of x lagged within panels given by id, in original order
  # Input validation
  if (lag < 0) { 
    lag <- abs(lag)
    warning(paste('lag is < 0, using', lag, 'instead')) 
  }
  # Construct data if not given
  if (is.null(data)) data <- data.frame(x=x, id=id, t=t)
  # Subset to relevant parts
  data <- data[, c(x, id, t)]
  
  k <- lag
  lag.length.flag <- FALSE
  lagger <- function(x, k) { 
    if (k >= length(x)) {
      res <- rep(NA, length(x))
      lag.length.flag <<- TRUE
    }  
    if (k < length(x)) res <- c(rep(NA, k), x[1:(length(x)-k)]) 
    return(res)
  }
  
  data$orig.order <- 1:nrow(data) # to reorder results later
  data <- data[order(data[, id], data[, t]), ]
  res <- unlist(by(data[, x], data[, id], lagger, k=k))
  res <- res[order(data$orig.order)] # reorder results to original order
  
  if (lag.length.flag) warning('Lag order was larger than some or all unit series.')
  
  names(res) <- NULL
  return(res)
}