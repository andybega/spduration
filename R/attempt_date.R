#' Attempt to convert to R date format
#' 
#' For internal use only
#'
#' @param date character or numeric
#' @param by character, "year", "month", or "day"
#' 
#' @keywords internal

attempt_date <- function(date, by) {
  if (!class(date)=="Date") {
  	date <- as.character(date)
  	try(date <- as.Date(date), silent=TRUE)
  	if (class(date)=="Date") {
  	  warning("Converting to 'Date' class")
    } else if (by=="year") {
  	  try(date <- as.Date(paste0(date, "-06-30")), silent=TRUE)
  	  if (class(date)=="Date") {
  	  	warning("Converting to 'Date' class with yyyy-06-30")
  	  }
  	} else if (by=="month") {
  	  try(date <- as.Date(paste0(date, "-15")), silent=TRUE)
  	  if (class(date)=="Date") {
  	  	warning("Converting to 'Date' class with yyyy-mm-15")
  	  }
  	}
  }
  if (!class(date)=="Date") {
  	stop(paste("Could not convert to class 'Date'"))
  }
  return(date)
}