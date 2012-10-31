


panelLag <- function(x, id, t, lag=1, data=NULL) {
  # Returns a version of x lagged within panels given by id.
  if (is.null(data)==T) data <- data.frame(x=x, id=id, t=t)
  k <- lag
  lags <- function(x, k) { 
  c(rep(NA, k), x[1:(length(x)-k)]) 
  }
  
  data <- data[order(data[, id], data[, t]), ]
  result <- unlist(by(data[, x], data[, id], lags, k=k))
  return(result)
}



#data <- data.frame(x=c(1,2,3,4,5,6,7,8,9,0), id=c(1,1,1,1,1,2,2,2,2,2), t=c(1,2,3,4,5,1,2,3,4,5))
#panelLag('x', 'id', 't',lag=2, data=data)

