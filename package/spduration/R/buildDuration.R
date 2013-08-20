#' @export
#' @importFrom plyr ddply
buildDuration <- function(data, y, unitID, tID, freq="monthly", 
                          slice.last=FALSE) {
  
  # Check input
  if (class(data[, tID])!="Date") stop(paste(tID, "is not class 'Date' (?as.Date())"))
  supported.freq=c("monthly", "yearly")
  if (!freq %in% supported.freq) stop("frequency must be 'monthly' or 'yearly'")
  if (any(is.na(data[, c(y, unitID, tID)]))) stop("There are NA's in y, unitID, or tID")
  
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
  if (freq=="monthly") {
    res$end.spell <- ifelse(format(res[, tID], '%Y-%m')==format(as.Date(res$end), '%Y-%m'), 1, 0)
  }
  if (freq=="yearly") {
    res$end.spell <- ifelse(format(res[, tID], '%Y')==format(as.Date(res$end), '%Y'), 1, 0)
  }
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
  
  # Slice option for forecast data; for CRISP/ICEWS
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
  
  # Reorder res by ccode/date
  res <- res[order(res[, unitID], res[, tID]), ]
  res <- res[, !colnames(res) %in% "end"]
  return(res)
}