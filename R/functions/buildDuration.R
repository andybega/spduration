buildDuration <- function(data, y, unitID, tID, slice.last=FALSE) {
  require(plyr)
  
  res <- data[order(data[, unitID], data[, tID]), ]
  
  # Mark failure (0, 1)
  failure <- function(x) return(c(0, pmax(0, diff(x))))
  res$failure <- unlist(by(res[, y], res[, unitID], failure))
  
  # Drop ongoing (1->1) spells, keep only 0 and onset (0->1)
  res <- subset(res, !(get(y)==1 & failure==0))
  
  # Mark end of a spell and create unique ID
  # A spell can end 3 ways: failure, right-censor because past observation
  # period, or right censor because state ceased to exist.
  res$temp.t <- res[, tID]
  res <- ddply(res, .variables=unitID, transform, end=max(temp.t)) 
  res <- res[, !(colnames(res) %in% 'temp.t')]
  res$end.spell <- ifelse(format(res[, tID], '%Y-%m')==format(as.Date(res$end), '%Y-%m'), 1, 0)
  res$end.spell <- ifelse(res$failure==1, 1, res$end.spell)
  res$spellID <- rev(cumsum(rev(res$end.spell)))
  
  # Was there a failure at the end of a spell?
  failedspells <- res$spellID[res$failure==1]
  helper <- cbind(failedspells, 0)
  colnames(helper) <- c("spellID", "cured")
  
  # Create cure (c) variable
  res <- merge(res, helper, by=("spellID"), all.x=TRUE)
  res$cured[is.na(res$cured)] <- 1
  res$atrisk <- 1 - res$cured
  
  # Create censor variable
  res$censor <- ifelse(with(res, end.spell==1 & failure==0), 1, 0)
  
  # Create duration variable, need to order by spell ID and date!!!!
  helper <- rep(1, dim(res)[1])
  res <- res[order(res$spellID, res[, tID]), ]
  res$duration <- unlist(by(helper, res$spellID, cumsum))
  
  # Slice option for forecast data; for CRISP/ICEWS
  if (slice.last==TRUE) {
    dataend = max(data[, tID])
    pred.data <- subset(res, date==dataend)
    missing <- setdiff(unique(data[, unitID]), unique(pred.data[, unitID]))
    missing <- data[data[, tID]==dataend & data[, unitID] %in% missing, ]
    missing <- cbind(spellID=rep(NA, dim(missing)[1]), missing)
    missing$failure <- 0
    missing$end <- NA
    missing$end.spell <- NA
    missing$cured <- 0
    missing$atrisk <- 1
    missing$censor <- 1
    missing$duration <- 1
    res <- rbind(pred.data, missing)
  }
  
  # Reorder res by ccode/date
  res <- res[order(res[, unitID], res[, tID]), ]
  return(res)
}