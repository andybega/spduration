buildDuration <- function(data, y) {
  require(plyr)
  
  data <- data[order(data$ccode,data$date), ]
  
  # Mark failure (0, 1)
  failure <- function(x) return(c(0, pmax(0, diff(x))))
  data$failure <- unlist(by(data[, y], data$ccode, failure))
  
  # Drop ongoing (1->1) spells, keep only 0 and onset (0->1)
  data <- subset(data, !(get(y)==1 & failure==0))
  
  # Mark end of a spell and create unique ID
  # A spell can end 3 ways: failure, right-censor because past observation
  # period, or right censor because state ceased to exist.
  data <- ddply(data, .(ccode), transform, end=max(date)) 
  data$end.spell <- ifelse(format(data$date, '%Y-%m')==format(as.Date(data$end), '%Y-%m'), 1, 0)
  data$end.spell <- ifelse(data$failure==1, 1, data$end.spell)
  data$spellID <- rev(cumsum(rev(data$end.spell)))
  
  # Was there a failure at the end of a spell?
  failedspells <- data$spellID[data$failure==1]
  helper <- cbind(failedspells, 0)
  colnames(helper) <- c("spellID", "cured")
  
  # Create cure (c) variable
  data <- merge(data, helper, by=("spellID"), all.x=TRUE)
  data$cured[is.na(data$cured)] <- 1
  data$atrisk <- 1 - data$cured
  
  # Create censor variable
  data$censor <- ifelse(with(data, end.spell==1 & failure==0), 1, 0)
  
  # Create duration variable, need to order by spell ID and date!!!!
  helper <- rep(1, dim(data)[1])
  data <- data[order(data$spellID, data$date), ]
  data$duration <- unlist(by(helper, data$spellID, cumsum))
  
  # Reorder data by ccode/date
  data <- data[order(data$ccode, data$date), ]
  
  # Done
  return(data)
}