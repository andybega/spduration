# rm(list=ls())
# # setwd('~/Dropbox/Work/spdur_package/package/spduration_0.9/spdurationCNTRYplot')
# setwd('~/Dropbox/spdur_package/package/spduration_0.9/spdurationCNTRYplot')
# load('spdurModelResults.rda')

# eoi='insurgency'
# model=model
# # country='CHAD'
# country='LIBYA'
# lastMonth=as.Date("2001-03-01")
# fcastMonths=24
# cumul=T
# yext=F

countryplot <- function(eoi='insurgency', model, country, lastMonth, 
                        fcastMonths=24, cumul=T, yext=T) {
  # Input validation
  if (!'spdur' %in% class(model)) stop('"model" argument must have class "spdur"')
  ## check eoi is valid
  
  require(spduration)
  require(ggplot2)
  require(reshape2)
  require(Hmisc)
  
  # Get original model data and actual event values
  data <- get(paste(model$call$data))
  
  # Check month is in valid date range
  if (!lastMonth >= min(data$date) | !lastMonth <= max(data$date)) {
    stop(paste0("lastMonth is not in data date range.\n",
                "Check range(", paste(model$call$data), "$date)"))
  }
  
  # Slice country data and check for ongoing event
  if (!country %in% unique(data$country)) {
    stop(paste0(country, " is not a recognized country name.\n",
                "Check unique(", paste(model$call$data), "$country)"))
  }
  dataCountry <- data[data$country==country, ]
  dataSlice <- data[data$country==country & data$date==lastMonth, ]
  
  # Assumes that missing date has to be for ongoing event, since we just checked
  # if it is in the overall CRISP data range.
  if (dim(dataSlice)[1]==0) {
    stop(paste0(lastMonth, " missing for ", country, "\n",
                "Is it ongoing event?"))
  }
  
  # start and end dates
  fperiod <- c(lastMonth, 
               seq(lastMonth, by="month", length=fcastMonths)[fcastMonths]) 
  # empty data frame for results
  fcastData <- data.frame(matrix(NA, nrow=fcastMonths, ncol=4))
  colnames(fcastData) <- c("date", "prob", "cum_prob", "obs")
  # add dates
  fcastData$date <- seq(lastMonth, by="month", length=fcastMonths)
  # add probabilities
  fcastData[, "prob"] <- c(forecast(model, pred.data=dataSlice, 
                                  stat='conditional hazard', npred=fcastMonths))
  fcastData[, "cum_prob"] <- 1 - cumprod(1 - fcastData[, "prob"])
  # get observed
  pos_in_obs <- match(fcastData$date, dataCountry$date)
  fcastData$obs <- ifelse(is.na(pos_in_obs), NA, dataCountry[, eoi])
  
  # calculate MTTF
  prob <- c(forecast(model, pred.data=dataSlice, 
                     stat='conditional hazard', npred=120))
  cum_prob <- 1 - cumprod(1 - prob)
  if (cum_prob[120] < 0.5) {
    mttf <- "MTTF >10 years"
  } else {
    mttf <- paste("MTTF", round(which(cum_prob>=0.5)[1]/12,2), "years")
  }
  
  # this is what we will plot (cumul option)
  if (cumul) {
    fcastData$line <- fcastData$cum_prob
  } else {
    fcastData$line <- fcastData$prob
  }
  
  # set factor type for plotting (NA, 0, 1)
  # need to set levels so that countries missing one of the three plot with
  # correct color, otherwise color is assigned in ggplot when creating factor.
  col_pal <- c("#FF0000", "#FEE8D6", "gray80") # event, no event, no data
  names(col_pal) <- c("event", "no event", "no data")
  fcastData$type <- ifelse(is.na(fcastData$obs), "no data", "no event")
  fcastData$type <- ifelse((!is.na(fcastData$obs) & fcastData$obs==1), 
                           "event", fcastData$type)
  
  # we will need this for bar and scale height:
  max_p <- max(fcastData[, "line"])
  
  # upper value for y axis, default is 1
  if (yext) {
      y_upp <- 1
    } else {
      y_upp <- round(1.5*max_p, digits=2)
    }

  fcastData$bar_h <- y_upp
  fcastData$bar_l <- 0
  
  # Plotting
  temp <- ggplot(fcastData, aes(x=date))
  # use rect. instead of bar to allow for varying width corresponding to days
  # in a month
  temp <- temp + geom_rect(aes(xmin=date, xmax=(date + monthDays(date) - 2), 
                               ymin=bar_l, ymax=bar_h, fill=type), alpha=0.8) 
  # add stat value
  temp <- temp + geom_line(aes(x=date, y=line), expand=c(0,0))
  # adjust scales
  step <- round(fcastMonths/10) #number controls how many months will be shown
  temp <- temp + 
    # set y scale upper limit
    scale_y_continuous(expand=c(0,0), limits=c(0, y_upp)) +
    # colors for bars
    scale_fill_manual(values=col_pal) +
    # set labels for x scale
    scale_x_date(breaks=c(fcastData$date[seq(1, fcastMonths, by=step)]),
                 expand=c(0,0)) +
    # scale and plot titles
    labs(title=paste(country, ": ", fperiod[1], " to ", fperiod[2], sep=''),
         x=NULL, y=NULL)

  # Add MTTF
  mttfXpos <- round(length(fcastData$date)/5,0)
  temp <- temp + annotate("text", label=mttf, x=fcastData$date[mttfXpos], y=y_upp*0.9)
    
  # theme formatting
  temp <- temp + 
    # white background
    theme_bw() +
    # other
    theme(
      panel.grid.major=element_blank(), 
      panel.grid.minor=element_blank(),
      axis.ticks=element_blank(), 
      legend.position='none',  
      axis.text.x=element_text(angle=45, size=8, hjust=1,vjust=1),
      axis.text.y=element_text(size=8)
    )
  return(temp)
}

# good plot
# png("chad-cum.png", height=300, width=600)
# par(mfrow=c(2,1))
# countryplot(eoi='insurgency', model, "CHAD", lastMonth=as.Date("2001-03-01"), 
#             fcastMonths=36, cumul=T, yext=F)
# dev.off()
# png("chad-haz.png", height=300, width=600)
# countryplot(eoi='insurgency', model, "CHAD", lastMonth=as.Date("2001-03-01"), 
#             fcastMonths=36, cumul=F, yext=F)
# dev.off()