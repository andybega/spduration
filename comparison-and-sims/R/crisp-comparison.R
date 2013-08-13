# Cure coding problem
# Andreas Beger
# August 2013

# Setup -------------------------------------------------------------------

# wd
setwd("~/Work/spdur_package/comparison-and-sims/")

# Libraries
library(spduration)
library(CRISP)
library(ggplot2)
library(pROC)
library(xtable)

# create connection to log file
log.name <- paste0("cure_", gsub(":", "-", sub(" ", "_", Sys.time())), ".txt")
sink(paste0("logs/", log.name))
cat(paste0("\n", " Split-duration cure coding problem\n", 
           paste(rep("=", 50), collapse=""), "\n"))
cat(paste0("\nTime: ", Sys.time(), "\nUser: ", Sys.info()[ "user"], "\n\n"))

# Functions

section <- function(title) {
  cat(paste0("\n ", title, "\n", paste(rep("=", 50), collapse=""), "\n\n"))
}
catl <- function(string) {
  cat(paste0("\n", string, "\n"))
}


# Get data ----------------------------------------------------------------

data(crisp.data)
data(cutoffs)

# Generate some variables
crisp.data$low_intensity <- with(crisp.data, ins.l.count.both.l1 + reb.l.count.both.l1 + eth.rel.l.count.l1)
crisp.data$high_intensity <- with(crisp.data, ins.h.count.both.l1 + reb.h.count.both.l1 + eth.rel.h.count.l1)
crisp.data$high_neighbors <- with(crisp.data, W.knn4.std.ins.h.count.both.l1 + W.knn4.std.reb.h.count.both.l1 + W.knn4.std.eth.rel.h.count.l1)
crisp.data$lgdpc.l1 <- with(crisp.data, log(NY.GDP.PCAP.KD.l1))
crisp.data$lpop.l1 <- with(crisp.data, log(SP.POP.TOTL.l1))
crisp.data$polity2.l1 <- with(crisp.data, DEMOC.l1 - AUTOC.l1)

# Subset to what we need
dur.eq <- c("high_neighbors", "high_intensity", "low_intensity")
cure.eq <- c("Amnesty.l1", "polity2.l1", "lgdpc.l1", "lpop.l1")
keep <- c("ccode", "date", "rebellion", dur.eq, cure.eq)
crisp.data <- subset(crisp.data, select=keep)

# Build duration data and check spdur and CRISP spdur() match -------------

section("Check that CRISP and spduration spdur() match.")

reb.data <- build.duration(data=crisp.data, y="rebellion", cutoffs$trainingend, 
                           cutoffs$teststart, cutoffs$dataend)

train <- reb.data$training
test <- reb.data$test
pred.data <- reb.data$pred.data

catl("Model using CRISP::spdur")
model <- CRISP::spdur(
  duration ~  high_neighbors + high_intensity + low_intensity, 
  c ~ Amnesty.l1 + polity2.l1 + lgdpc.l1 + lpop.l1, 
  last=train$end.spell, data=train, test=test, distr="weibull", iter=300
  )

catl("Model using spduration:::spdur_crisp")
model2 <- spduration:::spdur_crisp(
  duration ~  high_neighbors + high_intensity + low_intensity, 
  c ~ Amnesty.l1 + polity2.l1 + lgdpc.l1 + lpop.l1, 
  train=train, test=test, pred=pred.data, last="end.spell", distr="weibull", 
  iter=300
  )

# Match up coefficients
catl("Do spduration and CRISP spdur() results match?")
data.frame(spduration=coef(model2), crisp=model$model$coef)

# They match, so we can just move on with spdur_crisp()


# How often does cure coding change? --------------------------------------

section("How often does cure coding change?")

# Function that for a given date, sees how many failures there are later
# for spells that are coded as cured.
failLater <- function(trainend) {
  # Get cure coded spells
  data <- subset(crisp.data, date<=trainend)
  dur.data <- buildDuration(data=data, y="rebellion", "ccode", "date")
  spell.ends <- subset(dur.data, end.spell==1,
                         c("ccode", "date", "failure", "duration", "cured"))
  cured <- subset(spell.ends, cured==1)
  
  # Get later failures (after duration cutoff date)
  later.data <- subset(crisp.data, ccode %in% cured$ccode & date>trainend)
  later.data <- ddply(later.data, .(ccode), summarise, later.fail=max(rebellion))
  
  # Join results
  cured <- join(cured, later.data, by="ccode", type="left")
  res <- list(trainend=trainend, spell.ends=spell.ends, cured=cured)
  return(res)
}

delivery <- failLater(cutoffs$trainingend)
cured.tc <- data.frame(table(delivery$cured$duration))
names(cured.tc) <- c("t.censor", "Count")
xtable(cured.tc)

# Function to summarize the key results
sum.failLater <- function(...) {
  data <- failLater(...)
  res <- data.frame(trainend=data$trainend, 
                    spells=dim(data$spell.ends)[1],
                    failed=as.integer(sum(data$spell.ends$failure, na.rm=T)),
                    cured=dim(data$cured)[1],
                    late.fail=sum(data$cured$later.fail)
                    )
  res
}

# Roll through several train end dates to see how much misclassification
# changes:
sum.failLater(cutoffs$trainingend)
dates <- seq.Date(as.Date("2001-11-01"), as.Date("2012-11-01"), by="year")
cure.codings <- do.call(rbind, lapply(rev(dates), sum.failLater))
cure.codings$trainend <- as.character(cure.codings$trainend)

# get percent misclassified as cured: 
cure.codings$f.neg <- with(cure.codings, late.fail/(failed + late.fail))
cure.codings$f.neg <- round(cure.codings$f.neg, digits=2)

catl("CRISP cure miscoding by train end cutoff")
xtable(cure.codings)


# Durations and alternatives for cure coding ------------------------------

hist(delivery$cured$duration)

# Ideas:
# select a cutoff duration for cure coding
# select a cutoff duration, and set risk only for failed spells. 
# estimate the rest with a model (risk eq.?) and code thusly.

# to do:
# compare models with miscoded and correct cure codings

# Done, close log connection ----------------------------------------------
sink()