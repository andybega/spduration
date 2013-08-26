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
log.name <- paste0("compare_", gsub(":", "-", sub(" ", "_", Sys.time())), ".txt")
sink(paste0("logs/", log.name))
cat(paste0("\n", " Split-duration vs. probit comparison\n", 
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



# Build duration data -----------------------------------------------------

reb.data <- build.duration(data=crisp.data, y="rebellion", cutoffs$trainingend, 
                           cutoffs$teststart, cutoffs$dataend)

train <- reb.data$train
test <- reb.data$test
pred.data <- reb.data$pred.data

spdur1 <- spdurCrisp(duration ~ lgdpc.l1, c ~ lpop.l1, train=train, test=test, 
                     pred=pred.data, last="end.spell", distr="weibull")

probit1 <- glm(rebellion ~ lgdpc.l1 + lpop.l1, family=binomial(link="probit"), 
               data=train, na.action=na.exclude)
probit1$train.p <- predict(probit1, type="response")
probit1$test.p <- predict(probit1, type="response", newdata=test)

auc(train$failure, spdur1$train.p)
auc(test$failure, spdur1$test.p)
auc(train$failure, probit1$train.p)
auc(test$failure, probit1$test.p)

# Done, close log connection ----------------------------------------------
sink()