##
##    Demo/test code for "spduration" R package using Powell & Thyne coups
##    Andreas Beger
##    Cassy Dorff
##    October 2013
##


# 1. install the latest version of spduration on dropbox
# 2. update this demo file so it uses the coups stuff
# 3. try out methods for spdurCRISP (model2 at the bottom), and see what breaks


library(spduration)

## 
# Task: create coups model and try out some of the methods
data("coups")


# Demo model to save estimation for examples
duration.coup <- spduration::buildDuration(coups, "succ.coup", unitID="gwcode", tID='year',
                          freq="year")                    

# Split duration model of coups
model.coups <- spduration::spdur(duration ~ polity2, atrisk ~ polity2, last='end.spell', 
                     data=duration.coup, distr="weibull", max.iter=300)

# summary methods
# Error: 'summary' is not an exported object from 'namespace:spduration'
# Same error for nobs, AIC, BIC & Predict

spduration::summary(model.coups) #works!
spduration::nobs(model.coups) # works
spduration::AIC(model.coups) #works!
spduration::BIC(model.coups) #works!

# predict
pred <- duration.coup[-model.coups$na.action, ]
pred$yhat <- spduration::predict(model.coups)
top5 <- pred[pred$year==2010, c('gwcode', "year", 'yhat')]
top5 <- top5[order(top5$yhat, decreasing=TRUE), ]
head(top5)


### CRISP wrapper (spdurCRISP)
# training: buildDuration, subset the coups through 2000
coups.train <- coups[coups$year<="2000-06-30",]
duration.coups.train <- spduration::buildDuration(coups.train, "succ.coup", unitID="gwcode", 
                                      tID='year', freq="year")

# test: buildDuration with coups through 2009
coups.test <- coups[coups$year<="2009-06-30",]
duration.coups.test <-spduration::buildDuration(coups.test, "succ.coup", unitID="gwcode", 
                                     tID='year', freq="year")

# then subset it by dropping all before 2001
duration.coups.test <- duration.coups.test[duration.coups.test$year>=2001, ]

#pred: slice the last year of coups 2010
coups.prediction<-coups[coups$year>="2010-06-30",]
duration.coups.prediction<- spduration::buildDuration(coups.prediction, "succ.coup", unitID="gwcode", tID='year', freq="year")
duration.coups.prediction<- duration.coups.prediction[complete.cases(duration.coups.prediction), ]

#plots
plot(model.coups)
plot(model.coups2)
countryplot(model.coups2)