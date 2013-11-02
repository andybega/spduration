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
duration.coup <- buildDuration(coups, "succ.coup", unitID='gwcode', tID='year',
                          freq="yearly")
duration.coup <- duration.coup[!is.na(duration.coup$failure), ]                          

# Split duration model of coups
model.coups <- spdur(duration ~ polity2, atrisk ~ polity2, last='end.spell', data=dur.coup, distr="weibull", max.iter=300)

# summary methods
summary(model.coups)

# predict
duration.coup$atrisk.hat <- predict(model.coups)
top5 <- duration.coup[duration.coup$date==as.Date('2011-09-01'), c('country', 'atrisk.hat')]
top5 <- top5[order(top5$atrisk.hat, decreasing=TRUE), ]
head(top5)

### CRISP wrapper (spdurCRISP)
# # training: buildDuration, subset the coups through 2000
# # test: buildDuration with coups through 2009, then subset it by dropping all before 2001
# # pred: slice the last year of coups 2010
# 
# # Test CRISP/ICEWS wrapper
# model.coups2 <- spdurCrisp(
#   duration ~ polity2, atrisk ~ polity2,
#   last='end.spell', train=duration.ins, test=duration.coup[1,], 
#   pred=duration.coup[1,], distr="weibull", iter=300)
# 
# # try out methods
# summary(model.coups2)

# nobs(model.coup2)
# AIC(model.coup2)
# BIC(model.coup2)












#-----------old--------------

#Build duration version from country-month dataset
#data(insurgency)
# duration.ins <- buildDuration(insurgency, 'insurgency', unitID='ccode', tID='date')
# duration.ins <- duration.ins[!is.na(duration.ins$failure), ]
# 
# ## Split duration model of insurgency
# model <- spdur(
#   duration ~ low_intensity + high_neighbors + exclpop.l1,
#   atrisk ~ excl_groups_count.l1 + high_neighborhood + high_intensity + exclpop.l1 + lgdppc.l1,
#   last='end.spell', data=duration.ins, distr="weibull", max.iter=300)
# 
# # Summary and print.summary methods
# summary(model)
# 
# # Predict method
# duration.ins$atrisk.hat <- predict(model)
# top5 <- duration.ins[duration.ins$date==as.Date('2012-09-01'), c('country', 'atrisk.hat')]
# top5 <- top5[order(top5$atrisk.hat, decreasing=TRUE), ]
# head(top5)
# 
# # Plot method
# library(separationplot)
# p <- plot(model)