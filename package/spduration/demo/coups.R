##
##    Demo/test code for "spduration" R package using Powell & Thyne coups
##    Andreas Beger
##    October 2013
##

# all of the stuff below is old and needs to be changes

# 1. install the latest version of spduration on dropbox
# 2. update this demo file so it uses the coups stuff
# 3. try out methods for spdurCRISP (model2 at the bottom), and see what breaks

##########
# What: Demo/test code for spdur dev
# Date: November 2012
# Who:  Andreas Beger
###########

library(spduration)

# Task: create coups model and try out some of the methods
data("coups")

# Demo model to save estimation for examples
dur.coup <- buildDuration(coups, "succ.coup", unitID='gwcode', tID='year',
                          freq="yearly")

## Split duration model of coups
model.coups <- spdur(duration ~ polity2, atrisk ~ polity2, data=dur.coup)

## Build duration version from country-month dataset
# data(insurgency)
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
# 
# ### CRISP wrapper (spdurCRISP)
# # training: buildDuration, subset the coups through 2000
# # test: buildDuration with coups through 2009, then subset it by dropping all before 2001
# # pred: slice the last year of coups 2010
# 
# # Test CRISP/ICEWS wrapper
# model2 <- spdurCrisp(
#   duration ~ low_intensity + high_neighbors + exclpop.l1,
#   atrisk ~ excl_groups_count.l1 + high_neighborhood + high_intensity + 
#     exclpop.l1 + lgdppc.l1,
#   last='end.spell', train=duration.ins, test=duration.ins[1,], 
#   pred=duration.ins[1,], distr="weibull", iter=300)
# 
# # try out methods
# summary(model2)
# # ran ok, output is good/error message
# nobs(model2)
# AIC(model2)
# BIC(model2)