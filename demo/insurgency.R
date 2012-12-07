##########
# What: Demo/test code for spdur dev
# Date: November 2012
# Who:  Andreas Beger
###########

library(spduration)

## Build duration version from country-month dataset
data(insurgency)
duration.ins <- buildDuration(insurgency, 'insurgency', unitID='ccode', tID='date')

## Split duration model of insurgency
model <- spduration::spdur(
  duration ~ low_intensity + high_neighbors + exclpop.l1,
  atrisk ~ excl_groups_count.l1 + high_neighborhood + high_intensity + exclpop.l1 + lgdppc.l1,
  last='end.spell', data=duration.ins, distr="weibull", max.iter=300)

# Summary and print.summary methods
summary(model)

# Predict method
duration.ins$atrisk.hat <- predict(model)
top5 <- duration.ins[duration.ins$date==as.Date('2012-09-01'), c('country', 'atrisk.hat')]
top5 <- top5[order(top5$atrisk.hat, decreasing=TRUE), ]
head(top5)

# Plot method
library(separationplot)
p <- plot(model)

# Compare to wicews results
library(wicews)
model2 <- wicews::spdur(
  duration ~ low_intensity + high_neighbors + exclpop.l1,
  atrisk ~ excl_groups_count.l1 + high_neighborhood + high_intensity + exclpop.l1 + lgdppc.l1,
  last=duration.ins$end.spell, test=duration.ins[1,], data=duration.ins, distr="weibull", iter=300)

# Test CRISP/ICEWS wrapper
model3 <- spdur.crisp(
  duration ~ low_intensity + high_neighbors + exclpop.l1,
  atrisk ~ excl_groups_count.l1 + high_neighborhood + high_intensity + exclpop.l1 + lgdppc.l1,
  last='end.spell', train=duration.ins, test=duration.ins[1,], pred=duration.ins[1,], distr="weibull", iter=300)