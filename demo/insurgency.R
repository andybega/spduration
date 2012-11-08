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

# Test summary and print.summary methods
summary(model)

# Test predict method
atrisk <- predict(model)

# Test plot method
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
  last='end.spell', test=duration.ins[1,], train=duration.ins, pred=duration.ins[1,], distr="weibull", iter=300)