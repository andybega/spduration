##########
# What: Demo/test code for spdur dev
# Date: November 2012
# Who:  Andreas Beger
###########

library(spduration)

## Build duration version from country-month dataset
data(insurgency)
duration.ins <- buildDuration(insurgency, 'insurgency', unitID='ccode', tID='date')
duration.ins <- duration.ins[!is.na(duration.ins$failure), ]

## Split duration model of insurgency
model <- spdur(
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

# Test CRISP/ICEWS wrapper
model3 <- spdurCrisp(
  duration ~ low_intensity + high_neighbors + exclpop.l1,
  atrisk ~ excl_groups_count.l1 + high_neighborhood + high_intensity + 
    exclpop.l1 + lgdppc.l1,
  last='end.spell', train=duration.ins, test=duration.ins[1,], 
  pred=duration.ins[1,], distr="weibull", iter=300)