## Test code for spduration package

library(spduration)

# build data
data(insurgency)
duration.ins <- buildDuration(insurgency, 'insurgency', unitID='ccode', tID='date')

##########
# Loglog model
#
##########

model_loglog <- spdur(
  duration ~ low_intensity + high_neighbors + exclpop.l1,
  atrisk ~ excl_groups_count.l1 + high_neighborhood + high_intensity + exclpop.l1 + lgdppc.l1,
  last='end.spell', data=duration.ins, distr="loglog", max.iter=300)

##########
# Predict function
#
##########

# get model estimates
data(model.ins)

stat_choices <- c('conditional risk', 'conditional cure', 'hazard', 'failure',
                  'unconditional risk', 'unconditional cure', 
                  'conditional hazard', 'conditional failure')

for (stat in stat_choices) {
  cat('Statistic:', stat, '\n')
  print(system.time(predict(model.ins, stat=stat)))
}