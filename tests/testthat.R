library(testthat)
library(spduration)

#test_check("spduration")

data(coups)
dur.coup <- add_duration(coups, "succ.coup", "gwcode", "year", freq="year")

model3 <- spdurCrisp(duration ~ polity2, atrisk ~ polity2, train=dur.coup, test=dur.coup[1,], 
  pred=dur.coup[1,])
plot(model3)
