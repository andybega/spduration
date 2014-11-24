library(spduration)

data(coups)
dur.coup <- add_duration(coups, "succ.coup", unitID='gwcode', tID='year',
                         freq="year")

test_that("spdur weibull converges", {
  model.weib   <- spdur(duration ~ polity2, atrisk ~ polity2, data=dur.coup, 
                        dist="weibull", silent=TRUE)
  expect_is(model.weib, "spdur")
})

test_that("spdur loglog converges", {
  model.loglog <- spdur(duration ~ polity2, atrisk ~ polity2, data=dur.coup, 
                        dist="loglog", silent=TRUE)
  expect_is(model.loglog, "spdur")
})


