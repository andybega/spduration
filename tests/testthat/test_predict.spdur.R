library(spduration)
data(model.coups)
context("`predict` handling of NA values")

test_that("methods run without error", {
  data(model.coups)
  
  expect_is(predict(model.coups), "numeric")
  expect_is(fitted(model.coups), "numeric")
  expect_is(residuals(model.coups), "numeric")
  expect_equal(predict(model.coups), fitted(model.coups))
})

test_that("predict matches known values", {
  data(model.coups)
  
  expect_equal(
    head(predict(model.coups)),
    c(0.0165520048538834, 0.00572643232295353, 0.00302637135976872, 
      0.00244680644578594, 0.0164928008959435, 0.0367075314817715)
  )
})

# test code
# estimate model and predict
# load('data/coups.rda')
# dur.coup <- buildDuration(coups, "succ.coup", unitID='gwcode', tID='year',
#                           freq="year")
# model.coups <- spdur(duration ~ polity2, atrisk ~ polity2, data=dur.coup)
# pred1 <- predict(model.coups)
# all.equal(c(head(pred1)), c(0.99851638891028, 0.999983390673093, 
#   0.999998875400387, 0.999999541492892, 0.161009654496433, 0.948028467976632))
# # try new data
# test.data <- dur.coup[dur.coup$year>2000, ]
# pred2 <- predict(model.coups, test.data)
# all.equal(c(head(pred2)), c(0.9999924, 0.9999812, 0.9999998, 0.9999924, 
#                             0.9999988, 0.9999995), tolerance=1e-05)
# # try complete cases only
# dur.coup3 <- dur.coup[complete.cases(dur.coup), ]
# model.coups3 <- spdur(duration ~ polity2, atrisk ~ polity2, data=dur.coup3)
# pred3.1 <- predict(model.coups3)
# all.equal(c(head(pred3.1)), c(1.000000e+00, 6.871392e-12, 9.999997e-01, 
#                               1.000000e+00, 1.000000e+00, 5.193213e-03),
#           tolerance=1e-05)
# test.data3 <- dur.coup3[dur.coup3$year>2000, ]
# pred3 <- predict(model.coups3, test.data3)
# all.equal(c(head(pred3)), c(.836345e-03, 3.218981e-12, 3.774758e-15, 
#                             2.153515e-03, 1.000000e+00, 1.000000e+00),
#           tolerance=1e-03)