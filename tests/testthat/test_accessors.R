context("Accessors")

test_that("accessors work", {
  data("model.coups")
  
  expect_error(logLik(model.coups), NA)
  expect_error(nobs(model.coups), NA)
  expect_error(coef(model.coups), NA)
  expect_error(vcov(model.coups), NA)
  expect_error(terms(model.coups), NA)
  expect_error(model.matrix(model.coups), NA)
})