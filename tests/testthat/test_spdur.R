
test_that("spdur estimates at all", {
  data(coups)
  dur.coups <- add_duration(coups, "succ.coup", unitID="gwcode", tID="year",
                            freq="year")
  expect_error(
    spdur(duration ~ polity2, atrisk ~ polity2, data=dur.coups, silent = TRUE),
    NA)
  
})

test_that("loglog distr works", {
  data(coups)
  dur.coups <- add_duration(coups, "succ.coup", unitID="gwcode", tID="year",
                            freq="year")
  expect_error(
    spdur(duration ~ polity2, atrisk ~ polity2, data=dur.coups, silent = TRUE,
          distr = "loglog"),
    NA)
})

test_that("spdur works with tibble input", {
  data(coups)
  dur.coups <- add_duration(coups, "succ.coup", unitID="gwcode", tID="year",
                            freq="year")
  dur.coups <- tibble::as_tibble(dur.coups)
  expect_error(
    spdur(duration ~ polity2, atrisk ~ polity2, data=dur.coups, silent = TRUE),
    NA)
})

test_that("na.action is handled", {
  data(coups)
  dur.coups <- add_duration(coups, "succ.coup", unitID="gwcode", tID="year",
                            freq="year")
  
  # Estimate model
  expect_error(
    spdur(duration ~ polity2, atrisk ~ polity2, data=dur.coups, silent = TRUE,
          na.action = na.fail),
    "missing values in object"
  )

  expect_error(
    spdur(duration ~ polity2, atrisk ~ polity2, data=dur.coups, silent = TRUE,
          na.action = na.pass),
    "na.pass is not supported"
  )
  
  expect_s3_class(spdur(duration ~ polity2, atrisk ~ polity2, data=dur.coups, silent = TRUE,
        na.action = na.omit)$na.action, "omit")
  
  expect_s3_class(spdur(duration ~ polity2, atrisk ~ polity2, data=dur.coups, silent = TRUE,
        na.action = na.exclude)$na.action, "exclude")
  
})
